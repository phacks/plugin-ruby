require 'set'

module Document
  Concat = Struct.new(:parts)
  Indent = Struct.new(:contents)
  Align = Struct.new(:n, :contents)
  Group = Struct.new(:contents, :break)
  Fill = Struct.new(:parts)
  IfBreak = Struct.new(:break_contents, :flat_contents)
  LineSuffix = Struct.new(:contents)
  LineSuffixBoundary = Object.new
  BreakParent = Object.new
  Trim = Object.new
  Cursor = Object.new
  LineType = Struct.new(:soft, :hard, :literal, keyword_init: true)
  Line = LineType.new(soft: false, hard: false, literal: false)
  SoftLine = LineType.new(soft: true, hard: false, literal: false)
  HardLineType = LineType.new(soft: false, hard: true, literal: false)
  HardLine = Concat.new([HardLineType, BreakParent])
  LiteralLineType = LineType.new(soft: false, hard: true, literal: true)
  LiteralLine = Concat.new([LiteralLineType, BreakParent])

  module Builders
    def concat(parts); Concat.new(parts); end
    def indent(contents); Indent.new(contents); end
    def align(n, contents); Align.new(n, contents); end
    def group(contents); Group.new(contents, false); end
    def dedent_to_root(contents); Align.new(-Float::INFINITY, contents); end
    def mark_as_root(contents); Align.new(:root, contents); end
    def dedent(contents); Align.new(-1, contents); end
    def fill(parts); Fill.new(parts); end
    def if_break(break_contents, flat_contents); IfBreak.new(break_contents, fill_contents); end
    def line_suffix(contents); LineSuffix.new(:contents); end
    def line_suffix_boundary; LineSuffixBoundary; end
    def break_parent; BreakParent; end
    def trim; Trim; end
    def line; Line; end
    def softline; SoftLine; end
    def hardline; HardLine; end
    def literalline; LiteralLine; end
    def cursor; Cursor; end

    def join(sep, arr)
      res = []
      arr.each_with_index do |v, i|
        res << sep if i != 0
        res << v
      end

      concat(res)
    end

    def add_alignment_to_doc(doc, size)
      aligned = doc

      if size > 0
        (0...(size.to_f / 2).floor).times do
          aligned = indent(aligned)
        end

        aligned = align(size % 2, aligned)
        aligned = align(-Float::INFINITY, aligned)
      end

      aligned
    end
  end

  class BreakPropagator
    def propagate(doc)
      stack = [doc]
      marker = Object.new

      group_stack = []
      visited = Set.new
      
      while stack.any?
        doc = stack.pop

        if doc == marker
          if stack.pop.is_a?(Group)
            group = group_stack.pop
            group_stack.last&.break = true if group.break
          end

          next
        end

        stack += [doc, marker]
        enter = true

        case doc
        when BreakParent
          group_stack.last&.break = true
        when Group
          group_stack << doc
          enter = false if visited.include?(doc)
          visited << doc
        end

        if enter
          case doc
          when Array
            doc.reverse_each { |part| stack << part }
          when Concat, Fill
            doc.parts.reverse_each { |part| stack << part }
          when IfBreak
            stack << doc.break_contents if doc.break_contents
            stack << doc.flat_contents if doc.flat_contents
          when Align, Indent, Group, LineSuffix
            stack << doc.contents
          end
        end
      end
    end
  end

  class Printer
    MODE_BREAK = 1;
    MODE_FLAT = 2;

    DEFAULT_OPTIONS = {
      print_width: 80
    }

    attr_reader :options

    def initialize(options = {})
      @options = DEFAULT_OPTIONS.merge(options)
    end

    def print(doc)
      BreakPropagator.new.propagate(doc)

      pos = 0
      cmds = [[IndentLevel.new, MODE_BREAK, doc]]
      out = []

      should_remeasure = false
      line_suffixes = []

      while cmds.any?
        ind, mode, doc = cmds.pop

        case doc
        when String
          out << doc
          pos += doc.length
        when Array
          doc.reverse_each { |part| cmds << [ind, mode, part] }
        when Concat
          doc.parts.reverse_each { |part| cmds << [ind, mode, part] }
        when Cursor
          out << doc
        when Indent
          cmds << [ind.indent, mode, doc.contents]
        when Align
          cmds << [ind.align(doc.n), mode, doc.contents]
        when Trim
          pos -= trim(out)
        when Group
          if mode == MODE_FLAT && !should_remeasure
            cmds << [ind, doc.break ? MODE_BREAK : MODE_FLAT, doc.contents]
          else
            should_remeasure = false
            next_cmd = [ind, MODE_FLAT, doc.contents]

            if !doc.break && fits?(next_cmd, cmds, options[:print_width] - pos)
              cmds << next_cmd
            else
              cmds << [ind, MODE_BREAK, doc.contents]
            end
          end
        when Fill
          next if doc.parts.empty?

          rem = options[:print_width] - pos;
          content, whitespace, = doc.parts

          content_flat_cmd = [ind, MODE_FLAT, content]
          content_break_cmd = [ind, MODE_BREAK, content]
          content_fits = fits?(content_flat_cmd, [], rem, true)

          if doc.parts.length == 1
            cmds << (content_fits ? content_flat_cmd : content_break_cmd)
            next
          end

          whitespace_flat_cmd = [ind, MODE_FLAT, whitespace]
          whitespace_break_cmd = [ind, MODE_BREAK, whitespace]

          if doc.parts.length == 2
            cmds << (content_fits ? whitespace_flat_cmd : whitespace_break_cmd)
            next
          end

          remaining_cmd = [ind, mode, Fill.new(doc.parts[2..-1])]
          second_content = doc.parts[2]

          first_and_second_content_flat_cmd =
            [ind, MODE_FLAT, Concat.new([content, whitespace, second_content])]

          if fits?(first_and_second_content_flat_cmd, [], rem)
            cmds += [remaining_cmd, whitespace_flat_cmd, content_flat_cmd]
          elsif content_fits
            cmds += [remaining_cmd, whitespace_break_cmd, content_flat_cmd]
          else
            cmds += [remaining_cmd, whitespace_break_cmd, content_break_cmd]
          end
        when IfBreak
          if mode == MODE_BREAK
            cmds << [ind, mode, doc.break_contents] if doc.break_contents
          elsif mode == MODE_FLAT
            cmds << [ind, mode, doc.flat_contents] if doc.flat_contents
          end
        when LineSuffix
          line_suffixes << [ind, mode, doc.contents]
        when LineSuffixBoundary
          cmds << [ind, mode, HardLineType]
        when LineType
          if mode == MODE_FLAT
            if !doc.hard
              if !doc.soft
                out << ' '
                pos += 1
              end

              next
            else
              should_remeasure = true
            end
          end

          if line_suffixes.any?
            cmds << [ind, mode, doc]
            cmds += line_suffixes.reverse
            line_suffixes = []
          elsif doc.literal
            if ind.root
              out += ["\n", ind.root.value]
              pos = ind.root.length
            else
              out << "\n"
              pos = 0
            end
          else
            pos -= trim(out)
            out += ["\n", ind.value]
            pos = ind.length
          end
        when BreakParent
          # do nothing
        else
          raise "Unexpected doc type: #{doc}"
        end

        if cmds.empty? && line_suffixes.any?
          cmds += line_suffixes.reverse
          line_suffixes = []
        end
      end

      if out.include?(Cursor)
        before, around, after = out.slice_when { |part| part == Cursor }.map(&:join)

        {
          formatted: "#{before}#{around}#{after}",
          cursor_node_start: before.length,
          cursor_node_text: around
        }
      end

      { formatted: out.join }
    end

    private

    def fits?(next_cmd, rest_cmds, width, must_be_flat = false)
      rest_idx = rest_cmds.length
      cmds = [next_cmd]
      out = []

      while width >= 0
        if cmds.empty?
          return true if rest_idx == 0

          rest_idx -= 1
          cmds << rest_cmds[rest_idx]
          next
        end

        ind, mode, doc = cmds.pop

        case doc
        when String
          out << doc
          width -= doc.length
        when Array
          doc.reverse_each { |part| cmds << [ind, mode, part] }
        when Concat
          doc.parts.reverse_each { |part| cmds << [ind, mode, part] }
        when Indent
          cmds << [ind.indent, mode, doc.contents]
        when Align
          cmds << [ind.align(doc.n), mode, doc.contents]
        when Trim
          width += trim(out)
        when Group
          return false if doc.break && must_be_flat

          cmds << [ind, doc.break ? MODE_BREAK : mode, doc.contents]
        when Fill
          doc.parts.reverse_each { |part| cmds << [ind, mode, part] }
        when IfBreak
          if mode == MODE_BREAK
            cmds << [ind, mode, doc.break_contents] if doc.break_contents
          else
            cmds << [ind, mode, doc.flat_contents] if doc.flat_contents
          end
        when LineType
          if mode == MODE_FLAT
            if !doc.hard
              if !doc.soft
                out << ' '
                width -= 1
              end

              next
            end
          end

          return true
        end
      end

      false
    end

    def trim(out)
      return 0 if out.empty?

      trimmed = 0

      while out.any? && out.last.is_a?(String) && out.last.match?(/\A[\t ]*\z/)
        trimmed += out.pop.length
      end
    
      if out.any? && out.last.is_a?(String)
        length = out.last.length
        out.last.gsub!(/[\t ]*\z/, '')
        trimmed += length - out.last.length
      end
    
      trimmed
    end

    class IndentLevel
      IndentPart = Object.new
      DedentPart = Object.new

      StringAlignPart = Struct.new(:n)
      NumberAlignPart = Struct.new(:n)

      attr_reader :value, :length, :queue, :root

      def initialize(value: '', length: 0, queue: [], root: nil)
        @value = value
        @length = length
        @queue = queue
        @root = root
      end

      def align(n)
        case n
        when NilClass
          self
        when :root
          IndentLevel.new(value: value, length: length, queue: queue, root: self)
        when -Float::INFINITY
          root || IndentLevel.new
        when String
          indent(StringAlignPart.new(n))
        else
          indent(n < 0 ? DedentPart : NumberAlignPart.new(n))
        end
      end

      def indent(part = IndentPart)
        next_value = ''
        next_length = 0
        next_queue = (part == DedentPart ? queue[0...-1] : [*queue, part])

        last_spaces = 0
  
        add_spaces = ->(count) {
          next_value << ' ' * count
          next_length += count
        }
  
        flush_spaces = -> {
          add_spaces[last_spaces] if last_spaces > 0
          last_spaces = 0
        }
  
        next_queue.each do |part|
          case part
          when IndentPart
            flush_spaces.call
            add_spaces.call(2)
          when StringAlignPart
            flush_spaces.call
            next_value += part.n
            next_length += part.n.length
          when NumberAlignPart
            last_spaces += part.n
          end
        end
  
        flush_spaces.call

        IndentLevel.new(
          value: next_value,
          length: next_length,
          queue: next_queue,
          root: root
        )
      end
    end
  end
end

# include Document::Builders

# doc = group(concat([
#   group(concat(["class", " ", "Foo"])),
#   indent(concat([
#     line,
#     "1",
#     break_parent
#   ])),
#   hardline,
#   "end"
# ]))

# puts Document::Printer.new.print(doc)[:formatted]
# exit

require 'json'

def to_doc(json)
  case json
  when Array
    return json.map { |part| to_doc(part) }
  when String
    return json
  end

  case json['type']
  when 'align'
    n = json['n']
    n = :root if n.is_a?(Hash) && n['type'] == 'root'

    Document::Align.new(n, to_doc(json['contents']))
  when 'concat'
    Document::Concat.new(to_doc(json['parts']))
  when 'group'
    Document::Group.new(to_doc(json['contents']), json['break'])
  when 'break-parent'
    Document::BreakParent
  when 'indent'
    Document::Indent.new(to_doc(json['contents']))
  when 'if-break'
    Document::IfBreak.new(to_doc(json['breakContents'] || ''), to_doc(json['flatContents'] || ''))
  when 'line-suffix'
    Document::LineSuffix.new(to_doc(json['contents']))
  when 'trim'
    Document::Trim
  when 'fill'
    Document::Fill.new(to_doc(json['parts']))
  else
    if json['type'] == 'line' && json['literal']
      return Document::LiteralLineType
    elsif json['type'] == 'line' && json['hard']
      return Document::HardLineType
    elsif json['type'] == 'line' && json['soft']
      return Document::SoftLine
    elsif json['type'] == 'line'
      return Document::Line
    end

    pp json
  end
end

source = ARGV.first ? File.read(ARGV.first) : ARGF.read
puts Document::Printer.new(print_width: 80).print(to_doc(JSON.parse(source)))[:formatted]

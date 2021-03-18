require 'json'
require 'set'

module Document
  # These are container document nodes. They contain other nodes, markers,
  # strings, or arrays.
  Concat = Struct.new(:parts)
  Indent = Struct.new(:contents)
  Align = Struct.new(:n, :contents)
  Group = Struct.new(:contents, :break)
  Fill = Struct.new(:parts)
  IfBreak = Struct.new(:break_contents, :flat_contents)
  LineSuffix = Struct.new(:contents)

  # These are marker document nodes. They are used to indicate the bounds of
  # entities or to force some behavior in the printer.
  LineSuffixBoundary = Object.new
  BreakParent = Object.new
  Trim = Object.new
  Cursor = Object.new

  # These are the various types of line document nodes. They are used to
  # indicate to the printer when to put a newline depending on break or flat
  # mode.
  LineType = Struct.new(:soft, :hard, :literal, keyword_init: true)
  Line = LineType.new(soft: false, hard: false, literal: false)
  SoftLine = LineType.new(soft: true, hard: false, literal: false)
  HardLineType = LineType.new(soft: false, hard: true, literal: false)
  HardLine = Concat.new([HardLineType, BreakParent])
  LiteralLineType = LineType.new(soft: false, hard: true, literal: true)
  LiteralLine = Concat.new([LiteralLineType, BreakParent])

  # These are convience methods with shortcuts for creating the document nodes.
  # These methods map to the document builder commands from prettier here:
  # https://github.com/prettier/prettier/blob/main/src/document/doc-builders.js
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

    # Join is not a node in and of itself, it's just a convenient wrapper for a
    # concat node that has an interspersed separator.
    def join(sep, arr)
      res = []
      arr.each_with_index do |v, i|
        res << sep if i != 0
        res << v
      end

      Concat.new(res)
    end

    def add_alignment_to_doc(doc, size)
      aligned = doc

      if size > 0
        (0...(size.to_f / 2).floor).times do
          aligned = indent(aligned)
        end

        aligned = Align.new(size % 2, aligned)
        aligned = Align.new(-Float::INFINITY, aligned)
      end

      aligned
    end
  end

  # This is a generic traverser that will visit each container document node
  # with a specified visitor. The visitor should respond to both #on_enter(doc)
  # and #on_exit(doc).
  class Traverser
    # This object is used to mark a place in the stack for a node that we've
    # already visited. When a marker is encountered, we know it's time to pop
    # the following node off the stack.
    MARKER = Object.new

    attr_reader :visitor

    def initialize(visitor)
      @visitor = visitor
    end

    def traverse(doc)
      stack = [doc]
      
      while stack.any?
        doc = stack.pop

        if doc == MARKER
          visitor.on_exit(stack.pop)
          next
        end

        stack += [doc, MARKER]

        if visitor.on_enter(doc)
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

    def self.traverse(visitor_class, doc)
      new(visitor_class.new).traverse(doc)
    end
  end

  # This is a visitor that can be passed to the traverser that will propagate
  # break-parent nodes all of the way up the tree. When a break-parent is
  # encountered, is will break the surrounding group, and then that group will
  # break its parent, and so on.
  class PropagateBreaksVisitor
    attr_reader :groups, :visited

    def initialize
      @groups = []
      @visited = Set.new
    end

    def on_enter(doc)
      case doc
      when BreakParent
        groups.last&.break = true
      when Group
        groups << doc
        return false if visited.include?(doc)

        visited << doc
      end

      true
    end

    def on_exit(doc)
      groups.last&.break = true if doc.is_a?(Group) && groups.pop.break
    end
  end

  # This is an output buffer. It's effectively an array of strings that is built
  # up we walk through the document node tree. Once all of the parts have been
  # added to the buffer, you can call #to_formatted to get a resulting output
  # object.
  class Buffer
    # This is plain output, returned when the cursor information is not known.
    Output = Struct.new(:formatted)

    # This is enhanced output, returned when the cursor information is known.
    # It includes the offset of the cursor as well as the text that should be
    # selected after printing has been performed.
    OutputWithCursor = Struct.new(:formatted, :cursor_node_start, :cursor_node_text)

    attr_reader :newline, :parts

    def initialize(newline = "\n")
      @newline = newline
      @parts = []
    end

    def <<(part)
      @parts << part
    end

    def trim!
      return 0 if parts.empty?

      trimmed = 0

      while parts.any? && parts.last.is_a?(String) && parts.last.match?(/\A[\t ]*\z/)
        trimmed += parts.pop.length
      end

      if parts.any? && parts.last.is_a?(String)
        length = parts.last.length
        parts.last.gsub!(/[\t ]*\z/, '')
        trimmed += length - parts.last.length
      end

      trimmed
    end

    def line_break
      parts << newline
    end

    def to_formatted
      if parts.include?(Cursor)
        before, around, after =
          parts.slice_when { |part| part == Cursor }.map.with_index do |part, index|
            part.pop if index != 2
            part.join
          end

        OutputWithCursor.new("#{before}#{around}#{after}", before.length, around)
      end

      Output.new(parts.join)
    end
  end

  # This object represents the current level of indentation within the printer.
  # It has the ability to generate new levels of indentation through the #align
  # and #indent methods, which loosely correspond to the builder commands of the
  # same name.
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

    # This can accept a whole lot of different kinds of objects, due to the
    # nature of the flexibility of the Align document node.
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

  # This is the place where we actually walk the document node tree and print
  # out the resulting string.
  class Printer
    # There are two modes in printing, break and flat. When we're in break mode,
    # any lines will use their newline, any if-breaks will use their break
    # contents, etc. When we're in flat mode, we attempt to print everything on
    # one line until we either hit a broken group, a forced line, or the print
    # width.
    MODE_BREAK = 1;
    MODE_FLAT = 2;

    DEFAULT_OPTIONS = { print_width: 80 }.freeze

    attr_reader :options

    def initialize(options = {})
      @options = DEFAULT_OPTIONS.merge(options)
    end

    def print(doc)
      # First, ensure that we've propagated all of the necessary break-parent
      # nodes throughout the tree.
      Traverser.traverse(PropagateBreaksVisitor, doc)

      # This represents how far along the current line we are. It gets reset
      # back to 0 when we encounter a newline.
      position = 0

      # This is our command stack. A command consists of a triplet of an
      # indentation level, the mode (break or flat), and a document node.
      commands = [[IndentLevel.new, MODE_BREAK, doc]]

      # This is the output buffer. It keeps track of all of the string parts
      # that will eventually comprise the overall output.
      buffer = Buffer.new

      # This is a small optimization boolean. It keeps track of whether or not
      # when we hit a group node we should check if it fits on the same line.
      should_remeasure = false

      # This is a separate command stack that includes the same kind of triplets
      # as the commands variable. It is used to keep track of things that should
      # go at the end of printed lines once the other document nodes are
      # accounted for. Typically this is used to implement comments.
      line_suffixes = []

      # This is a linear stack instead of a mutually recursive call defined on
      # the individual document nodes for efficiency.
      while commands.any?
        indent, mode, doc = commands.pop

        case doc
        when String
          buffer << doc
          position += doc.length
        when Array
          doc.reverse_each { |part| commands << [indent, mode, part] }
        when Concat
          doc.parts.reverse_each { |part| commands << [indent, mode, part] }
        when Cursor
          buffer << doc
        when Indent
          commands << [indent.indent, mode, doc.contents]
        when Align
          commands << [indent.align(doc.n), mode, doc.contents]
        when Trim
          position -= buffer.trim!
        when Group
          if mode == MODE_FLAT && !should_remeasure
            commands << [indent, doc.break ? MODE_BREAK : MODE_FLAT, doc.contents]
          else
            should_remeasure = false
            next_cmd = [indent, MODE_FLAT, doc.contents]

            if !doc.break && fits?(next_cmd, commands, options[:print_width] - position)
              commands << next_cmd
            else
              commands << [indent, MODE_BREAK, doc.contents]
            end
          end
        when Fill
          next if doc.parts.empty?

          rem = options[:print_width] - position;
          content, whitespace, = doc.parts

          content_flat_cmd = [indent, MODE_FLAT, content]
          content_break_cmd = [indent, MODE_BREAK, content]
          content_fits = fits?(content_flat_cmd, [], rem, true)

          if doc.parts.length == 1
            commands << (content_fits ? content_flat_cmd : content_break_cmd)
            next
          end

          whitespace_flat_cmd = [indent, MODE_FLAT, whitespace]
          whitespace_break_cmd = [indent, MODE_BREAK, whitespace]

          if doc.parts.length == 2
            commands << (content_fits ? whitespace_flat_cmd : whitespace_break_cmd)
            next
          end

          remaining_cmd = [indent, mode, Fill.new(doc.parts[2..-1])]
          second_content = doc.parts[2]

          first_and_second_content_flat_cmd =
            [indent, MODE_FLAT, Concat.new([content, whitespace, second_content])]

          if fits?(first_and_second_content_flat_cmd, [], rem)
            commands += [remaining_cmd, whitespace_flat_cmd, content_flat_cmd]
          elsif content_fits
            commands += [remaining_cmd, whitespace_break_cmd, content_flat_cmd]
          else
            commands += [remaining_cmd, whitespace_break_cmd, content_break_cmd]
          end
        when IfBreak
          if mode == MODE_BREAK
            commands << [indent, mode, doc.break_contents] if doc.break_contents
          elsif mode == MODE_FLAT
            commands << [indent, mode, doc.flat_contents] if doc.flat_contents
          end
        when LineSuffix
          line_suffixes << [indent, mode, doc.contents]
        when LineSuffixBoundary
          commands << [indent, mode, HardLineType]
        when LineType
          if mode == MODE_FLAT
            if !doc.hard
              if !doc.soft
                buffer << ' '
                position += 1
              end

              next
            else
              should_remeasure = true
            end
          end

          if line_suffixes.any?
            commands << [indent, mode, doc]
            commands += line_suffixes.reverse
            line_suffixes = []
          elsif doc.literal
            buffer.line_break

            if indent.root
              buffer << indent.root.value
              position = indent.root.length
            else
              position = 0
            end
          else
            position -= buffer.trim!
            buffer.line_break
            buffer << indent.value
            position = indent.length
          end
        when BreakParent
          # do nothing
        else
          raise "Unexpected doc type: #{doc}"
        end

        if commands.empty? && line_suffixes.any?
          commands += line_suffixes.reverse
          line_suffixes = []
        end
      end

      buffer.to_formatted
    end

    def self.format(doc, options = {})
      new(options).print(doc).formatted
    end

    private

    # This method returns a boolean as to whether or not the remaining commands
    # fit onto the remaining space on the current line. If we finish printing
    # all of the commands or if we hit a newline, then we return true. Otherwise
    # if we continue printing past the remaining space, we return false.
    def fits?(next_command, rest_commands, remaining, must_be_flat = false)
      # This is the index in the remaining commands that we've handled so far.
      # We reverse through the commands and add them to the stack if we've run
      # out of nodes to handle.
      rest_index = rest_commands.length

      # This is our stack of commands, very similar to the commands list in the
      # print method.
      commands = [next_command]

      # This is our output buffer, really only necessary to keep track of
      # because we could encounter a Trim document node that would actually add
      # remaining space.
      buffer = Buffer.new

      while remaining >= 0
        if commands.empty?
          return true if rest_index == 0

          rest_index -= 1
          commands << rest_commands[rest_index]
          next
        end

        ind, mode, doc = commands.pop

        case doc
        when String
          buffer << doc
          remaining -= doc.length
        when Array
          doc.reverse_each { |part| commands << [ind, mode, part] }
        when Concat
          doc.parts.reverse_each { |part| commands << [ind, mode, part] }
        when Indent
          commands << [ind.indent, mode, doc.contents]
        when Align
          commands << [ind.align(doc.n), mode, doc.contents]
        when Trim
          remaining += buffer.trim!
        when Group
          return false if doc.break && must_be_flat

          commands << [ind, doc.break ? MODE_BREAK : mode, doc.contents]
        when Fill
          doc.parts.reverse_each { |part| commands << [ind, mode, part] }
        when IfBreak
          if mode == MODE_BREAK
            commands << [ind, mode, doc.break_contents] if doc.break_contents
          else
            commands << [ind, mode, doc.flat_contents] if doc.flat_contents
          end
        when LineType
          if mode == MODE_FLAT
            if !doc.hard
              if !doc.soft
                buffer << ' '
                remaining -= 1
              end

              next
            end
          end

          return true
        end
      end

      false
    end
  end

  # This is a serializer that will read prettier's doc node JSON and convert it
  # into the ruby structures that we expect.
  module Serializer
    def self.load(json)
      case json
      when Array
        return json.map { |part| load(part) }
      when String
        return json
      end

      case json['type']
      when 'align'
        n = json['n']
        n = :root if n.is_a?(Hash) && n['type'] == 'root'
    
        Document::Align.new(n, load(json['contents']))
      when 'concat'
        Document::Concat.new(load(json['parts']))
      when 'group'
        Document::Group.new(load(json['contents']), json['break'])
      when 'break-parent'
        Document::BreakParent
      when 'indent'
        Document::Indent.new(load(json['contents']))
      when 'if-break'
        Document::IfBreak.new(
          load(json['breakContents'] || ''),
          load(json['flatContents'] || '')
        )
      when 'line-suffix'
        Document::LineSuffix.new(load(json['contents']))
      when 'trim'
        Document::Trim
      when 'fill'
        Document::Fill.new(load(json['parts']))
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
    
        raise json.inspect
      end
    end
  end
end

# An example AST node using the builder convenience methods to create a document
# node tree that can then be printed later.
class ExampleNode
  include Document::Builders

  def to_doc
    group(concat([
      group(concat(["class", " ", "Foo"])),
      indent(concat([
        line,
        "1",
        break_parent
      ])),
      hardline,
      "end"
    ]))
  end
end

# This script can accept the path to a serialized document JSON file, a
# serialized document JSON on stdin, or nothing in which case it will print the
# example node.
doc =
  if ARGV.first
    Document::Serializer.load(JSON.parse(File.read(ARGV.first)))
  elsif !STDIN.tty?
    Document::Serializer.load(JSON.parse(ARGF.read))
  else
    ExampleNode.new.to_doc
  end

puts Document::Printer.format(doc)

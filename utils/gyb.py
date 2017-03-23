#!/usr/bin/env python
# GYB: Generate Your Boilerplate (improved names welcome; at least
# this one's short).  See -h output for instructions

from __future__ import print_function

import os
import re
try:
    from cStringIO import StringIO
except ImportError:
    from io import StringIO
import textwrap
import tokenize

from bisect import bisect

try:
    basestring
except NameError:
    basestring = str


def get_line_starts(s):
    """Return a list containing the start index of each line in s.

    The list also contains a sentinel index for the end of the string,
    so there will be one more element in the list than there are lines
    in the string
    """
    starts = [0]

    for line in s.split('\n'):
        starts.append(starts[-1] + len(line) + 1)

    starts[-1] -= 1
    return starts


def strip_trailing_nl(s):
    """If s ends with a newline, drop it; else return s intact"""
    return s[:-1] if s.endswith('\n') else s


def split_lines(s):
    """Split s into a list of lines, each of which has a trailing newline

    If the lines are later concatenated, the result is s, possibly
    with a single appended newline.
    """
    return [l + '\n' for l in s.split('\n')]


# text on a line up to the first '$$', '${', or '%%'
literalText = r'(?: [^$\n%] | \$(?![${]) | %(?!%) )*'

# The part of an '%end' line that follows the '%' sign
linesClose = r'[\ \t]* end [\ \t]* (?: \# .* )? $'

# Note: Where "# Absorb" appears below, the regexp attempts to eat up
# through the end of ${...} and %{...}% constructs.  In reality we
# handle this with the Python tokenizer, which avoids mis-detections
# due to nesting, comments and strings.  This extra absorption in the
# regexp facilitates testing the regexp on its own, by preventing the
# interior of some of these constructs from being treated as literal
# text.
tokenize_re = re.compile(
    r'''
# %-lines and %{...}-blocks
    # \n? # absorb one preceding newline
    ^
    (?:
      (?P<gybLines>
        (?P<_indent> [\ \t]* % (?! [{%] ) [\ \t]* ) (?! [\ \t] | ''' +
    linesClose + r''' ) .*
        ( \n (?P=_indent) (?! ''' + linesClose + r''' ) .* ) *
      )
      | (?P<gybLinesClose> [\ \t]* % [ \t]* ''' + linesClose + r''' )
      | [\ \t]* (?P<gybBlockOpen> %\{  )
        (?: [^}]| \} (?!%) )* \}%  # Absorb
    )
    \n? # absorb one trailing newline

# Substitutions
| (?P<substitutionOpen> \$\{ )
  [^}]* \} # Absorb

# %% and $$ are literal % and $ respectively
| (?P<symbol>[$%]) (?P=symbol)

# Literal text
| (?P<literal> ''' + literalText + r'''
    (?:
      # newline that doesn't precede space+%
      (?: \n (?! [\ \t]* %[^%] ) )
      ''' + literalText + r'''
    )*
    \n?
  )
''', re.VERBOSE | re.MULTILINE)

gyb_block_close = re.compile('\}%[ \t]*\n?')


def token_pos_to_index(token_pos, start, line_starts):
    """Translate a tokenize (line, column) pair into an absolute
    position in source text given the position where we started
    tokenizing and a list that maps lines onto their starting
    character indexes.
    """
    relative_token_line_plus1, token_col = token_pos

    # line number where we started tokenizing
    start_line_num = bisect(line_starts, start) - 1

    # line number of the token in the whole text
    abs_token_line = relative_token_line_plus1 - 1 + start_line_num

    # if found in the first line, adjust the end column to account
    # for the extra text
    if relative_token_line_plus1 == 1:
        token_col += start - line_starts[start_line_num]

    # Sometimes tokenizer errors report a line beyond the last one
    if abs_token_line >= len(line_starts):
        return line_starts[-1]

    return line_starts[abs_token_line] + token_col


def tokenize_python_to_unmatched_close_curly(source_text, start, line_starts):
    """Apply Python's tokenize to source_text starting at index start
    while matching open and close curly braces.  When an unmatched
    close curly brace is found, return its index.  If not found,
    return len(source_text).  If there's a tokenization error, return
    the position of the error.
    """
    stream = StringIO(source_text)
    stream.seek(start)
    nesting = 0

    try:
        for kind, text, token_start, token_end, line_text \
                in tokenize.generate_tokens(stream.readline):

            if text == '{':
                nesting += 1
            elif text == '}':
                nesting -= 1
                if nesting < 0:
                    return token_pos_to_index(token_start, start, line_starts)

    except tokenize.TokenError as error:
        (message, error_pos) = error.args
        return token_pos_to_index(error_pos, start, line_starts)

    return len(source_text)


def tokenize_template(template_text):
    r"""Given the text of a template, returns an iterator over
    (tokenType, token, match) tuples.

    **Note**: this is template syntax tokenization, not Python
    tokenization.

    When a non-literal token is matched, a client may call
    iter.send(pos) on the iterator to reset the position in
    template_text at which scanning will resume.

    This function provides a base level of tokenization which is
    then refined by ParseContext.token_generator.

    >>> from pprint import *
    >>> pprint(list((kind, text) for kind, text, _ in tokenize_template(
    ...   '%for x in range(10):\n%  print x\n%end\njuicebox')))
    [('gybLines', '%for x in range(10):\n%  print x'),
     ('gybLinesClose', '%end'),
     ('literal', 'juicebox')]

    >>> pprint(list((kind, text) for kind, text, _ in tokenize_template(
    ... '''Nothing
    ... % if x:
    ... %    for i in range(3):
    ... ${i}
    ... %    end
    ... % else:
    ... THIS SHOULD NOT APPEAR IN THE OUTPUT
    ... ''')))
    [('literal', 'Nothing\n'),
     ('gybLines', '% if x:\n%    for i in range(3):'),
     ('substitutionOpen', '${'),
     ('literal', '\n'),
     ('gybLinesClose', '%    end'),
     ('gybLines', '% else:'),
     ('literal', 'THIS SHOULD NOT APPEAR IN THE OUTPUT\n')]

    >>> for kind, text, _ in tokenize_template('''
    ... This is $some$ literal stuff containing a ${substitution}
    ... followed by a %{...} block:
    ...   %{
    ...   # Python code
    ...   }%
    ... and here $${are} some %-lines:
    ...   % x = 1
    ...   % y = 2
    ...   % if z == 3:
    ...   %    print '${hello}'
    ...   % end
    ...   %    for x in zz:
    ...   %        print x
    ...     % # different indentation
    ... % twice
    ... and some lines that literally start with a %% token
    ... %% first line
    ...   %% second line
    ... '''):
    ...     print((kind, text.strip().split('\n',1)[0]))
    ('literal', 'This is $some$ literal stuff containing a')
    ('substitutionOpen', '${')
    ('literal', 'followed by a %{...} block:')
    ('gybBlockOpen', '%{')
    ('literal', 'and here ${are} some %-lines:')
    ('gybLines', '% x = 1')
    ('gybLinesClose', '% end')
    ('gybLines', '%    for x in zz:')
    ('gybLines', '% # different indentation')
    ('gybLines', '% twice')
    ('literal', 'and some lines that literally start with a % token')
    """
    pos = 0
    end = len(template_text)

    saved_literal = []
    literal_first_match = None

    while pos < end:
        m = tokenize_re.match(template_text, pos, end)

        # pull out the one matched key (ignoring internal patterns starting
        # with _)
        ((kind, text), ) = (
            (kind, text) for (kind, text) in m.groupdict().items()
            if text is not None and kind[0] != '_')

        if kind in ('literal', 'symbol'):
            if len(saved_literal) == 0:
                literal_first_match = m
            # literals and symbols get batched together
            saved_literal.append(text)
            pos = None
        else:
            # found a non-literal.  First yield any literal we've accumulated
            if saved_literal != []:
                yield 'literal', ''.join(saved_literal), literal_first_match
                saved_literal = []

            # Then yield the thing we found.  If we get a reply, it's
            # the place to resume tokenizing
            pos = yield kind, text, m

        # If we were not sent a new position by our client, resume
        # tokenizing at the end of this match.
        if pos is None:
            pos = m.end(0)
        else:
            # Client is not yet ready to process next token
            yield

    if saved_literal != []:
        yield 'literal', ''.join(saved_literal), literal_first_match


def split_gyb_lines(source_lines):
    r"""Return a list of lines at which to split the incoming source

    These positions represent the beginnings of python line groups that
    will require a matching %end construct if they are to be closed.

    >>> src = split_lines('''\
    ... if x:
    ...     print x
    ... if y: # trailing comment
    ...     print z
    ...     if z: # another comment\
    ... ''')
    >>> s = split_gyb_lines(src)
    >>> len(s)
    2
    >>> src[s[0]]
    '    print z\n'
    >>> s[1] - len(src)
    0

    >>> src = split_lines('''\
    ... if x:
    ...     if y: print 1
    ...     if z:
    ...         print 2
    ...     pass\
    ... ''')
    >>> s = split_gyb_lines(src)
    >>> len(s)
    1
    >>> src[s[0]]
    '    if y: print 1\n'

    >>> src = split_lines('''\
    ... if x:
    ...     if y:
    ...         print 1
    ...         print 2
    ... ''')
    >>> s = split_gyb_lines(src)
    >>> len(s)
    2
    >>> src[s[0]]
    '    if y:\n'
    >>> src[s[1]]
    '        print 1\n'
    """
    last_token_text, last_token_kind = None, None
    unmatched_indents = []

    dedents = 0
    try:
        for token_kind, token_text, token_start, \
            (token_end_line, token_end_col), line_text \
            in tokenize.generate_tokens(lambda i=iter(source_lines):
                                        next(i)):

            if token_kind in (tokenize.COMMENT, tokenize.ENDMARKER):
                continue

            if token_text == '\n' and last_token_text == ':':
                unmatched_indents.append(token_end_line)

            # The tokenizer appends dedents at EOF; don't consider
            # those as matching indentations.  Instead just save them
            # up...
            if last_token_kind == tokenize.DEDENT:
                dedents += 1
            # And count them later, when we see something real.
            if token_kind != tokenize.DEDENT and dedents > 0:
                unmatched_indents = unmatched_indents[:-dedents]
                dedents = 0

            last_token_text, last_token_kind = token_text, token_kind

    except tokenize.TokenError:
        # Let the later compile() call report the error
        return []

    if last_token_text == ':':
        unmatched_indents.append(len(source_lines))

    return unmatched_indents


def code_starts_with_dedent_keyword(source_lines):
    r"""Return True iff the incoming Python source_lines begin with "else",
    "elif", "except", or "finally".

    Initial comments and whitespace are ignored.

    >>> code_starts_with_dedent_keyword(split_lines('if x in y: pass'))
    False
    >>> code_starts_with_dedent_keyword(split_lines('except ifSomethingElse:'))
    True
    >>> code_starts_with_dedent_keyword(
    ...     split_lines('\n# comment\nelse: # yes'))
    True
    """
    token_text = None
    for token_kind, token_text, _, _, _ \
            in tokenize.generate_tokens(lambda i=iter(source_lines): next(i)):

        if token_kind != tokenize.COMMENT and token_text.strip() != '':
            break

    return token_text in ('else', 'elif', 'except', 'finally')


class ParseContext(object):

    """State carried through a parse of a template"""

    filename = ''
    template = ''
    line_starts = []
    code_start_line = -1
    code_text = None
    tokens = None       # The rest of the tokens
    close_lines = False

    def __init__(self, filename, template=None):
        self.filename = os.path.abspath(filename)
        if template is None:
            with open(filename) as f:
                self.template = f.read()
        else:
            self.template = template
        self.line_starts = get_line_starts(self.template)
        self.tokens = self.token_generator(tokenize_template(self.template))
        self.next_token()

    def pos_to_line(self, pos):
        return bisect(self.line_starts, pos) - 1

    def token_generator(self, base_tokens):
        r"""Given an iterator over (kind, text, match) triples (see
        tokenize_template above), return a refined iterator over
        token_kinds.

        Among other adjustments to the elements found by base_tokens,
        this refined iterator tokenizes python code embedded in
        template text to help determine its true extent.  The
        expression "base_tokens.send(pos)" is used to reset the index at
        which base_tokens resumes scanning the underlying text.

        >>> ctx = ParseContext('dummy', '''
        ... %for x in y:
        ... %    print x
        ... % end
        ... literally
        ... ''')
        >>> while ctx.token_kind:
        ...     print((ctx.token_kind, ctx.code_text or ctx.token_text))
        ...     ignored = ctx.next_token()
        ('literal', '\n')
        ('gybLinesOpen', 'for x in y:\n')
        ('gybLines', '    print x\n')
        ('gybLinesClose', '% end')
        ('literal', 'literally\n')

        >>> ctx = ParseContext('dummy',
        ... '''Nothing
        ... % if x:
        ... %    for i in range(3):
        ... ${i}
        ... %    end
        ... % else:
        ... THIS SHOULD NOT APPEAR IN THE OUTPUT
        ... ''')
        >>> while ctx.token_kind:
        ...     print((ctx.token_kind, ctx.code_text or ctx.token_text))
        ...     ignored = ctx.next_token()
        ('literal', 'Nothing\n')
        ('gybLinesOpen', 'if x:\n')
        ('gybLinesOpen', '   for i in range(3):\n')
        ('substitutionOpen', 'i')
        ('literal', '\n')
        ('gybLinesClose', '%    end')
        ('gybLinesOpen', 'else:\n')
        ('literal', 'THIS SHOULD NOT APPEAR IN THE OUTPUT\n')

        >>> ctx = ParseContext('dummy',
        ... '''% for x in [1, 2, 3]:
        ... %   if x == 1:
        ... literal1
        ... %   elif x > 1:  # add output line here to fix bug
        ... %     if x == 2:
        ... literal2
        ... %     end
        ... %   end
        ... % end
        ... ''')
        >>> while ctx.token_kind:
        ...     print((ctx.token_kind, ctx.code_text or ctx.token_text))
        ...     ignored = ctx.next_token()
        ('gybLinesOpen', 'for x in [1, 2, 3]:\n')
        ('gybLinesOpen', '  if x == 1:\n')
        ('literal', 'literal1\n')
        ('gybLinesOpen', 'elif x > 1:  # add output line here to fix bug\n')
        ('gybLinesOpen', '  if x == 2:\n')
        ('literal', 'literal2\n')
        ('gybLinesClose', '%     end')
        ('gybLinesClose', '%   end')
        ('gybLinesClose', '% end')
        """
        for self.token_kind, self.token_text, self.token_match in base_tokens:
            kind = self.token_kind
            self.code_text = None

            # Do we need to close the current lines?
            self.close_lines = kind == 'gybLinesClose'

            # %{...}% and ${...} constructs
            if kind.endswith('Open'):

                # Tokenize text that follows as Python up to an unmatched '}'
                code_start = self.token_match.end(kind)
                self.code_start_line = self.pos_to_line(code_start)

                close_pos = tokenize_python_to_unmatched_close_curly(
                    self.template, code_start, self.line_starts)
                self.code_text = self.template[code_start:close_pos]
                yield kind

                if (kind == 'gybBlockOpen'):
                    # Absorb any '}% <optional-comment> \n'
                    m2 = gyb_block_close.match(self.template, close_pos)
                    if not m2:
                        raise ValueError("Invalid block closure")
                    next_pos = m2.end(0)
                else:
                    assert kind == 'substitutionOpen'
                    # skip past the closing '}'
                    next_pos = close_pos + 1

                # Resume tokenizing after the end of the code.
                base_tokens.send(next_pos)

            elif kind == 'gybLines':

                self.code_start_line = self.pos_to_line(
                    self.token_match.start('gybLines'))
                indentation = self.token_match.group('_indent')

                # Strip off the leading indentation and %-sign
                source_lines = re.split(
                    '^' + re.escape(indentation),
                    self.token_match.group('gybLines') + '\n',
                    flags=re.MULTILINE)[1:]

                if code_starts_with_dedent_keyword(source_lines):
                    self.close_lines = True

                last_split = 0
                for line in split_gyb_lines(source_lines):
                    self.token_kind = 'gybLinesOpen'
                    self.code_text = ''.join(source_lines[last_split:line])
                    yield self.token_kind
                    last_split = line
                    self.code_start_line += line - last_split
                    self.close_lines = False

                self.code_text = ''.join(source_lines[last_split:])
                if self.code_text:
                    self.token_kind = 'gybLines'
                    yield self.token_kind
            else:
                yield self.token_kind

    def next_token(self):
        """Move to the next token"""
        for kind in self.tokens:
            return self.token_kind

        self.token_kind = None


_default_line_directive = '// ###sourceLocation'


class ExecutionContext(object):

    """State we pass around during execution of a template"""

    def __init__(self, line_directive=_default_line_directive,
                 **local_bindings):
        self.local_bindings = local_bindings
        self.line_directive = line_directive
        self.local_bindings['__context__'] = self
        self.result_text = []
        self.last_file_line = None

    def append_text(self, text, file, line):
        # see if we need to inject a line marker
        if self.line_directive:
            if (file, line) != self.last_file_line:
                # We can only insert the line directive at a line break
                if len(self.result_text) == 0 \
                   or self.result_text[-1].endswith('\n'):
                    self.result_text.append('%s(file: "%s", line: %d)\n' % (
                        self.line_directive, file, line + 1))
                # But if the new text contains any line breaks, we can create
                # one
                elif '\n' in text:
                    i = text.find('\n')
                    self.result_text.append(text[:i + 1])
                    self.last_file_line = (
                        self.last_file_line[0], self.last_file_line[1] + 1)
                    # and try again
                    self.append_text(text[i + 1:], file, line)
                    return

        self.result_text.append(text)
        self.last_file_line = (file, line + text.count('\n'))


class ASTNode(object):

    """Abstract base class for template AST nodes"""

    def __init__(self):
        raise NotImplementedError("ASTNode.__init__ is not implemented.")

    def execute(self, context):
        raise NotImplementedError("ASTNode.execute is not implemented.")

    def __str__(self, indent=''):
        raise NotImplementedError("ASTNode.__str__ is not implemented.")

    def format_children(self, indent):
        if not self.children:
            return ' []'

        return '\n'.join(
            ['', indent + '['] +
            [x.__str__(indent + 4 * ' ') for x in self.children] +
            [indent + ']'])


class Block(ASTNode):

    """A sequence of other AST nodes, to be executed in order"""

    children = []

    def __init__(self, context):
        self.children = []

        while context.token_kind and not context.close_lines:
            if context.token_kind == 'literal':
                node = Literal
            else:
                node = Code
            self.children.append(node(context))

    def execute(self, context):
        for x in self.children:
            x.execute(context)

    def __str__(self, indent=''):
        return indent + 'Block:' + self.format_children(indent)


class Literal(ASTNode):

    """An AST node that generates literal text"""

    def __init__(self, context):
        self.text = context.token_text
        start_position = context.token_match.start(context.token_kind)
        self.start_line_number = context.pos_to_line(start_position)
        self.filename = context.filename
        context.next_token()

    def execute(self, context):
        context.append_text(self.text, self.filename, self.start_line_number)

    def __str__(self, indent=''):
        return '\n'.join(
            [indent + x for x in ['Literal:'] +
             strip_trailing_nl(self.text).split('\n')])


class Code(ASTNode):

    """An AST node that is evaluated as Python"""

    code = None
    children = ()
    kind = None

    def __init__(self, context):

        source = ''
        source_line_count = 0

        def accumulate_code():
            s = source + (context.code_start_line - source_line_count) * '\n' \
                + textwrap.dedent(context.code_text)
            line_count = context.code_start_line + \
                context.code_text.count('\n')
            context.next_token()
            return s, line_count

        eval_exec = 'exec'
        if context.token_kind.startswith('substitution'):
            eval_exec = 'eval'
            source, source_line_count = accumulate_code()
            source = '(' + source.strip() + ')'

        else:
            while context.token_kind == 'gybLinesOpen':
                source, source_line_count = accumulate_code()
                source += '    __children__[%d].execute(__context__)\n' % len(
                    self.children)
                source_line_count += 1

                self.children += (Block(context),)

            if context.token_kind == 'gybLinesClose':
                context.next_token()

        if context.token_kind == 'gybLines':
            source, source_line_count = accumulate_code()

            # Only handle a substitution as part of this code block if
            # we don't already have some %-lines.
        elif context.token_kind == 'gybBlockOpen':

            # Opening ${...} and %{...}% constructs
            source, source_line_count = accumulate_code()

        self.filename = context.filename
        self.start_line_number = context.code_start_line
        self.code = compile(source, context.filename, eval_exec)
        self.source = source

    def execute(self, context):
        # Save __children__ from the local bindings
        save_children = context.local_bindings.get('__children__')
        # Execute the code with our __children__ in scope
        context.local_bindings['__children__'] = self.children
        context.local_bindings['__file__'] = self.filename
        result = eval(self.code, context.local_bindings)

        if context.local_bindings['__children__'] is not self.children:
            raise ValueError("The code is not allowed to mutate __children__")
        # Restore the bindings
        context.local_bindings['__children__'] = save_children

        # If we got a result, the code was an expression, so append
        # its value
        if result is not None \
                or (isinstance(result, basestring) and result != ''):
            from numbers import Number, Integral
            result_string = None
            if isinstance(result, Number) and not isinstance(result, Integral):
                result_string = repr(result)
            else:
                result_string = str(result)
            context.append_text(
                result_string, self.filename, self.start_line_number)

    def __str__(self, indent=''):
        source_lines = re.sub(r'^\n', '', strip_trailing_nl(
            self.source), flags=re.MULTILINE).split('\n')
        if len(source_lines) == 1:
            s = indent + 'Code: {' + source_lines[0] + '}'
        else:
            s = indent + 'Code:\n' + indent + '{\n' + '\n'.join(
                indent + 4 * ' ' + l for l in source_lines
            ) + '\n' + indent + '}'
        return s + self.format_children(indent)


def expand(filename, line_directive=_default_line_directive, **local_bindings):
    r"""Return the contents of the givepn template file, executed with the given
    local bindings.

    >>> from tempfile import NamedTemporaryFile
    >>> f = NamedTemporaryFile()
    >>> f.write(
    ... '''---
    ... % for i in range(int(x)):
    ... a pox on ${i} for epoxy
    ... % end
    ... ''')
    >>> f.flush()
    >>> result = expand(
    ...   f.name, line_directive='//#sourceLocation', x=2
    ... ).replace(
    ...   '"%s"' % f.name, '"dummy.file"')
    >>> print(result, end='')
    //#sourceLocation(file: "dummy.file", line: 1)
    ---
    //#sourceLocation(file: "dummy.file", line: 3)
    a pox on 0 for epoxy
    //#sourceLocation(file: "dummy.file", line: 3)
    a pox on 1 for epoxy

    """
    with open(filename) as f:
        t = parse_template(filename, f.read())
        return execute_template(
            t, line_directive=line_directive, **local_bindings)


def parse_template(filename, text=None):
    r"""Return an AST corresponding to the given template file.

    If text is supplied, it is assumed to be the contents of the file,
    as a string.

    >>> print(parse_template('dummy.file', text=
    ... '''% for x in [1, 2, 3]:
    ... %   if x == 1:
    ... literal1
    ... %   elif x > 1:  # add output line after this line to fix bug
    ... %     if x == 2:
    ... literal2
    ... %     end
    ... %   end
    ... % end
    ... '''))
    Block:
    [
        Code:
        {
            for x in [1, 2, 3]:
                __children__[0].execute(__context__)
        }
        [
            Block:
            [
                Code:
                {
                    if x == 1:
                        __children__[0].execute(__context__)
                    elif x > 1:  # add output line after this line to fix bug
                        __children__[1].execute(__context__)
                }
                [
                    Block:
                    [
                        Literal:
                        literal1
                    ]
                    Block:
                    [
                        Code:
                        {
                            if x == 2:
                                __children__[0].execute(__context__)
                        }
                        [
                            Block:
                            [
                                Literal:
                                literal2
                            ]
                        ]
                    ]
                ]
            ]
        ]
    ]

    >>> print(parse_template(
    ...     'dummy.file',
    ...     text='%for x in range(10):\n%  print(x)\n%end\njuicebox'))
    Block:
    [
        Code:
        {
            for x in range(10):
                __children__[0].execute(__context__)
        }
        [
            Block:
            [
                Code: {print(x)} []
            ]
        ]
        Literal:
        juicebox
    ]

    >>> print(parse_template('/dummy.file', text=
    ... '''Nothing
    ... % if x:
    ... %    for i in range(3):
    ... ${i}
    ... %    end
    ... % else:
    ... THIS SHOULD NOT APPEAR IN THE OUTPUT
    ... '''))
    Block:
    [
        Literal:
        Nothing
        Code:
        {
            if x:
                __children__[0].execute(__context__)
            else:
                __children__[1].execute(__context__)
        }
        [
            Block:
            [
                Code:
                {
                    for i in range(3):
                        __children__[0].execute(__context__)
                }
                [
                    Block:
                    [
                        Code: {(i)} []
                        Literal:
    <BLANKLINE>
                    ]
                ]
            ]
            Block:
            [
                Literal:
                THIS SHOULD NOT APPEAR IN THE OUTPUT
            ]
        ]
    ]

    >>> print(parse_template('dummy.file', text='''%
    ... %for x in y:
    ... %    print(y)
    ... '''))
    Block:
    [
        Code:
        {
            for x in y:
                __children__[0].execute(__context__)
        }
        [
            Block:
            [
                Code: {print(y)} []
            ]
        ]
    ]

    >>> print(parse_template('dummy.file', text='''%
    ... %if x:
    ... %    print(y)
    ... AAAA
    ... %else:
    ... BBBB
    ... '''))
    Block:
    [
        Code:
        {
            if x:
                __children__[0].execute(__context__)
            else:
                __children__[1].execute(__context__)
        }
        [
            Block:
            [
                Code: {print(y)} []
                Literal:
                AAAA
            ]
            Block:
            [
                Literal:
                BBBB
            ]
        ]
    ]

    >>> print(parse_template('dummy.file', text='''%
    ... %if x:
    ... %    print(y)
    ... AAAA
    ... %# This is a comment
    ... %else:
    ... BBBB
    ... '''))
    Block:
    [
        Code:
        {
            if x:
                __children__[0].execute(__context__)
            # This is a comment
            else:
                __children__[1].execute(__context__)
        }
        [
            Block:
            [
                Code: {print(y)} []
                Literal:
                AAAA
            ]
            Block:
            [
                Literal:
                BBBB
            ]
        ]
    ]

    >>> print(parse_template('dummy.file', text='''\
    ... %for x in y:
    ... AAAA
    ... %if x:
    ... BBBB
    ... %end
    ... CCCC
    ... '''))
    Block:
    [
        Code:
        {
            for x in y:
                __children__[0].execute(__context__)
        }
        [
            Block:
            [
                Literal:
                AAAA
                Code:
                {
                    if x:
                        __children__[0].execute(__context__)
                }
                [
                    Block:
                    [
                        Literal:
                        BBBB
                    ]
                ]
                Literal:
                CCCC
            ]
        ]
    ]
    """
    return Block(ParseContext(filename, text))


def execute_template(
        ast, line_directive=_default_line_directive, **local_bindings):
    r"""Return the text generated by executing the given template AST.

    Keyword arguments become local variable bindings in the execution context

    >>> ast = parse_template('/dummy.file', text=
    ... '''Nothing
    ... % if x:
    ... %    for i in range(3):
    ... ${i}
    ... %    end
    ... % else:
    ... THIS SHOULD NOT APPEAR IN THE OUTPUT
    ... ''')
    >>> out = execute_template(ast, line_directive='//#sourceLocation', x=1)
    >>> out = out.replace(os.path.abspath(os.sep) + 'dummy.file', "DUMMY-FILE")
    >>> print(out, end="")
    //#sourceLocation(file: "DUMMY-FILE", line: 1)
    Nothing
    //#sourceLocation(file: "DUMMY-FILE", line: 4)
    0
    //#sourceLocation(file: "DUMMY-FILE", line: 4)
    1
    //#sourceLocation(file: "DUMMY-FILE", line: 4)
    2

    >>> ast = parse_template('/dummy.file', text=
    ... '''Nothing
    ... % a = []
    ... % for x in range(3):
    ... %    a.append(x)
    ... % end
    ... ${a}
    ... ''')
    >>> out = execute_template(ast, line_directive='//#sourceLocation', x=1)
    >>> out = out.replace(os.path.abspath(os.sep) + 'dummy.file', "DUMMY-FILE")
    >>> print(out, end="")
    //#sourceLocation(file: "DUMMY-FILE", line: 1)
    Nothing
    //#sourceLocation(file: "DUMMY-FILE", line: 6)
    [0, 1, 2]
    """
    execution_context = ExecutionContext(
        line_directive=line_directive, **local_bindings)
    ast.execute(execution_context)
    return ''.join(execution_context.result_text)


def main():
    import argparse
    import sys

    parser = argparse.ArgumentParser(
        formatter_class=argparse.RawDescriptionHelpFormatter,
        description='Generate Your Boilerplate!', epilog='''
    A GYB template consists of the following elements:

      - Literal text which is inserted directly into the output

      - %% or $$ in literal text, which insert literal '%' and '$'
        symbols respectively.

      - Substitutions of the form ${<python-expression>}.  The Python
        expression is converted to a string and the result is inserted
        into the output.

      - Python code delimited by %{...}%.  Typically used to inject
        definitions (functions, classes, variable bindings) into the
        evaluation context of the template.  Common indentation is
        stripped, so you can add as much indentation to the beginning
        of this code as you like

      - Lines beginning with optional whitespace followed by a single
        '%' and Python code.  %-lines allow you to nest other
        constructs inside them.  To close a level of nesting, use the
        "%end" construct.

      - Lines beginning with optional whitespace and followed by a
        single '%' and the token "end", which close open constructs in
        %-lines.

    Example template:

          - Hello -
        %{
             x = 42
             def succ(a):
                 return a+1
        }%

        I can assure you that ${x} < ${succ(x)}

        % if int(y) > 7:
        %    for i in range(3):
        y is greater than seven!
        %    end
        % else:
        y is less than or equal to seven
        % end

          - The End. -

    When run with "gyb -Dy=9", the output is

          - Hello -

        I can assure you that 42 < 43

        y is greater than seven!
        y is greater than seven!
        y is greater than seven!

          - The End. -
'''
    )
    parser.add_argument(
        '-D', action='append', dest='defines', metavar='NAME=VALUE',
        default=[],
        help='''Bindings to be set in the template's execution context''')

    parser.add_argument(
        'file', type=argparse.FileType(),
        help='Path to GYB template file (defaults to stdin)', nargs='?',
        default=sys.stdin)
    parser.add_argument(
        '-o', dest='target', type=argparse.FileType('w'),
        help='Output file (defaults to stdout)', default=sys.stdout)
    parser.add_argument(
        '--test', action='store_true',
        default=False, help='Run a self-test')
    parser.add_argument(
        '--verbose-test', action='store_true',
        default=False, help='Run a verbose self-test')
    parser.add_argument(
        '--dump', action='store_true',
        default=False, help='Dump the parsed template to stdout')
    parser.add_argument(
        '--line-directive', default='// ###sourceLocation',
        help='Line directive prefix; empty => no line markers')

    args = parser.parse_args(sys.argv[1:])

    if args.test or args.verbose_test:
        import doctest
        selfmod = sys.modules[__name__]
        if doctest.testmod(selfmod, verbose=args.verbose_test or None).failed:
            sys.exit(1)

    bindings = dict(x.split('=', 1) for x in args.defines)
    ast = parse_template(args.file.name, args.file.read())
    if args.dump:
        print(ast)
    # Allow the template to import .py files from its own directory
    sys.path = [os.path.split(args.file.name)[0] or '.'] + sys.path

    args.target.write(execute_template(ast, args.line_directive, **bindings))


if __name__ == '__main__':
    main()

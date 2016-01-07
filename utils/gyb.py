#!/usr/bin/env python
# GYB: Generate Your Boilerplate (improved names welcome; at least
# this one's short).  See -h output for instructions

from __future__ import print_function

import re
try:
    from cStringIO import StringIO
except ImportError:
    from io import StringIO
import tokenize
import textwrap
from bisect import bisect
import os

def getLineStarts(s):
    """Return a list containing the start index of each line in s.

    The list also contains a sentinel index for the end of the string,
    so there will be one more element in the list than there are lines
    in the string
    """
    starts = [0]

    for line in s.split('\n'):
        starts.append(starts[-1] + len(line)+1)

    starts[-1] -= 1
    return starts

def stripTrailingNL(s):
    """If s ends with a newline, drop it; else return s intact"""
    return s[:-1] if s.endswith('\n') else s

def splitLines(s):
    """Split s into a list of lines, each of which has a trailing newline
    
    If the lines are later concatenated, the result is s, possibly
    with a single appended newline.
    """
    return [ l + '\n' for l in s.split('\n') ]
    
# text on a line up to the first '$$', '${', or '%%'
literalText = r'(?: [^$\n%] | \$(?![${]) | %(?!%) )*'

# The part of an '%end' line that follows the '%' sign
linesClose = r'[\ \t]* end [\ \t]* (?: \# .* )? $'

## Note: Where "# Absorb" appears below, the regexp attempts to eat up
## through the end of ${...} and %{...}% constructs.  In reality we
## handle this with the Python tokenizer, which avoids mis-detections
## due to nesting, comments and strings.  This extra absorption in the
## regexp facilitates testing the regexp on its own, by preventing the
## interior of some of these constructs from being treated as literal
## text.
tokenizeRE = re.compile(
    r'''
# %-lines and %{...}-blocks
    # \n? # absorb one preceding newline
    ^
    (?: 
      (?P<gybLines>
        (?P<_indent> [\ \t]* % (?! [{%] ) [\ \t]* ) (?! [\ \t] | '''+linesClose+r''' ) .*
        ( \n (?P=_indent) (?! '''+linesClose+r''' ) .* ) *
      )
      | (?P<gybLinesClose> [\ \t]* % [ \t]* '''+linesClose+r''' )
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
| (?P<literal> '''+literalText+r'''
    (?: 
      # newline that doesn't precede space+%
      (?: \n (?! [\ \t]* %[^%] ) )
      '''+literalText+r'''
    )*
    \n?
  )
'''
, re.VERBOSE|re.MULTILINE)

gybBlockClose = re.compile('\}%[ \t]*\n?')

def tokenPosToIndex(tokenPos, start, lineStarts):
    """Translate a tokenize (line, column) pair into an absolute
    position in source text given the position where we started
    tokenizing and a list that maps lines onto their starting
    character indexes.
    """
    relativeTokenLinePlus1, tokenCol = tokenPos

    # line number where we started tokenizing
    startLineNum = bisect(lineStarts, start) - 1

    # line number of the token in the whole text
    absTokenLine = relativeTokenLinePlus1 - 1 + startLineNum

    # if found in the first line, adjust the end column to account
    # for the extra text
    if relativeTokenLinePlus1 == 1:
        tokenCol += start - lineStarts[startLineNum]

    # Sometimes tokenizer errors report a line beyond the last one
    if absTokenLine >= len(lineStarts):
        return lineStarts[-1]

    return lineStarts[absTokenLine] + tokenCol

def tokenizePythonToUnmatchedCloseCurly(sourceText, start, lineStarts):
    """Apply Python's tokenize to sourceText starting at index start
    while matching open and close curly braces.  When an unmatched
    close curly brace is found, return its index.  If not found,
    return len(sourceText).  If there's a tokenization error, return
    the position of the error.
    """
    stream = StringIO(sourceText)
    stream.seek( start )
    nesting = 0

    try:
        for kind, text, tokenStart, tokenEnd, lineText \
            in tokenize.generate_tokens(stream.readline):

            if text == '{':
                nesting += 1
            elif text == '}':
                nesting -= 1
                if nesting < 0:
                    return tokenPosToIndex(tokenStart, start, lineStarts)

    except tokenize.TokenError as error:
        (message, errorPos) = error.args
        return tokenPosToIndex(errorPos, start, lineStarts)

    return len(sourceText)
    
def tokenizeTemplate(templateText):
    r"""Given the text of a template, returns an iterator over
(tokenType,token,match) tuples.  

    **Note**: this is template syntax tokenization, not Python
    tokenization.

    When a non-literal token is matched, a client may call
    iter.send(pos) on the iterator to reset the position in
    templateText at which scanning will resume.

    This function provides a base level of tokenization which is
    then refined by ParseContext.tokenGenerator.

    >>> from pprint import *
    >>> pprint(list((kind, text) for kind,text,_ in tokenizeTemplate(
    ...   '%for x in range(10):\n%  print x\n%end\njuicebox')))
    [('gybLines', '%for x in range(10):\n%  print x'),
     ('gybLinesClose', '%end'),
     ('literal', 'juicebox')]

    >>> pprint(list((kind, text) for kind,text,_ in tokenizeTemplate(
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

    >>> for kind, text, _ in tokenizeTemplate('''
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
    ...     print (kind, text.strip().split('\n',1)[0])
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
    end = len(templateText)

    savedLiteral = []
    literalFirstMatch = None

    while pos < end:
        m = tokenizeRE.match(templateText, pos, end)
        
        # pull out the one matched key (ignoring internal patterns starting with _)
        ((kind, text), ) = (
            (kind,text) for (kind,text) in m.groupdict().items() 
            if text is not None and kind[0] != '_')
                
        if kind in ('literal', 'symbol'):
            if len(savedLiteral) == 0:
                literalFirstMatch = m
            savedLiteral.append(text) # literals and symbols get batched together
            pos = None
        else:
            # found a non-literal.  First yield any literal we've accumulated
            if savedLiteral != []:
                yield 'literal', ''.join(savedLiteral), literalFirstMatch
                savedLiteral = []

            # Then yield the thing we found.  If we get a reply, it's
            # the place to resume tokenizing
            pos = yield kind, text, m
        
        # If we were not sent a new position by our client, resume
        # tokenizing at the end of this match.
        if pos is None:
            pos = m.end(0)
        else:
            yield # Client is not yet ready to process next token

    if savedLiteral != []:
        yield 'literal', ''.join(savedLiteral), literalFirstMatch

def splitGybLines(sourceLines):
    r"""Return a list of lines at which to split the incoming source

    These positions represent the beginnings of python line groups that
    will require a matching %end construct if they are to be closed.

    >>> src = splitLines('''\
    ... if x:
    ...     print x
    ... if y: # trailing comment
    ...     print z
    ...     if z: # another comment\
    ... ''')
    >>> s = splitGybLines(src)
    >>> len(s)
    2
    >>> src[s[0]]
    '    print z\n'
    >>> s[1] - len(src)
    0

    >>> src = splitLines('''\
    ... if x:
    ...     if y: print 1
    ...     if z:
    ...         print 2
    ...     pass\
    ... ''')
    >>> s = splitGybLines(src)
    >>> len(s)
    1
    >>> src[s[0]]
    '    if y: print 1\n'

    >>> src = splitLines('''\
    ... if x:
    ...     if y:
    ...         print 1
    ...         print 2
    ... ''')
    >>> s = splitGybLines(src)
    >>> len(s)
    2
    >>> src[s[0]]
    '    if y:\n'
    >>> src[s[1]]
    '        print 1\n'
    """
    lastTokenText,lastTokenKind = None,None
    unmatchedIndents = []

    dedents = 0
    try:
        for tokenKind, tokenText, tokenStart, (tokenEndLine, tokenEndCol), lineText \
            in tokenize.generate_tokens(lambda i = iter(sourceLines): next(i)):

            if tokenKind in (tokenize.COMMENT, tokenize.ENDMARKER): 
                continue

            if tokenText == '\n' and lastTokenText == ':':
                unmatchedIndents.append(tokenEndLine)
                
            # The tokenizer appends dedents at EOF; don't consider
            # those as matching indentations.  Instead just save them
            # up...
            if lastTokenKind == tokenize.DEDENT:
                dedents += 1
            # And count them later, when we see something real.
            if tokenKind != tokenize.DEDENT and dedents > 0:
                unmatchedIndents = unmatchedIndents[:-dedents]
                dedents = 0
                
            lastTokenText,lastTokenKind = tokenText,tokenKind

    except tokenize.TokenError:
        return [] # Let the later compile() call report the error

    if lastTokenText == ':':
        unmatchedIndents.append(len(sourceLines))

    return unmatchedIndents

def codeStartsWithDedentKeyword(sourceLines):
    r"""Return True iff the incoming Python sourceLines begin with "else",
    "elif", "except", or "finally".

    Initial comments and whitespace are ignored.

    >>> codeStartsWithDedentKeyword(splitLines('if x in y: pass'))
    False
    >>> codeStartsWithDedentKeyword(splitLines('except ifSomethingElseHappens:'))
    True
    >>> codeStartsWithDedentKeyword(splitLines('\n# this is a comment\nelse: # yes'))
    True
    """
    tokenText = None
    for tokenKind, tokenText, _, _, _ \
        in tokenize.generate_tokens(lambda i = iter(sourceLines): next(i)):

        if tokenKind != tokenize.COMMENT and tokenText.strip() != '':
            break

    return tokenText in ('else', 'elif', 'except', 'finally')

class ParseContext:
    """State carried through a parse of a template"""
    filename = ''
    template = ''
    lineStarts = []
    codeStartLine = -1
    codeText = None
    tokens = None       # The rest of the tokens
    closeLines = False

    def __init__(self, filename, template=None):
        self.filename = os.path.abspath(filename)
        if template is None:
            with open(filename) as f:
                self.template = f.read()
        else:
            self.template = template
        self.lineStarts = getLineStarts(self.template)
        self.tokens = self.tokenGenerator(tokenizeTemplate(self.template))
        self.nextToken()

    def posToLine(self, pos):
        return bisect(self.lineStarts, pos) - 1
            
    def tokenGenerator(self, baseTokens):
        r""" Given an iterator over (kind, text, match) triples (see
        tokenizeTemplate above), return a refined iterator over
        tokenKinds.  

        Among other adjustments to the elements found by baseTokens,
        this refined iterator tokenizes python code embedded in
        template text to help determine its true extent.  The
        expression "baseTokens.send(pos)" is used to reset the index at
        which baseTokens resumes scanning the underlying text.

        >>> ctx = ParseContext('dummy', '''
        ... %for x in y:
        ... %    print x
        ... % end
        ... literally
        ... ''')
        >>> while ctx.tokenKind:
        ...     print (ctx.tokenKind, ctx.codeText or ctx.tokenText)
        ...     ignored = ctx.nextToken()
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
        >>> while ctx.tokenKind:
        ...     print (ctx.tokenKind, ctx.codeText or ctx.tokenText)
        ...     ignored = ctx.nextToken()
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
        ... %   elif x > 1:   # add an output line after this line to fix the bug
        ... %     if x == 2:
        ... literal2
        ... %     end
        ... %   end
        ... % end
        ... ''')
        >>> while ctx.tokenKind:
        ...     print (ctx.tokenKind, ctx.codeText or ctx.tokenText)
        ...     ignored = ctx.nextToken()
        ('gybLinesOpen', 'for x in [1, 2, 3]:\n')
        ('gybLinesOpen', '  if x == 1:\n')
        ('literal', 'literal1\n')
        ('gybLinesOpen', 'elif x > 1:   # add an output line after this line to fix the bug\n')
        ('gybLinesOpen', '  if x == 2:\n')
        ('literal', 'literal2\n')
        ('gybLinesClose', '%     end')
        ('gybLinesClose', '%   end')
        ('gybLinesClose', '% end')
        """
        for self.tokenKind, self.tokenText, self.tokenMatch in baseTokens:
            kind = self.tokenKind
            self.codeText = None
            
            # Do we need to close the current lines?
            self.closeLines = kind == 'gybLinesClose'

            if kind.endswith('Open'): # %{...}% and ${...} constructs

                # Tokenize text that follows as Python up to an unmatched '}'
                codeStart = self.tokenMatch.end(kind)
                self.codeStartLine = self.posToLine(codeStart)

                closePos = tokenizePythonToUnmatchedCloseCurly(
                    self.template, codeStart, self.lineStarts)
                self.codeText = self.template[codeStart:closePos]
                yield kind

                if (kind == 'gybBlockOpen'):
                    # Absorb any '}% <optional-comment> \n'
                    m2 = gybBlockClose.match(self.template, closePos)
                    if not m2:
                        raise ValueError("Invalid block closure")
                    nextPos = m2.end(0)
                else:
                    assert kind == 'substitutionOpen'
                    nextPos = closePos + 1 # skip past the closing '}'

                # Resume tokenizing after the end of the code.
                baseTokens.send(nextPos)

            elif kind == 'gybLines':
                
                self.codeStartLine = self.posToLine(self.tokenMatch.start('gybLines'))
                indentation = self.tokenMatch.group('_indent')

                # Strip off the leading indentation and %-sign
                sourceLines = re.split(
                    '^' + re.escape(indentation), 
                    self.tokenMatch.group('gybLines')+'\n', 
                    flags=re.MULTILINE)[1:]
                
                if codeStartsWithDedentKeyword(sourceLines):
                    self.closeLines = True

                lastSplit = 0
                for line in splitGybLines(sourceLines):
                    self.tokenKind = 'gybLinesOpen'
                    self.codeText = ''.join(sourceLines[lastSplit:line])
                    yield self.tokenKind
                    lastSplit = line
                    self.codeStartLine += line - lastSplit
                    self.closeLines = False

                self.codeText = ''.join(sourceLines[lastSplit:])
                if self.codeText:
                    self.tokenKind = 'gybLines'
                    yield self.tokenKind
            else:
                yield self.tokenKind


    def nextToken(self):
        """Move to the next token"""
        for kind in self.tokens:
            return self.tokenKind

        self.tokenKind = None

class ExecutionContext:
    """State we pass around during execution of a template"""
    def __init__(self, lineDirective='// ###line', **localBindings):
        self.localBindings = localBindings
        self.lineDirective = lineDirective
        self.localBindings['__context__'] = self
        self.resultText = []
        self.lastFileLine = None

    def appendText(self, text, file, line):
        # see if we need to inject a line marker
        if self.lineDirective:
            if (file,line) != self.lastFileLine:
                # We can only insert the line directive at a line break
                if len(self.resultText) == 0 \
                   or self.resultText[-1].endswith('\n'):
                    self.resultText.append('%s %d "%s"\n' % (self.lineDirective, line + 1, file))
                # But if the new text contains any line breaks, we can create one
                elif '\n' in text:
                    i = text.find('\n')
                    self.resultText.append(text[:i + 1])
                    self.lastFileLine = (self.lastFileLine[0], self.lastFileLine[1] + 1)
                    # and try again
                    self.appendText(text[i + 1:], file, line)
                    return
                    
        self.resultText.append(text)
        self.lastFileLine = (file, line + text.count('\n'))
        
class ASTNode(object):
    """Abstract base class for template AST nodes"""
    def __init__(self):
        raise NotImplemented

    def execute(self, context):
        raise NotImplemented

    def __str__(self, indent = ''):
        raise NotImplemented

    def formatChildren(self, indent):
        if not self.children:
            return ' []'

        return '\n'.join(
            ['', indent + '['] 
            + [ x.__str__(indent + 4*' ') for x in self.children ]
            + [indent + ']'])


class Block(ASTNode):
    """A sequence of other AST nodes, to be executed in order"""
    children = []

    def __init__(self, context):
        self.children = []

        while context.tokenKind and not context.closeLines:
            if context.tokenKind == 'literal':
                Node = Literal
            else:
                Node = Code
            self.children.append(Node(context))

    def execute(self, context):
        for x in self.children:
            x.execute(context)

    def __str__(self, indent = ''):
        return indent + 'Block:' + self.formatChildren(indent)

class Literal(ASTNode):
    """An AST node that generates literal text"""
    def __init__(self, context):
        self.text = context.tokenText
        startPosition = context.tokenMatch.start(context.tokenKind)
        self.startLineNumber = context.posToLine(startPosition)
        self.filename = context.filename
        context.nextToken()

    def execute(self, context):
        context.appendText(self.text, self.filename, self.startLineNumber)

    def __str__(self, indent = ''):
        return '\n'.join(
            [indent + x for x in [ 'Literal:'] + stripTrailingNL(self.text).split('\n')])

class Code(ASTNode):
    """An AST node that is evaluated as Python"""
    code = None
    children = ()
    kind = None

    def __init__(self, context):

        source = ''
        sourceLineCount = 0

        def accumulateCode():
            s = source + (context.codeStartLine - sourceLineCount) * '\n' \
                + textwrap.dedent(context.codeText)
            lineCount = context.codeStartLine + context.codeText.count('\n')
            context.nextToken()
            return s, lineCount

        evalExec = 'exec'
        if context.tokenKind.startswith('substitution'):
            evalExec = 'eval'
            source, sourceLineCount = accumulateCode()
            source = '('+source.strip()+')'

        else:
            while context.tokenKind == 'gybLinesOpen':
                source, sourceLineCount = accumulateCode()
                source += '    __children__[%d].execute(__context__)\n' % len(self.children)
                sourceLineCount += 1

                self.children += (Block(context),)

            if context.tokenKind == 'gybLinesClose':
                context.nextToken()
        
        if context.tokenKind == 'gybLines':
            source, sourceLineCount = accumulateCode()
            
            # Only handle a substitution as part of this code block if
            # we don't already have some %-lines.
        elif context.tokenKind == 'gybBlockOpen':

            # Opening ${...} and %{...}% constructs
            source, sourceLineCount = accumulateCode()

        self.filename = context.filename
        self.startLineNumber = context.codeStartLine
        self.code = compile(source, context.filename, evalExec)
        self.source = source

    def execute(self, context):
        # Save __children__ from the local bindings
        saveChildren = context.localBindings.get('__children__')
        # Execute the code with our __children__ in scope 
        context.localBindings['__children__'] = self.children
        result = eval(self.code, context.localBindings)

        if context.localBindings['__children__'] is not self.children:
            raise ValueError("The code is not allowed to mutate __children__")
        # Restore the bindings
        context.localBindings['__children__'] = saveChildren

        # If we got a result, the code was an expression, so append
        # its value
        if result is not None and result != '':
            context.appendText(str(result), self.filename, self.startLineNumber)

    def __str__(self, indent = ''):
        sourceLines = re.sub(r'^\n', '', stripTrailingNL(self.source), flags=re.MULTILINE).split('\n')
        if len(sourceLines) == 1:
            s = indent + 'Code: {' + sourceLines[0] + '}'
        else:
            s = indent + 'Code:\n' + indent + '{\n' + '\n'.join(
                indent + 4*' ' + l for l in sourceLines
            ) + '\n' + indent + '}'
        return s + self.formatChildren(indent)

def parseTemplate(filename, text = None):
    r"""Return an AST corresponding to the given template file.
    
    If text is supplied, it is assumed to be the contents of the file,
    as a string.

    >>> print parseTemplate('dummy.file', text=
    ... '''% for x in [1, 2, 3]:
    ... %   if x == 1:
    ... literal1
    ... %   elif x > 1:   # add an output line after this line to fix the bug
    ... %     if x == 2:
    ... literal2
    ... %     end
    ... %   end
    ... % end
    ... ''')
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
                    elif x > 1:   # add an output line after this line to fix the bug
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

    >>> print parseTemplate('dummy.file', text='%for x in range(10):\n%  print x\n%end\njuicebox')
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
                Code: {print x} []
            ]
        ]
        Literal:
        juicebox
    ]

    >>> print parseTemplate('/dummy.file', text=
    ... '''Nothing
    ... % if x:
    ... %    for i in range(3):
    ... ${i}
    ... %    end
    ... % else:
    ... THIS SHOULD NOT APPEAR IN THE OUTPUT
    ... ''')
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

    >>> print parseTemplate('dummy.file', text='''%
    ... %for x in y:
    ... %    print y
    ... ''')
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
                Code: {print y} []
            ]
        ]
    ]

    >>> print parseTemplate('dummy.file', text='''%
    ... %if x:
    ... %    print y
    ... AAAA
    ... %else:
    ... BBBB
    ... ''')
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
                Code: {print y} []
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

    >>> print parseTemplate('dummy.file', text='''%
    ... %if x:
    ... %    print y
    ... AAAA
    ... %# This is a comment
    ... %else:
    ... BBBB
    ... ''')
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
                Code: {print y} []
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

    >>> print parseTemplate('dummy.file', text='''\
    ... %for x in y:
    ... AAAA
    ... %if x:
    ... BBBB
    ... %end
    ... CCCC
    ... ''')
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

def executeTemplate(ast, lineDirective='', **localBindings):
    r"""Return the text generated by executing the given template AST.

Keyword arguments become local variable bindings in the execution context

    >>> ast = parseTemplate('/dummy.file', text=
    ... '''Nothing
    ... % if x:
    ... %    for i in range(3):
    ... ${i}
    ... %    end
    ... % else:
    ... THIS SHOULD NOT APPEAR IN THE OUTPUT
    ... ''')
    >>> print executeTemplate(ast, lineDirective='//#line', x=1),
    //#line 1 "/dummy.file"
    Nothing
    //#line 4 "/dummy.file"
    0
    //#line 4 "/dummy.file"
    1
    //#line 4 "/dummy.file"
    2

    >>> ast = parseTemplate('/dummy.file', text=
    ... '''Nothing
    ... % a = []
    ... % for x in range(3):
    ... %    a.append(x)
    ... % end
    ... ${a}
    ... ''')
    >>> print executeTemplate(ast, lineDirective='//#line', x=1),
    //#line 1 "/dummy.file"
    Nothing
    //#line 6 "/dummy.file"
    [0, 1, 2]
"""
    executionContext = ExecutionContext(lineDirective=lineDirective, **localBindings)
    ast.execute(executionContext)
    return ''.join(executionContext.resultText)

def main():
    import argparse, sys

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
    parser.add_argument('-D', action='append', dest='defines', metavar='NAME=VALUE',
                    default=[],
                    help='''Bindings to be set in the template's execution context'''
                    )

    parser.add_argument('file', type=argparse.FileType(), help='Path to GYB template file (defaults to stdin)', nargs='?', default=sys.stdin)
    parser.add_argument('-o', dest='target', type=argparse.FileType('w'), help='Output file (defaults to stdout)', default=sys.stdout)
    parser.add_argument('--test', action='store_true', default=False, help='Run a self-test')
    parser.add_argument('--verbose-test', action='store_true', default=False, help='Run a verbose self-test')
    parser.add_argument('--dump', action='store_true', default=False, help='Dump the parsed template to stdout')
    parser.add_argument('--line-directive', default='// ###line', help='Line directive prefix; empty => no line markers')
    

    args = parser.parse_args(sys.argv[1:])

    if args.test or args.verbose_test:
        import doctest
        if doctest.testmod(verbose=args.verbose_test).failed:
            sys.exit(1)
        
    bindings = dict( x.split('=', 1) for x in args.defines )
    ast = parseTemplate(args.file.name, args.file.read())
    if args.dump:
        
        print(ast)
    # Allow the template to import .py files from its own directory
    sys.path = [os.path.split(args.file.name)[0] or '.'] + sys.path
    
    args.target.write(executeTemplate(ast, args.line_directive, **bindings))

if __name__ == '__main__':
    main()
    

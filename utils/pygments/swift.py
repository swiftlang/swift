#!/usr/bin/python

import re

from pygments.lexer import Lexer, RegexLexer, include, bygroups, using, this, do_insertions
from pygments.token import *

__all__ = ['SwiftLexer', 'SwiftConsoleLexer']


class SwiftLexer(RegexLexer):
    name = 'Swift'
    aliases = ['swift']
    filenames = ['*.swift']

    flags = re.MULTILINE | re.DOTALL

    _isa = r'([a-zA-Z_][a-zA-Z0-9_]*)(\s+)(:)(\s+)([A-Z0-9_][a-zA-Z0-9_]*)' 
    _isa_comma = r'([a-zA-Z_][a-zA-Z0-9_]*)(\s+)(:)(\s+)([A-Z0-9_][a-zA-Z0-9_]*)(,\s?)'
    _name = r'[a-zA-Z_][a-zA-Z0-9_?]*'

    tokens = {
   
        'root' : [
            (r'^', Punctuation, 'root2'),
        ],
 
        'root2' : [
            (r'\n', Text, '#pop'),
             
            (r'//.*?\n', Comment.Single, '#pop'),
            (r'/\*', Comment.Multiline, 'comment'),

            (r'\b(var|let)\s', Keyword.Declaration, 'var-decl'),
            (r'\bfor\s', Keyword.Reserved, 'for-loop'),
            (r'\b(func|init|deinit)\s', Keyword.Declaration, 'func-decl'),
            (r'(\bset\b)(\s?)(\()', bygroups(Keyword.Declaration, Whitespace, Punctuation), 'arg-list'),
            (r'(set|get)(:)', bygroups(Keyword.Reserved, Punctuation)),
            (r'\b(self|Self)\b', Name.Builtin.Pseudo),
            (r'\bid\b', Name.Builtin),

            (r'\bimport\s+', Keyword.Namespace, 'import'),
            (r'\b(class|struct|protocol|extension)\s', Keyword.Declaration, 'class-decl'),

            (r'(\b[A-Z][a-zA-Z0-9_]*\s?)(\()', bygroups(Name.Constant, Punctuation), 'type-cast'),
            (r'(\b[A-Z][a-zA-Z0-9_]*)(\.)([a-z][a-zA-Z0-9_]*)', bygroups(Name.Constant, Punctuation, Name), 'arg-list'),
            (r'"', String, 'string'),
            
            (r'(\bnew\b\s?)', Keyword.Reserved, 'class-name'),
            (r'\b(true|false)\b', Keyword.Reserved),
            (r'\b(if|else)\s', Keyword.Reserved),
            (r'\b(return|break)\b', Keyword.Reserved),

            (r'[\^\*!%&<>+=/?-]|\.{2}', Operator),
            (r'\$([0-9]+)', Name.Variable),   #Tokens
            (r'[\[\]\(\)\{\}\|:;,.#]', Punctuation),
            (r'[0-9]+\.[0-9]+', Number.Float),
            (r'0x[0-9a-fA-F]+', Number.Hex),
            (r'[0-9]+', Number.Integer),
            (r'\s', Whitespace),
            
            (r'\(', Punctuation, 'tuple'),
            
            include('name'),
            
        ],

        'isa' : [
            (_isa, bygroups(Name, Whitespace, Punctuation, Whitespace, Name.Constant)),
        ],
        
        'class-isa' : [
            (_isa, bygroups(Name.Class, Whitespace, Punctuation, Whitespace, Name.Constant)),
        ],

        'var-isa' : [
            (_isa, bygroups(Name.Variable, Whitespace, Punctuation, Whitespace, Name.Constant)),
        ],

        'var-isa-pop' : [
            (_isa, bygroups(Name.Variable, Whitespace, Punctuation, Whitespace, Name.Constant), '#pop'),
        ],

        'var-isa-comma' : [
            (_isa_comma, bygroups(Name.Variable, Whitespace, Punctuation, Whitespace, Name.Constant, Punctuation)),
        ],

        'var-name' : [
            (r'[a-zA-Z_][a-zA-Z0-9_?]*', Name.Variable),
        ],

        'tuple' : [
            (r'\(', Punctuation, 'in-tuple'),
        ],

        'in-tuple' : [
            (r'\)', Punctuation, '#pop'),
            include('class-name'),
            include('name'),
            include('isa'),
            include('root2'),
        ],
        
        'name' : [
            (_name, Name),
        ],

        'comment' : [
            (r'[^*/]', Comment.Multiline),
            (r'/\*', Comment.Multiline, '#push'),
            (r'\*/', Comment.Multiline, '#pop'),
            (r'[*/]', Comment.Multiline),
        ],

        'import' : [
            (_name, Name.Namespace),
            #('\n', Punctuation, '#pop'),
        ],

        'generic-type' : [
            (r'>', Punctuation, '#pop'),
            include('class-name'),
            include('isa'),
            include('root2'),
        ],

        'class-name' : [
            (r'[A-Z][a-zA-Z0-9_?]*', Name.Constant),
            (r'(\[)([0-9]+)(\])', bygroups(Operator, Number.Integer, Operator)), 
            (r'<', Punctuation, 'generic-type'),
            (r'\.\(', Punctuation, 'arg-list'),
            (r'\(', Punctuation, 'type-cast'),
            (r'\)', Punctuation, '#pop'),
        ],

        'label' : [
            (r'[a-zA-Z_][a-zA-Z0-9_]*:(?=\s*\n)', Name.Label),
        ],

        'ws-pop' : [
            (r'\s?[\s\n]', Whitespace, '#pop'),
        ],
            
        'var-decl' : [ 
            (r'(\[)([\w\s,]*)(\])(\s+)', bygroups(Punctuation, Name.Attribute, Punctuation, Whitespace)),
            include('tuple'),
            include('var-isa-comma'),
            include('var-isa-pop'),
            include('var-name'),
            (r',\s+', Punctuation, 'var-decl'),
            include('ws-pop'), 
        ],

        'for-loop' : [
            (r'\sin\s', Keyword.Reserved),
            include('isa'),
            include('name'),
            include('ws-pop'),
            include('root2'),
        ],

        'func-decl' : [
            (r'(\[)([\w\s,]*)(\])(\s+)', bygroups(Punctuation, Name.Attribute, Punctuation, Whitespace)),
            (r'\s?\breturn\b', Keyword.Reserved, 'root2'),
            (r'\s?\w', Name.Function),
            (r'<', Punctuation, 'generic-type'),
            (r'\(\s?', Punctuation, 'arg-list'),
            (r'\s?->\s?', Operator, 'return-type'),
            (r'\s?\{', Punctuation, '#pop'),
        ],

        'return-type' : [
            include('tuple'),
            include('class-name'),
            (r'\bid\b', Name.Builtin),
            (r'\s?\{', Punctuation, '#pop'),
            (r'\s?\)', Punctuation),
        ],

        'class-decl' : [
            (r'\{', Punctuation, '#pop'),
            (r'(\[)([\w\s,]*)(\])(\s+)', bygroups(Punctuation, Name.Attribute, Punctuation, Whitespace)),
            include('class-isa'),
            (r'[A-Z][a-zA-Z0-9_?]*', Name.Class),
            (r'\s', Whitespace),
            (r'<', Punctuation, 'generic-type'),
        ], 

        'arg-list' : [
            (r',\s?', Punctuation),
            (r'\)', Punctuation, '#pop'),
            include('isa'),
            (r'\s?->\s?', Operator, 'return-type'),
            include('root2'),
        ],

        'type-cast' : [
            (r'\)', Punctuation, '#pop'),
            include('root2'),
        ],

        'in-interpolated' : [
            ('\)', String.Interpol, '#pop'),
            include('root2'),
        ],

        'string' : [
            (r'"', String, '#pop'),
            (r'\\([\\abfnrtv"\']|x[a-fA-F0-9]{2,4}|[0-7]{1,3})', String.Escape),
            (r'\\\(', String.Interpol, 'in-interpolated'),
            (r'[^\\"]+', String),
            (r'\\', String),
        ],
    }


class SwiftConsoleLexer(RegexLexer):
    name = 'SwiftConsole'
    aliases = ['swift-console']
    filenames = ['*.swiftc']

    flags = re.MULTILINE | re.DOTALL

    _isa = r'([a-zA-Z_][a-zA-Z0-9_]*)(\s+)(:)(\s+)([A-Z0-9_][a-zA-Z0-9_]*)' 
    _isa_comma = r'([a-zA-Z_][a-zA-Z0-9_]*)(\s+)(:)(\s+)([A-Z0-9_][a-zA-Z0-9_]*)(,\s?)'
    _name = r'[a-zA-Z_][a-zA-Z0-9_?]*'

    tokens = SwiftLexer.tokens.copy()
    tokens['root'] = [
        (r'Welcome to swift.  Type \':help\' for assistance.', Generic.Prompt),
        (r'(\(swift\) |        )', Generic.Prompt, 'root2'),
        (r'\(swift\)', Generic.Prompt),
        (r'       ', Generic.Prompt),
        (r'//.*?\n', Generic.Output),
        (r'<REPL Buffer>:[0-9]*:[0-9]*:.*?\n', Generic.Heading),
        (r'~*?\^\s?~*?\n', Generic.Heading),
        (r'.*?\n', Generic.Output),
    ]

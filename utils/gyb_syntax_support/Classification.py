from Node import error
from kinds import lowercase_first_word  # noqa: I201


class SyntaxClassification(object):
    '''
    Represents a classification a token can receive for syntax highlighting.
    '''

    def __init__(self, name, description):
        self.name = name
        self.swift_name = lowercase_first_word(name)
        self.description = description


SYNTAX_CLASSIFICATIONS = [
    SyntaxClassification('None', description='''
    The token should not receive syntax coloring.
    '''),
    SyntaxClassification('Keyword', description='''
    A Swift keyword, including contextual keywords.
    '''),
    SyntaxClassification('Identifier', description='''
    A generic identifier.
    '''),
    SyntaxClassification('TypeIdentifier', description='''
    An identifier referring to a type.
    '''),
    SyntaxClassification('DollarIdentifier', description='''
    An identifier starting with `$` like `$0`.
    '''),
    SyntaxClassification('IntegerLiteral', description='''
    An integer literal.
    '''),
    SyntaxClassification('FloatingLiteral', description='''
    A floating point literal.
    '''),
    SyntaxClassification('StringLiteral', description='''
    A string literal including multiline string literals.
    '''),
    SyntaxClassification('StringInterpolationAnchor', description='''
    The opening and closing paranthesis of string interpolation.
    '''),
    SyntaxClassification('PoundDirectiveKeyword', description='''
    A `#` keyword like `#warning`.
    '''),
    SyntaxClassification('BuildConfigId', description='''
    A build configuration directive like `#if`, `#elseif`, `#else`.
    '''),
    SyntaxClassification('Attribute', description='''
    An attribute starting with an `@`.
    '''),
    SyntaxClassification('ObjectLiteral', description='''
    An image, color, etc. literal.
    '''),
    SyntaxClassification('EditorPlaceholder', description='''
    An editor placeholder of the form `<#content#>`
    '''),
    SyntaxClassification('LineComment', description='''
    A line comment starting with `//`.
    '''),
    SyntaxClassification('DocLineComment', description='''
    A doc line comment starting with `///`.
    '''),
    SyntaxClassification('BlockComment', description='''
    A block comment starting with `/**` and ending with `*/.
    '''),
    SyntaxClassification('DocBlockComment', description='''
    A doc block comment starting with `/**` and ending with `*/.
    '''),
]


def classification_by_name(classification_name):
    if classification_name is None:
        return None
    for classification in SYNTAX_CLASSIFICATIONS:
        if classification.name == classification_name:
            return classification
    error("Unknown syntax classification '%s'" % classification_name)

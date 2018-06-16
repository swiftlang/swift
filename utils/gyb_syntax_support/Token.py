from Classification import classification_by_name
from Node import error
from kinds import lowercase_first_word


class Token(object):
    """
    Represents the specification for a Token in the TokenSyntax file.
    """

    def __init__(self, name, kind, serialization_code, text=None, 
                 classification='None', is_keyword=False):
        self.name = name
        self.kind = kind
        self.serialization_code = serialization_code
        self.text = text or ""
        self.classification = classification_by_name(classification)
        self.is_keyword = is_keyword

    def swift_kind(self):
        name = lowercase_first_word(self.name)
        if self.is_keyword:
            return name + 'Keyword'
        return name


class Keyword(Token):
    """
    Represents a keyword token.
    """

    def __init__(self, name, text, serialization_code):
        Token.__init__(self, name, 'kw_' + text, serialization_code, text=text, 
                       classification='Keyword', is_keyword=True)


SYNTAX_TOKENS = [
    Keyword('Associatedtype', 'associatedtype', serialization_code=1),
    Keyword('Class', 'class', serialization_code=2),
    Keyword('Deinit', 'deinit', serialization_code=3),
    Keyword('Enum', 'enum', serialization_code=4),
    Keyword('Extension', 'extension', serialization_code=5),
    Keyword('Func', 'func', serialization_code=6),
    Keyword('Import', 'import', serialization_code=7),
    Keyword('Init', 'init', serialization_code=8),
    Keyword('Inout', 'inout', serialization_code=9),
    Keyword('Let', 'let', serialization_code=10),
    Keyword('Operator', 'operator', serialization_code=11),
    Keyword('Precedencegroup', 'precedencegroup', serialization_code=12),
    Keyword('Protocol', 'protocol', serialization_code=13),
    Keyword('Struct', 'struct', serialization_code=14),
    Keyword('Subscript', 'subscript', serialization_code=15),
    Keyword('Typealias', 'typealias', serialization_code=16),
    Keyword('Var', 'var', serialization_code=17),
    Keyword('Fileprivate', 'fileprivate', serialization_code=18),
    Keyword('Internal', 'internal', serialization_code=19),
    Keyword('Private', 'private', serialization_code=20),
    Keyword('Public', 'public', serialization_code=21),
    Keyword('Static', 'static', serialization_code=22),
    Keyword('Defer', 'defer', serialization_code=23),
    Keyword('If', 'if', serialization_code=24),
    Keyword('Guard', 'guard', serialization_code=25),
    Keyword('Do', 'do', serialization_code=26),
    Keyword('Repeat', 'repeat', serialization_code=27),
    Keyword('Else', 'else', serialization_code=28),
    Keyword('For', 'for', serialization_code=29),
    Keyword('In', 'in', serialization_code=30),
    Keyword('While', 'while', serialization_code=31),
    Keyword('Return', 'return', serialization_code=32),
    Keyword('Break', 'break', serialization_code=33),
    Keyword('Continue', 'continue', serialization_code=34),
    Keyword('Fallthrough', 'fallthrough', serialization_code=35),
    Keyword('Switch', 'switch', serialization_code=36),
    Keyword('Case', 'case', serialization_code=37),
    Keyword('Default', 'default', serialization_code=38),
    Keyword('Where', 'where', serialization_code=39),
    Keyword('Catch', 'catch', serialization_code=40),
    Keyword('As', 'as', serialization_code=41),
    Keyword('Any', 'Any', serialization_code=42),
    Keyword('False', 'false', serialization_code=43),
    Keyword('Is', 'is', serialization_code=44),
    Keyword('Nil', 'nil', serialization_code=45),
    Keyword('Rethrows', 'rethrows', serialization_code=46),
    Keyword('Super', 'super', serialization_code=47),
    Keyword('Self', 'self', serialization_code=48),
    Keyword('CapitalSelf', 'Self', serialization_code=49),
    Keyword('Throw', 'throw', serialization_code=50),
    Keyword('True', 'true', serialization_code=51),
    Keyword('Try', 'try', serialization_code=52),
    Keyword('Throws', 'throws', serialization_code=53),
    Keyword('__FILE__', '__FILE__', serialization_code=54),
    Keyword('__LINE__', '__LINE__', serialization_code=55),
    Keyword('__COLUMN__', '__COLUMN__', serialization_code=56),
    Keyword('__FUNCTION__', '__FUNCTION__', serialization_code=57),
    Keyword('__DSO_HANDLE__', '__DSO_HANDLE__', serialization_code=58),
    Keyword('Wildcard', '_', serialization_code=59),
    Token('PoundAvailable', 'pound_available', text='#available',
          is_keyword=True, classification='Keyword', serialization_code=60),
    Token('PoundEndif', 'pound_endif', text='#endif',
          is_keyword=True, classification='PoundDirectiveKeyword', 
          serialization_code=61),
    Token('PoundElse', 'pound_else', text='#else',
          is_keyword=True, classification='PoundDirectiveKeyword', 
          serialization_code=62),
    Token('PoundElseif', 'pound_elseif', text='#elseif',
          is_keyword=True, classification='PoundDirectiveKeyword', 
          serialization_code=63),
    Token('PoundIf', 'pound_if', text='#if',
          is_keyword=True, classification='PoundDirectiveKeyword', 
          serialization_code=64),
    Token('PoundSourceLocation', 'pound_sourceLocation',
          text='#sourceLocation', is_keyword=True,
          classification='PoundDirectiveKeyword', serialization_code=65),
    Token('PoundWarning', 'pound_warning', text='#warning', is_keyword=True, 
          classification='PoundDirectiveKeyword', serialization_code=66),
    Token('PoundError', 'pound_error', text='#error', is_keyword=True,
          classification='PoundDirectiveKeyword', serialization_code=67),
    Token('PoundFile', 'pound_file', text='#file',
          is_keyword=True, classification='Keyword', serialization_code=68),
    Token('PoundLine', 'pound_line', text='#line',
          is_keyword=True, classification='Keyword', serialization_code=69),
    Token('PoundColumn', 'pound_column', text='#column',
          is_keyword=True, classification='Keyword', serialization_code=70),
    Token('PoundDsohandle', 'pound_dsohandle', text='#dsohandle',
          is_keyword=True, classification='Keyword', serialization_code=71),
    Token('PoundFunction', 'pound_function', text='#function',
          is_keyword=True, classification='Keyword', serialization_code=72),
    Token('PoundSelector', 'pound_selector', text='#selector',
          is_keyword=True, classification='Keyword', serialization_code=73),
    Token('PoundKeyPath', 'pound_keyPath', text='#keyPath',
          is_keyword=True, classification='Keyword', serialization_code=74),
    Token('PoundColorLiteral', 'pound_colorLiteral', text='#colorLiteral',
          is_keyword=True, classification='ObjectLiteral', 
          serialization_code=75),
    Token('PoundFileLiteral', 'pound_fileLiteral', text='#fileLiteral',
          is_keyword=True, classification='ObjectLiteral', 
          serialization_code=76),
    Token('PoundImageLiteral', 'pound_imageLiteral', text='#imageLiteral',
          is_keyword=True, classification='ObjectLiteral', 
          serialization_code=77),
    Token('Arrow', 'arrow', text='->', serialization_code=78),
    Token('Backtick', 'backtick', text='`', serialization_code=79),
    Token('AtSign', 'at_sign', text='@', classification='Attribute', 
          serialization_code=80),
    Token('Pound', 'pound', text='#', serialization_code=81),
    Token('Colon', 'colon', text=':', serialization_code=82),
    Token('Semicolon', 'semi', text=';', serialization_code=83),
    Token('Comma', 'comma', text=',', serialization_code=84),
    Token('Period', 'period', text='.', serialization_code=85),
    Token('Equal', 'equal', text='=', serialization_code=86),
    Token('PrefixPeriod', 'period_prefix', text='.', serialization_code=87),
    Token('LeftParen', 'l_paren', text='(', serialization_code=88),
    Token('RightParen', 'r_paren', text=')', serialization_code=89),
    Token('LeftBrace', 'l_brace', text='{', serialization_code=90),
    Token('RightBrace', 'r_brace', text='}', serialization_code=91),
    Token('LeftSquareBracket', 'l_square', text='[', serialization_code=92),
    Token('RightSquareBracket', 'r_square', text=']', serialization_code=93),
    Token('LeftAngle', 'l_angle', text='<', serialization_code=94),
    Token('RightAngle', 'r_angle', text='>', serialization_code=95),
    Token('PrefixAmpersand', 'amp_prefix', text='&', serialization_code=96),
    Token('PostfixQuestionMark', 'question_postfix', text='?', 
          serialization_code=97),
    Token('InfixQuestionMark', 'question_infix', text='?', 
          serialization_code=98),
    Token('ExclamationMark', 'exclaim_postfix', text='!', 
          serialization_code=99),
    Token('Backslash', 'backslash', text='\\\\', serialization_code=100),
    Token('StringInterpolationAnchor', 'string_interpolation_anchor',
          text=')', classification='StringInterpolationAnchor', 
          serialization_code=101),
    Token('StringQuote', 'string_quote', text='\\\"', 
          classification='StringLiteral', serialization_code=102),
    Token('MultilineStringQuote', 'multiline_string_quote',
          text='\\\"\\\"\\\"', classification='StringLiteral', 
          serialization_code=103),
    Token('StringSegment', 'string_segment', classification='StringLiteral', 
          serialization_code=104),
    Token('Identifier', 'identifier', classification=None, 
          serialization_code=105),
    Token('DollarIdentifier', 'dollarident', 
          classification='DollarIdentifier', serialization_code=106),
    Token('UnspacedBinaryOperator', 'oper_binary_unspaced', 
          serialization_code=107),
    Token('SpacedBinaryOperator', 'oper_binary_spaced', 
          serialization_code=108),
    Token('PrefixOperator', 'oper_prefix', serialization_code=109),
    Token('PostfixOperator', 'oper_postfix', serialization_code=110),
    Token('IntegerLiteral', 'integer_literal', 
          classification='IntegerLiteral', serialization_code=111),
    Token('FloatingLiteral', 'floating_literal', 
          classification='FloatingLiteral', serialization_code=112),
    Token('StringLiteral', 'string_literal',
          classification='StringLiteral', serialization_code=113),
    Token('ContextualKeyword', 'contextual_keyword', classification='Keyword', 
          serialization_code=114),
    Token('Unknown', 'unknown', serialization_code=115),
]

SYNTAX_TOKEN_MAP = {token.name + 'Token': token for token in SYNTAX_TOKENS}


def verify_no_duplicate_serialization_codes(tokens):
    used_codes = set()
    for token in tokens:
        if token.serialization_code in used_codes:
            error("Serialization code %d used twice for tokens" % 
                  token.serialization_code)
        used_codes.add(token.serialization_code)


verify_no_duplicate_serialization_codes(SYNTAX_TOKENS)

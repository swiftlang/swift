from kinds import lowercase_first_word


class Token(object):
    """
    Represents the specification for a Token in the TokenSyntax file.
    """

    def __init__(self, name, kind, text=None, is_keyword=False):
        self.name = name
        self.kind = kind
        self.text = text or ""
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

    def __init__(self, name, text):
        Token.__init__(self, name, 'kw_' + text, text=text, is_keyword=True)


SYNTAX_TOKENS = [
    Keyword('Associatedtype', 'associatedtype'),
    Keyword('Class', 'class'),
    Keyword('Deinit', 'deinit'),
    Keyword('Enum', 'enum'),
    Keyword('Extension', 'extension'),
    Keyword('Func', 'func'),
    Keyword('Import', 'import'),
    Keyword('Init', 'init'),
    Keyword('Inout', 'inout'),
    Keyword('Let', 'let'),
    Keyword('Operator', 'operator'),
    Keyword('Precedencegroup', 'precedencegroup'),
    Keyword('Protocol', 'protocol'),
    Keyword('Struct', 'struct'),
    Keyword('Subscript', 'subscript'),
    Keyword('Typealias', 'typealias'),
    Keyword('Var', 'var'),
    Keyword('Fileprivate', 'fileprivate'),
    Keyword('Internal', 'internal'),
    Keyword('Private', 'private'),
    Keyword('Public', 'public'),
    Keyword('Static', 'static'),
    Keyword('Defer', 'defer'),
    Keyword('If', 'if'),
    Keyword('Guard', 'guard'),
    Keyword('Do', 'do'),
    Keyword('Repeat', 'repeat'),
    Keyword('Else', 'else'),
    Keyword('For', 'for'),
    Keyword('In', 'in'),
    Keyword('While', 'while'),
    Keyword('Return', 'return'),
    Keyword('Break', 'break'),
    Keyword('Continue', 'continue'),
    Keyword('Fallthrough', 'fallthrough'),
    Keyword('Switch', 'switch'),
    Keyword('Case', 'case'),
    Keyword('Default', 'default'),
    Keyword('Where', 'where'),
    Keyword('Catch', 'catch'),
    Keyword('As', 'as'),
    Keyword('Any', 'Any'),
    Keyword('False', 'false'),
    Keyword('Is', 'is'),
    Keyword('Nil', 'nil'),
    Keyword('Rethrows', 'rethrows'),
    Keyword('Super', 'super'),
    Keyword('Self', 'self'),
    Keyword('CapitalSelf', 'Self'),
    Keyword('Throw', 'throw'),
    Keyword('True', 'true'),
    Keyword('Try', 'try'),
    Keyword('Throws', 'throws'),
    Keyword('__FILE__', '__FILE__'),
    Keyword('__LINE__', '__LINE__'),
    Keyword('__COLUMN__', '__COLUMN__'),
    Keyword('__FUNCTION__', '__FUNCTION__'),
    Keyword('__DSO_HANDLE__', '__DSO_HANDLE__'),
    Keyword('Wildcard', '_'),
    Token('PoundAvailable', 'pound_available', text='#available',
          is_keyword=True),
    Token('PoundEndif', 'pound_endif', text='#endif',
          is_keyword=True),
    Token('PoundElse', 'pound_else', text='#else',
          is_keyword=True),
    Token('PoundElseif', 'pound_elseif', text='#elseif',
          is_keyword=True),
    Token('PoundIf', 'pound_if', text='#if',
          is_keyword=True),
    Token('PoundSourceLocation', 'pound_sourceLocation',
          text='#sourceLocation', is_keyword=True),
    Token('PoundFile', 'pound_file', text='#file',
          is_keyword=True),
    Token('PoundLine', 'pound_line', text='#line',
          is_keyword=True),
    Token('PoundColumn', 'pound_column', text='#column',
          is_keyword=True),
    Token('PoundDsohandle', 'pound_dsohandle', text='#dsohandle',
          is_keyword=True),
    Token('PoundFunction', 'pound_function', text='#function',
          is_keyword=True),
    Token('PoundSelector', 'pound_selector', text='#selector',
          is_keyword=True),
    Token('PoundKeyPath', 'pound_keyPath', text='#keyPath',
          is_keyword=True),
    Token('PoundColorLiteral', 'pound_colorLiteral', text='#colorLiteral',
          is_keyword=True),
    Token('PoundFileLiteral', 'pound_fileLiteral', text='#fileLiteral',
          is_keyword=True),
    Token('PoundImageLiteral', 'pound_imageLiteral', text='#imageLiteral',
          is_keyword=True),
    Token('Arrow', 'arrow', text='->'),
    Token('AtSign', 'at_sign', text='@'),
    Token('Colon', 'colon', text=':'),
    Token('Semicolon', 'semi', text=';'),
    Token('Comma', 'comma', text=','),
    Token('Period', 'period', text='.'),
    Token('Equal', 'equal', text='='),
    Token('PrefixPeriod', 'period_prefix', text='.'),
    Token('LeftParen', 'l_paren', text='('),
    Token('RightParen', 'r_paren', text=')'),
    Token('LeftBrace', 'l_brace', text='{'),
    Token('RightBrace', 'r_brace', text='}'),
    Token('LeftSquareBracket', 'l_square', text='['),
    Token('RightSquareBracket', 'r_square', text=']'),
    Token('LeftAngle', 'l_angle', text='<'),
    Token('RightAngle', 'r_angle', text='>'),
    Token('PrefixAmpersand', 'amp_prefix', text='&'),
    Token('PostfixQuestionMark', 'question_postfix', text='?'),
    Token('InfixQuestionMark', 'question_infix', text='?'),
    Token('ExclamationMark', 'exclaim_postfix', text='!'),
    Token('Backslash', 'backslash', text='\\\\'),
    Token('StringInterpolationAnchor', 'string_interpolation_anchor',
          text=')'),
    Token('StringQuote', 'string_quote', text='\\\"'),
    Token('MultilineStringQuote', 'multiline_string_quote',
          text='\\\"\\\"\\\"'),
    Token('StringSegment', 'string_segment'),
    Token('Identifier', 'identifier'),
    Token('DollarIdentifier', 'dollarident'),
    Token('UnspacedBinaryOperator', 'oper_binary_unspaced'),
    Token('SpacedBinaryOperator', 'oper_binary_spaced'),
    Token('PrefixOperator', 'oper_prefix'),
    Token('PostfixOperator', 'oper_postfix'),
    Token('IntegerLiteral', 'integer_literal'),
    Token('FloatingLiteral', 'floating_literal'),
    Token('StringLiteral', 'string_literal'),
    Token('ContextualKeyword', 'contextual_keyword'),
    Token('Unknown', 'unknown'),
]

SYNTAX_TOKEN_MAP = {token.name + 'Token': token for token in SYNTAX_TOKENS}

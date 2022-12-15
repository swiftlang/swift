def lowercase_first_word(name):
    """
    Lowercases the first word in the provided camelCase or PascalCase string.
    EOF -> eof
    IfKeyword -> ifKeyword
    EOFToken -> eofToken
    """
    word_index = 0
    threshold_index = 1
    for c in name:
        if c.islower():
            if word_index > threshold_index:
                word_index -= 1
            break
        word_index += 1
    if word_index == 0:
        return name
    return name[:word_index].lower() + name[word_index:]


class Token(object):
    """
    Represents the specification for a Token in the TokenSyntax file.
    """

    def __init__(self, name, kind, name_for_diagnostics,
                 unprefixed_kind=None, text=None, classification='None',
                 is_keyword=False, requires_leading_space=False,
                 requires_trailing_space=False):
        self.name = name
        self.kind = kind
        if unprefixed_kind is None:
            self.unprefixed_kind = kind
        else:
            self.unprefixed_kind = unprefixed_kind
        self.name_for_diagnostics = name_for_diagnostics
        self.text = text
        self.is_keyword = is_keyword
        self.requires_leading_space = requires_leading_space
        self.requires_trailing_space = requires_trailing_space

    def swift_kind(self):
        name = lowercase_first_word(self.name)
        if self.is_keyword:
            return name + 'Keyword'
        return name


class Keyword(Token):
    """
    Represents a keyword token.
    """

    def __init__(self, name, text, classification='Keyword',
                 requires_leading_space=False, requires_trailing_space=True):
        Token.__init__(self,
                       name=name,
                       kind='kw_' + text,
                       name_for_diagnostics=text,
                       unprefixed_kind=text, text=text,
                       classification=classification, is_keyword=True,
                       requires_leading_space=requires_leading_space,
                       requires_trailing_space=requires_trailing_space)

    def macro_name(self):
        return "KEYWORD"


class SwiftKeyword(Keyword):
    def macro_name(self):
        return "SWIFT_KEYWORD"


class DeclKeyword(SwiftKeyword):
    def macro_name(self):
        return "DECL_KEYWORD"


class StmtKeyword(SwiftKeyword):
    def macro_name(self):
        return "STMT_KEYWORD"


class ExprKeyword(SwiftKeyword):
    def macro_name(self):
        return "EXPR_KEYWORD"


class PatternKeyword(SwiftKeyword):
    def macro_name(self):
        return "PAT_KEYWORD"


class SilKeyword(Keyword):
    def macro_name(self):
        return "SIL_KEYWORD"


class PoundKeyword(Token):
    def __init__(self, name, kind, text, name_for_diagnostics=None,
                 classification='Keyword'):
        if name_for_diagnostics is None:
            name_for_diagnostics = text
        Token.__init__(self,
                       name=name,
                       kind='pound_' + kind,
                       name_for_diagnostics=name_for_diagnostics,
                       unprefixed_kind=kind, text=text,
                       classification=classification, is_keyword=True,
                       requires_trailing_space=True)

    def macro_name(self):
        return "POUND_KEYWORD"


class PoundObjectLiteral(PoundKeyword):
    def __init__(self, name, kind, text, name_for_diagnostics,
                 protocol, classification='ObjectLiteral'):
        PoundKeyword.__init__(
            self,
            name=name,
            kind=kind,
            text=text,
            name_for_diagnostics=name_for_diagnostics,
            classification=classification
        )
        self.description = name_for_diagnostics
        self.protocol = protocol

    def macro_name(self):
        return "POUND_OBJECT_LITERAL"


class PoundConfig(PoundKeyword):
    def macro_name(self):
        return "POUND_CONFIG"


class PoundDirectiveKeyword(PoundKeyword):
    def __init__(self, name, kind, text,
                 classification='PoundDirectiveKeyword'):
        PoundKeyword.__init__(
            self,
            name=name,
            kind=kind,
            text=text,
            classification=classification
        )

    def macro_name(self):
        return "POUND_DIRECTIVE_KEYWORD"


class PoundConditionalDirectiveKeyword(PoundDirectiveKeyword):
    def __init__(self, name, kind, text,
                 classification='PoundDirectiveKeyword'):
        PoundKeyword.__init__(
            self,
            name=name,
            kind=kind,
            text=text,
            classification=classification
        )

    def macro_name(self):
        return "POUND_COND_DIRECTIVE_KEYWORD"


class Punctuator(Token):
    def __init__(self, name, kind, text, classification='None',
                 requires_leading_space=False, requires_trailing_space=False):
        Token.__init__(
            self,
            name=name,
            kind=kind,
            name_for_diagnostics=text,
            unprefixed_kind=None,
            text=text,
            classification=classification,
            is_keyword=False,
            requires_leading_space=requires_leading_space,
            requires_trailing_space=requires_trailing_space
        )

    def macro_name(self):
        return "PUNCTUATOR"


class Literal(Token):
    def macro_name(self):
        return "LITERAL"


class Misc(Token):
    def macro_name(self):
        return "MISC"


SYNTAX_TOKENS = [
    # Keywords that start decls
    DeclKeyword('Associatedtype', 'associatedtype'),
    DeclKeyword('Class', 'class'),
    DeclKeyword('Deinit', 'deinit'),
    DeclKeyword('Enum', 'enum'),
    DeclKeyword('Extension', 'extension'),
    DeclKeyword('Func', 'func'),
    DeclKeyword('Import', 'import'),
    DeclKeyword('Init', 'init'),
    DeclKeyword('Inout', 'inout'),
    DeclKeyword('Let', 'let'),
    DeclKeyword('Operator', 'operator'),
    DeclKeyword('Precedencegroup', 'precedencegroup'),
    DeclKeyword('Protocol', 'protocol'),
    DeclKeyword('Struct', 'struct'),
    DeclKeyword('Subscript', 'subscript'),
    DeclKeyword('Typealias', 'typealias'),
    DeclKeyword('Var', 'var'),

    DeclKeyword('Fileprivate', 'fileprivate'),
    DeclKeyword('Internal', 'internal'),
    DeclKeyword('Private', 'private'),
    DeclKeyword('Public', 'public'),
    DeclKeyword('Static', 'static'),

    # Statement keywords
    StmtKeyword('Defer', 'defer'),
    StmtKeyword('If', 'if'),
    StmtKeyword('Guard', 'guard'),
    StmtKeyword('Do', 'do', requires_trailing_space=False),
    StmtKeyword('Repeat', 'repeat'),
    StmtKeyword('Else', 'else'),
    StmtKeyword('For', 'for'),
    StmtKeyword('In', 'in'),
    StmtKeyword('While', 'while'),
    StmtKeyword('Return', 'return'),
    StmtKeyword('Break', 'break'),
    StmtKeyword('Continue', 'continue'),
    StmtKeyword('Fallthrough', 'fallthrough'),
    StmtKeyword('Switch', 'switch'),
    StmtKeyword('Case', 'case'),
    StmtKeyword('Default', 'default', requires_trailing_space=False),
    StmtKeyword('Where', 'where', requires_leading_space=True),
    StmtKeyword('Catch', 'catch', requires_leading_space=True,
                requires_trailing_space=False),
    StmtKeyword('Throw', 'throw'),

    # Expression keywords
    ExprKeyword('As', 'as'),
    ExprKeyword('Any', 'Any'),
    ExprKeyword('False', 'false', requires_trailing_space=False),
    ExprKeyword('Is', 'is'),
    ExprKeyword('Nil', 'nil', requires_trailing_space=False),
    ExprKeyword('Rethrows', 'rethrows'),
    ExprKeyword('Super', 'super', requires_trailing_space=False),
    ExprKeyword('Self', 'self', requires_trailing_space=False),
    ExprKeyword('CapitalSelf', 'Self', requires_trailing_space=False),
    ExprKeyword('True', 'true', requires_trailing_space=False),
    ExprKeyword('Try', 'try'),
    ExprKeyword('Throws', 'throws'),

    # Pattern keywords
    PatternKeyword('Wildcard', '_'),

    # Punctuators
    Punctuator('LeftParen', 'l_paren', text='('),
    Punctuator('RightParen', 'r_paren', text=')'),
    Punctuator('LeftBrace', 'l_brace', text='{', requires_leading_space=True),
    Punctuator('RightBrace', 'r_brace', text='}'),
    Punctuator('LeftSquareBracket', 'l_square', text='['),
    Punctuator('RightSquareBracket', 'r_square', text=']'),
    Punctuator('LeftAngle', 'l_angle', text='<', requires_leading_space=True,
               requires_trailing_space=True),
    Punctuator('RightAngle', 'r_angle', text='>', requires_leading_space=True,
               requires_trailing_space=True),

    Punctuator('Period', 'period', text='.'),
    Punctuator('PrefixPeriod', 'period_prefix', text='.'),
    Punctuator('Comma', 'comma', text=',', requires_trailing_space=True),
    Punctuator('Ellipsis', 'ellipsis', text='...'),
    Punctuator('Colon', 'colon', text=':', requires_trailing_space=True),
    Punctuator('Semicolon', 'semi', text=';'),
    Punctuator('Equal', 'equal', text='=', requires_leading_space=True,
               requires_trailing_space=True),
    Punctuator('AtSign', 'at_sign', text='@', classification='Attribute'),
    Punctuator('Pound', 'pound', text='#'),

    Punctuator('PrefixAmpersand', 'amp_prefix', text='&'),
    Punctuator('Arrow', 'arrow', text='->', requires_leading_space=True,
               requires_trailing_space=True),

    Punctuator('Backtick', 'backtick', text='`'),

    Punctuator('Backslash', 'backslash', text='\\\\'),

    Punctuator('ExclamationMark', 'exclaim_postfix', text='!'),

    Punctuator('PostfixQuestionMark', 'question_postfix', text='?'),
    Punctuator('InfixQuestionMark', 'question_infix', text='?'),

    Punctuator('StringQuote', 'string_quote', text='\\\"',
               classification='StringLiteral'),
    Punctuator('SingleQuote', 'single_quote', text='\\\'',
               classification='StringLiteral'),
    Punctuator('MultilineStringQuote', 'multiline_string_quote',
               text='\\\"\\\"\\\"', classification='StringLiteral'),

    # Keywords prefixed with a '#'.

    PoundKeyword('PoundKeyPath', 'keyPath', text='#keyPath'),
    PoundKeyword('PoundLine', 'line', text='#line'),
    PoundKeyword('PoundSelector', 'selector', text='#selector'),
    PoundKeyword('PoundFile', 'file', text='#file'),
    PoundKeyword('PoundFileID', 'fileID', text='#fileID'),
    PoundKeyword('PoundFilePath', 'filePath', text='#filePath'),
    PoundKeyword('PoundColumn', 'column', text='#column'),
    PoundKeyword('PoundFunction', 'function', text='#function'),
    PoundKeyword('PoundDsohandle', 'dsohandle', text='#dsohandle'),
    PoundKeyword('PoundAssert', 'assert', text='#assert'),

    PoundDirectiveKeyword('PoundSourceLocation', 'sourceLocation',
                          text='#sourceLocation'),
    PoundDirectiveKeyword('PoundWarning', 'warning', text='#warning'),
    PoundDirectiveKeyword('PoundError', 'error', text='#error'),

    PoundConditionalDirectiveKeyword('PoundIf', 'if', text='#if'),
    PoundConditionalDirectiveKeyword('PoundElse', 'else', text='#else'),
    PoundConditionalDirectiveKeyword('PoundElseif', 'elseif',
                                     text='#elseif'),
    PoundConditionalDirectiveKeyword('PoundEndif', 'endif',
                                     text='#endif'),

    PoundConfig('PoundAvailable', 'available', text='#available'),
    PoundConfig('PoundUnavailable', 'unavailable', text='#unavailable'),

    PoundObjectLiteral('PoundFileLiteral', 'fileLiteral',
                       text='#fileLiteral',
                       name_for_diagnostics='file reference',
                       protocol='ExpressibleByFileReferenceLiteral'),
    PoundObjectLiteral('PoundImageLiteral', 'imageLiteral',
                       text='#imageLiteral',
                       name_for_diagnostics='image',
                       protocol='ExpressibleByImageLiteral'),
    PoundObjectLiteral('PoundColorLiteral', 'colorLiteral',
                       text='#colorLiteral',
                       name_for_diagnostics='color',
                       protocol='ExpressibleByColorLiteral'),

    PoundConfig('PoundHasSymbol', '_hasSymbol', text='#_hasSymbol'),

    Literal('IntegerLiteral', 'integer_literal', name_for_diagnostics='integer literal',
            classification='IntegerLiteral'),
    Literal('FloatingLiteral', 'floating_literal',
            name_for_diagnostics='floating literal', classification='FloatingLiteral'),
    Literal('StringLiteral', 'string_literal', name_for_diagnostics='string literal',
            classification='StringLiteral'),
    Literal('RegexLiteral', 'regex_literal', name_for_diagnostics='regex literal'),

    Misc('Unknown', 'unknown', name_for_diagnostics='token'),
    Misc('Identifier', 'identifier', name_for_diagnostics='identifier',
         classification='Identifier'),
    Misc('UnspacedBinaryOperator', 'oper_binary_unspaced',
         name_for_diagnostics='binary operator',
         classification='OperatorIdentifier'),
    Misc('SpacedBinaryOperator', 'oper_binary_spaced',
         name_for_diagnostics='binary operator',
         classification='OperatorIdentifier',
         requires_leading_space=True, requires_trailing_space=True),
    Misc('PostfixOperator', 'oper_postfix', name_for_diagnostics='postfix operator',
         classification='OperatorIdentifier'),
    Misc('PrefixOperator', 'oper_prefix', name_for_diagnostics='prefix operator',
         classification='OperatorIdentifier'),
    Misc('DollarIdentifier', 'dollarident', name_for_diagnostics='dollar identifier',
         classification='DollarIdentifier'),

    Misc('ContextualKeyword', 'contextual_keyword', name_for_diagnostics='keyword',
         classification='Keyword'),
    Misc('RawStringDelimiter', 'raw_string_delimiter',
         name_for_diagnostics='raw string delimiter'),
    Misc('StringSegment', 'string_segment', name_for_diagnostics='string segment',
         classification='StringLiteral'),
    Misc('StringInterpolationAnchor', 'string_interpolation_anchor',
         name_for_diagnostics='string interpolation anchor',
         text=')', classification='StringInterpolationAnchor'),
    Misc('Yield', 'kw_yield', name_for_diagnostics='yield',
         text='yield'),

]

SYNTAX_TOKEN_MAP = {token.name + 'Token': token for token in SYNTAX_TOKENS}


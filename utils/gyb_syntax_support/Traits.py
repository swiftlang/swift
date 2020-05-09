from Child import Child


class Trait(object):
    def __init__(self, trait_name, description=None, children=None):
        self.trait_name = trait_name
        self.children = children
        self.description = description


TRAITS = [
    Trait('DeclGroup',
          children=[
              Child('Attributes', kind='AttributeList', is_optional=True),
              Child('Modifiers', kind='ModifierList', is_optional=True),
              Child('Members', kind='MemberDeclBlock'),
          ]),

    Trait('Braced',
          children=[
              Child('LeftBrace', kind='LeftBraceToken'),
              Child('RightBrace', kind='RightBraceToken'),
          ]),

    Trait('IdentifiedDecl',
          children=[
              Child('Identifier', kind='IdentifierToken'),
          ]),

    Trait('WithCodeBlock',
          children=[
              Child('Body', kind='CodeBlock'),
          ]),

    Trait('Parenthesized',
          children=[
              Child('LeftParen', kind='LeftParenToken'),
              Child('RightParen', kind='RightParenToken'),
          ]),

    Trait('WithTrailingComma',
          children=[
              Child('TrailingComma', kind='CommaToken', is_optional=True),
          ]),

    Trait('Labeled',
          children=[
              Child('LabelName', kind='IdentifierToken', is_optional=True),
              Child('LabelColon', kind='ColonToken', is_optional=True),
          ]),

    Trait('WithStatements',
          children=[
              Child('Statements', kind='CodeBlockItemList'),
          ]),
]

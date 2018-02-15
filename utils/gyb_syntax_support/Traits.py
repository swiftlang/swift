from Child import Child


class TRAIT(object):
    def __init__(self, trait_name, children):
        self.trait_name = trait_name
        self.children = children


TRAITS = [
    TRAIT('DeclGroup',
          children=[
              Child('Attributes', kind='AttributeList', is_optional=True),
              Child('AccessLevelModifier', kind='DeclModifier',
                    is_optional=True),
              Child('Members', kind='MemberDeclBlock'),
          ]),

    TRAIT('BracedSyntax',
          children=[
              Child('LeftBrace', kind='LeftBraceToken'),
              Child('RightBrace', kind='RightBraceToken'),
          ]),
]

from Child import Child


class Trait(object):
    def __init__(self, trait_name, children):
        self.trait_name = trait_name
        self.children = children


TRAITS = [
    Trait('DeclGroupSyntax',
          children=[
              Child('Attributes', kind='AttributeList', is_optional=True),
              Child('AccessLevelModifier', kind='DeclModifier',
                    is_optional=True),
              Child('Members', kind='MemberDeclBlock'),
          ]),

    Trait('BracedSyntax',
          children=[
              Child('LeftBrace', kind='LeftBraceToken'),
              Child('RightBrace', kind='RightBraceToken'),
          ]),
]

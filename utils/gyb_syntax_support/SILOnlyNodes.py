from .Child import Child
from .Node import Node  # noqa: I201

# These nodes are used only in SIL parsing.

SILONLY_NODES = [
    # generic-parameter-clause-list
    Node('GenericParameterClauseList', kind='SyntaxCollection',
         element='GenericParameterClause'),

    # sil-function-type -> generic-parameter-clause-list function-type
    Node('SILFunctionType', kind='Type',
         children=[
             Child('GenericParameterClauses',
                   kind='GenericParameterClauseList',
                   collection_element_name='GenericParameterClause',
                   is_optional=True),
             Child('SubstitutedAttrAtToken', kind='AtSignToken',
                   description='The `@` sign of the `@substituted` attribute',
                   is_optional=True),
             Child('SubstitutedAttrName', kind='IdentifierToken', 
                   text_choices=['substituted'],
                   is_optional=True,
                   description='''
                   The identifier of the `@substituted` attribute
                   '''),
             Child('PatternGenericParameterClauses',
                   kind='GenericParameterClauseList',
                   collection_element_name='PatternGenericParameterClause',
                   is_optional=True),
             Child('Function', kind='FunctionType'),
             Child('GenericSubstitution', kind='GenericSubstitution', 
                   is_optional=True),
             Child('GenericPatternSubstitution', kind='GenericSubstitution', 
                   is_optional=True),
         ]),

    # sil-box-type-field
    Node('SILBoxTypeField', kind='Syntax',
         children=[
             Child('Specifier', kind='Token',
                   token_choices=[
                       'LetToken',
                       'VarToken',
                   ]),
             Child('Type', kind='Type'),
             Child('TrailingComma', kind='CommaToken', is_optional=True),
         ]),
    Node('SILBoxTypeFieldList', kind='SyntaxCollection',
         element='SILBoxTypeField'),

    # sil-box-type -> generic-parameter-clause-list '{'
    Node('SILBoxType', kind='Type',
         children=[
             Child('GenericParameterClauses',
                   kind='GenericParameterClauseList',
                   collection_element_name='GenericParameterClause',
                   is_optional=True),
             Child('LeftBrace', kind='LeftBraceToken'),
             Child('Fields', kind='SILBoxTypeFieldList',
                   collection_element_name='Field'),
             Child('RightBrace', kind='RightBraceToken'),
             Child('GenericArgumentClause', kind='GenericArgumentClause',
                   is_optional=True),
         ]),

    Node('GenericSubstitution', kind='Syntax',
         children=[
             Child('ForKeyword', kind='ForToken'),
             Child('LeftAngleBracket', kind='LeftAngleToken'),
             Child('Substitutions', kind='GenericSubstitutionList', 
                   collection_element_name='Substitution'),
             Child('RightAngleBracket', kind='RightAngleToken'),
         ]),

    Node('GenericSubstitutionList', kind='SyntaxCollection',
         element='GenericSubstitutionElement',
         element_name='Substitution'),

    Node('GenericSubstitutionElement', kind='Syntax',
         traits=['WithTrailingComma'],
         children=[
             Child('Type', kind='Type'),
             Child('TrailingComma', kind='CommaToken',
                   is_optional=True),
         ]),
]

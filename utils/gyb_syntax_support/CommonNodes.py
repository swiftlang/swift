from .Child import Child
from .Node import Node  # noqa: I201

COMMON_NODES = [
    Node('Decl', name_for_diagnostics='declaration', kind='Syntax'),
    Node('Expr', name_for_diagnostics='expression', kind='Syntax'),
    Node('Stmt', name_for_diagnostics='statement', kind='Syntax'),
    Node('Type', name_for_diagnostics='type', kind='Syntax'),
    Node('Pattern', name_for_diagnostics='pattern', kind='Syntax'),
    Node('UnknownDecl', name_for_diagnostics='declaration', kind='Decl'),
    Node('UnknownExpr', name_for_diagnostics='expression', kind='Expr'),
    Node('UnknownStmt', name_for_diagnostics='statement', kind='Stmt'),
    Node('UnknownType', name_for_diagnostics='type', kind='Type'),
    Node('UnknownPattern', name_for_diagnostics='pattern', kind='Pattern'),
    Node('Missing', name_for_diagnostics=None, kind='Syntax'),
    Node('MissingDecl', name_for_diagnostics='declaration', kind='Decl', children=[
        Child('Attributes', kind='AttributeList',
              collection_element_name='Attribute', is_optional=True),
        Child('Modifiers', kind='ModifierList',
              collection_element_name='Modifier', is_optional=True),
    ]),
    Node('MissingExpr', name_for_diagnostics='expression', kind='Expr'),
    Node('MissingStmt', name_for_diagnostics='statement', kind='Stmt'),
    Node('MissingType', name_for_diagnostics='type', kind='Type'),
    Node('MissingPattern', name_for_diagnostics='pattern', kind='Pattern'),

    # code-block-item = (decl | stmt | expr) ';'?
    Node('CodeBlockItem', name_for_diagnostics=None, kind='Syntax',
         omit_when_empty=True,
         description="""
         A CodeBlockItem is any Syntax node that appears on its own line inside
         a CodeBlock.
         """,
         children=[
             Child('Item', kind='Syntax',
                   description="The underlying node inside the code block.",
                   node_choices=[
                       Child('Decl', kind='Decl'),
                       Child('Stmt', kind='Stmt'),
                       Child('Expr', kind='Expr'),
                       Child('TokenList', kind='TokenList'),
                       Child('NonEmptyTokenList', kind='NonEmptyTokenList'),
                   ]),
             Child('Semicolon', kind='SemicolonToken',
                   description="""
                   If present, the trailing semicolon at the end of the item.
                   """,
                   is_optional=True),
             Child('ErrorTokens', kind='Syntax', is_optional=True),
         ]),

    # code-block-item-list -> code-block-item code-block-item-list?
    Node('CodeBlockItemList', name_for_diagnostics=None,
         kind='SyntaxCollection', element='CodeBlockItem',
         elements_separated_by_newline=True),

    # code-block -> '{' stmt-list '}'
    Node('CodeBlock', name_for_diagnostics=None, kind='Syntax',
         traits=['Braced', 'WithStatements'],
         children=[
             Child('LeftBrace', kind='LeftBraceToken'),
             Child('Statements', kind='CodeBlockItemList',
                   collection_element_name='Statement', is_indented=True),
             Child('RightBrace', kind='RightBraceToken',
                   requires_leading_newline=True),
         ]),

    Node('UnexpectedNodes', name_for_diagnostics=None, kind='SyntaxCollection',
         element='Syntax',
         description='''
         A collection of syntax nodes that occurred in the source code but
         could not be used to form a valid syntax tree.
         '''),
]

from Child import Child
from Node import Node  # noqa: I201

COMMON_NODES = [
    Node('Decl', kind='Syntax'),
    Node('Expr', kind='Syntax'),
    Node('Stmt', kind='Syntax'),
    Node('Type', kind='Syntax'),
    Node('Pattern', kind='Syntax'),
    Node('UnknownDecl', kind='Decl'),
    Node('UnknownExpr', kind='Expr'),
    Node('UnknownStmt', kind='Stmt'),
    Node('UnknownType', kind='Type'),
    Node('UnknownPattern', kind='Pattern'),

    # code-block-item = (decl | stmt | expr) ';'?
    Node('CodeBlockItem', kind='Syntax', omit_when_empty=True,
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
    Node('CodeBlockItemList', kind='SyntaxCollection',
         element='CodeBlockItem'),

    # code-block -> '{' stmt-list '}'
    Node('CodeBlock', kind='Syntax',
         traits=['Braced', 'WithStatements'],
         children=[
             Child('LeftBrace', kind='LeftBraceToken'),
             Child('Statements', kind='CodeBlockItemList'),
             Child('RightBrace', kind='RightBraceToken'),
         ]),
]

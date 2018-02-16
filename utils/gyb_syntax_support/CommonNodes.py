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
    Node('CodeBlockItem', kind='Syntax',
         children=[
             Child('Item', kind='Syntax',
                   node_choices=[
                       Child('Decl', kind='Decl'),
                       Child('Stmt', kind='Stmt'),
                       Child('Expr', kind='Expr'),
                   ]),
             Child('Semicolon', kind='SemicolonToken',
                   is_optional=True),
         ]),

    # code-block-item-list -> code-block-item code-block-item-list?
    Node('CodeBlockItemList', kind='SyntaxCollection',
         element='CodeBlockItem'),

    # code-block -> '{' stmt-list '}'
    Node('CodeBlock', kind='Syntax',
         traits=['BracedSyntax'],
         children=[
             Child('LeftBrace', kind='LeftBraceToken'),
             Child('Statements', kind='CodeBlockItemList'),
             Child('RightBrace', kind='RightBraceToken'),
         ]),
]

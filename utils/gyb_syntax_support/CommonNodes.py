from Node import Node

COMMON_NODES = [
    Node('Decl', kind='Syntax'),
    Node('UnknownDecl', kind='Decl'),

    Node('Expr', kind='Syntax'),
    Node('UnknownExpr', kind='Expr'),

    Node('Stmt', kind='Syntax'),
    Node('UnknownStmt', kind='Stmt'),

    Node('Type', kind='Syntax'),
    Node('UnknownType', kind='Type'),

    Node('Pattern', kind='Syntax'),
    Node('UnknownPattern', kind='Pattern'),
]

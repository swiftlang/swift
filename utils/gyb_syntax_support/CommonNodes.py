from Node import Node

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
]

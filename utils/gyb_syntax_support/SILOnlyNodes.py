from Node import Node  # noqa: I201

# These nodes are used only in SIL parsing.

SILONLY_NODES = [
    # generic-parameter-clause-list
    Node('GenericParameterClauseList', kind='SyntaxCollection',
         element='GenericParameterClause'),
]

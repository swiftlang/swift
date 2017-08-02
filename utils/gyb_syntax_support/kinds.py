"""
All the known base syntax kinds. These will all be considered non-final classes
and other types will be allowed to inherit from them.
"""
SYNTAX_BASE_KINDS = ['Decl', 'Expr', 'Pattern', 'Stmt',
                     'Syntax', 'SyntaxCollection', 'Type']


def kind_to_type(kind):
    """
    Converts a SyntaxKind to a type name, checking to see if the kind is
    Syntax or SyntaxCollection first.
    A type name is the same as the SyntaxKind name with the suffix "Syntax"
    added.
    """
    if kind in ["Syntax", "SyntaxCollection"]:
        return kind
    if kind.endswith("Token"):
        return "TokenSyntax"
    return kind + "Syntax"

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


def lowercase_first_word(name):
    """
    Lowercases the first word in the provided camelCase or PascalCase string.
    EOF -> eof
    IfKeyword -> ifKeyword
    EOFToken -> eofToken
    """
    word_index = 0
    threshold_index = 1
    for c in name:
        if c.islower():
            if word_index > threshold_index:
                word_index -= 1
            break
        word_index += 1
    if word_index == 0:
        return name
    return name[:word_index].lower() + name[word_index:]

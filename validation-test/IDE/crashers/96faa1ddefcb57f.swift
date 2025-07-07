// {"kind":"complete","signature":"swift::Demangle::ASTBuilder::findTypeDecl(swift::DeclContext*, swift::Identifier, swift::Identifier, swift::Demangle::Node::Kind)"}
// Actual signature: openTypeParameter
// RUN: not --crash %target-swift-ide-test -code-completion --code-completion-token=COMPLETE -source-filename %s
struct a<b:RangeReplaceableCollection where b ={ c: b.Element? { #^COMPLETE^#

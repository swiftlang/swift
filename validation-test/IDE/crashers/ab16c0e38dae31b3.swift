// {"kind":"complete","signature":"swift::Mangle::ASTMangler::appendType(swift::Type, swift::GenericSignature, swift::ValueDecl const*)"}
// RUN: not --crash %target-swift-ide-test -code-completion --code-completion-token=COMPLETE -source-filename %s
func a<let b
#^COMPLETE^#

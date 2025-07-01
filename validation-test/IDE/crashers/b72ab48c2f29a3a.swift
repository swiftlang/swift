// {"kind":"complete","signature":"swift::rewriting::RequirementMachine::getReducedShape(swift::Type, llvm::ArrayRef<swift::GenericTypeParamType*>) const"}
// RUN: not --crash %target-swift-ide-test -code-completion --code-completion-token=COMPLETE -source-filename %s
class a<b{ c { struct d<f where f.e#^COMPLETE^#

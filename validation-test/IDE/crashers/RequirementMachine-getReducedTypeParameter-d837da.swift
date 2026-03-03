// {"kind":"complete","original":"ee63e244","signature":"swift::rewriting::RequirementMachine::getReducedTypeParameter(swift::CanType, llvm::ArrayRef<swift::GenericTypeParamType*>) const"}
// RUN: not --crash %target-swift-ide-test -code-completion -batch-code-completion -skip-filecheck -code-completion-diagnostics -source-filename %s
protocol a {
  associatedtype b
  struct c: a {
    #^^#
  }
    d(b )
  #e {
    d

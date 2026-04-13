// {"kind":"complete","original":"f14f150c","signature":"swift::Requirement::checkRequirement(llvm::SmallVectorImpl<swift::Requirement>&, bool, llvm::SmallVectorImpl<swift::ProtocolConformanceRef>*) const","signatureNext":"desugarRequirement"}
// RUN: not --crash %target-swift-ide-test -code-completion -batch-code-completion -skip-filecheck -code-completion-diagnostics -source-filename %s
protocol a
#^^#{
associatedtype b where b == Self }
protocol c: a struct d<e extension d: a where b == {
#^f^# }
extension d: c where e: c, e.b == d

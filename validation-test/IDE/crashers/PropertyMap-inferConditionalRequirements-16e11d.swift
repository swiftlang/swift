// {"kind":"complete","original":"f14f150c","signature":"swift::rewriting::PropertyMap::inferConditionalRequirements(swift::ProtocolConformance*, llvm::ArrayRef<swift::rewriting::Term>) const","signatureNext":"PropertyMap::concretizeNestedTypesFromConcreteParent"}
// RUN: not --crash %target-swift-ide-test -code-completion -batch-code-completion -skip-filecheck -code-completion-diagnostics -source-filename %s
protocol a
#^^#{
associatedtype b where b == Self }
protocol c: a struct d<e extension d: a where b == {
#^f^# }
extension d: c where e: c, e.b == d

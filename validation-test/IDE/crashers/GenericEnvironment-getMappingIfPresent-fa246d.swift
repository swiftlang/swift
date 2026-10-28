// {"kind":"complete","original":"f5caad07","signature":"swift::GenericEnvironment::getMappingIfPresent(swift::CanType) const","signatureAssert":"Assertion failed: (Index < Length && \"Invalid index!\"), function operator[]","signatureNext":"GenericEnvironment::getOrCreateArchetypeFromInterfaceType"}
// RUN: not --crash %target-swift-ide-test -code-completion -batch-code-completion -skip-filecheck -code-completion-diagnostics -source-filename %s
class a < b {
  @a(#^^# c struct d<b

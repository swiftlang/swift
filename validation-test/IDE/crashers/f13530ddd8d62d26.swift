// {"kind":"complete","signature":"swift::NominalTypeDecl::prepareConformanceTable() const","signatureAssert":"Assertion failed: (!isa<ProtocolDecl>(this) && \"Protocols don't have a conformance table\"), function prepareConformanceTable"}
// RUN: not --crash %target-swift-ide-test -code-completion -batch-code-completion -skip-filecheck -code-completion-diagnostics -source-filename %s
protocol a< b {
  associatedtype b
  func c<d>: a<d>#^COMPLETE^#

// {"kind":"complete","original":"b18a6ce5","signature":"swift::NormalProtocolConformance::getAssociatedConformance(swift::Type, swift::ProtocolDecl*) const","signatureAssert":"Assertion failed: (!AssociatedConformances[index]), function setAssociatedConformance","signatureNext":"ProtocolConformance::getAssociatedConformance"}
// RUN: not --crash %target-swift-ide-test -code-completion -batch-code-completion -skip-filecheck -code-completion-diagnostics -source-filename %s
protocol a {
  associatedtype b: c
  associatedtype d: Sequence where e == b.f
  subscript(d) d.Element
  protocol c {
    associatedtype f
  }
  struct g<h>: a {
    typealias b = i<g>#^^#
    subscript(ContiguousArray<h>) h
    struct i<j: a>: c {
      typealias k = j
      typealias f = k.d.Element

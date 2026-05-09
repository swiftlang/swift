// {"kind":"complete","original":"86a422f9","signature":"swift::InFlightSubstitution::lookupConformance(swift::Type, swift::ProtocolDecl*, unsigned int)","signatureAssert":"Assertion failed: (index < substPackPatterns.size() && \"replacement for pack parameter did not have the right \" \"size for expansion\"), function projectLaneFromPackConformance","signatureNext":"TypeSubstituter::transformDependentMemberType"}
// RUN: not --crash %target-swift-ide-test -code-completion -batch-code-completion -skip-filecheck -code-completion-diagnostics -source-filename %s
protocol a {
  associatedtype b
}
struct c<each d {
    e<
      each h: a
    >(repeat each h.b)
  where
    (
      repeat each h.f
    ) == (
      repeat each d
  func
    g
  :
    c<String, Int>#^^#

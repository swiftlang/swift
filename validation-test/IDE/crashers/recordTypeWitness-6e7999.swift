// {"kind":"complete","original":"0e0e1b1c","signature":"recordTypeWitness(swift::NormalProtocolConformance*, swift::AssociatedTypeDecl*, swift::Type, swift::TypeDecl*)","signatureAssert":"Assertion failed: (conformance->getTypeWitnessUncached(assocType) .getWitnessType() ->isEqual(type) && \"Conflicting type witness deductions\"), function recordTypeWitness","signatureNext":"resolveTypeWitnessViaLookup"}
// RUN: not --crash %target-swift-ide-test -code-completion -batch-code-completion -skip-filecheck -code-completion-diagnostics -source-filename %s
protocol a {
  associatedtype b: c
  associatedtype d: Self
  r(inout d)
}
protocol c {
  associatedtype e
}
protocol f {
  associatedtype g
}
protocol h: a where d: i {
  associatedtype q = d.g
  associatedtype b = j<Self, Self>
  associatedtype d = k<Self>
}
protocol l: c
  protocol i: f
    struct j<m, n>: l
      struct k<o: h>: i {
        typealias g = o.b.e
        struct p: h {
#^^#
          func r(_ : inout Int

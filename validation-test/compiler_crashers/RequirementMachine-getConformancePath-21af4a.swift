// {"kind":"typecheck","original":"49417267","signature":"swift::rewriting::RequirementMachine::getConformancePath(swift::Type, swift::ProtocolDecl*)","signatureNext":"SubstitutionMap::lookupConformance"}
// RUN: not --crash %target-swift-frontend -typecheck %s
protocol a {
  associatedtype b
}
protocol c {
  associatedtype Dim1
  associatedtype d: a
  associatedtype
    e: c
  where
    Dim1 == d.b,
    e.d == Dim1
}
protocol f {
  associatedtype g: c
  struct h<i: f>: f {
    typealias g = i.g
  }
}

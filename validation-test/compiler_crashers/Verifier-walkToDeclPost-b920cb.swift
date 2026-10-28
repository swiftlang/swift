// {"kind":"typecheck","signature":"(anonymous namespace)::Verifier::walkToDeclPost(swift::Decl*)","signatureNext":"Traversal::doIt"}
// RUN: not --crash %target-swift-frontend -typecheck %s
protocol a {
  associatedtype b
}
struct c<d: a> {
  typealias b = d.b
}
protocol e {
  typealias f = c<String>.b
}

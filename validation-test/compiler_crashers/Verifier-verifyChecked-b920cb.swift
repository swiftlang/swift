// {"kind":"typecheck","signature":"(anonymous namespace)::Verifier::verifyChecked(swift::ValueDecl*)"}
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

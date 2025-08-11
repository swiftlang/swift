// {"kind":"typecheck","signature":"(anonymous namespace)::TypePrinter::printArchetypeCommon(swift::Type, swift::GenericEnvironment*)"}
// RUN: not --crash %target-swift-frontend -typecheck %s
protocol a: Collection where Element == b.Element! {
  associatedtype b: Collection
}
protocol c {
  associatedtype d: a where d.b: a, d.Element == Self
}
func e () {
  func f<e : c>() {
    e.g
  }
}

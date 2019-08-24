// RUN: %target-typecheck-verify-swift

struct CustomString<T: _ExpressibleByBuiltinStringLiteral>
  : ExpressibleByStringLiteral {
  init(stringLiteral value: T) {}
}
struct CustomExtendedGraphemeCluster
  <T: _ExpressibleByBuiltinExtendedGraphemeClusterLiteral>
  : ExpressibleByExtendedGraphemeClusterLiteral {
  init(extendedGraphemeClusterLiteral value: T) {}
}
struct CustomUnicodeScalar<T: _ExpressibleByBuiltinUnicodeScalarLiteral>
  : ExpressibleByUnicodeScalarLiteral {
  init(unicodeScalarLiteral value: T) {
  }
}

func verify<T>(stringLiteral value: CustomString<T>, with: T.Type) {
}
func verify<T>(
  extendedGraphemeClusterLiteral value: CustomExtendedGraphemeCluster<T>,
  with: T.Type) {
}
func verify<T>(
  unicodeScalarLiteral value: CustomUnicodeScalar<T>, 
  with: T.Type) {
}

verify(unicodeScalarLiteral: "å", with: UnicodeScalar.self)
verify(unicodeScalarLiteral: "ß", with: Character.self)
verify(unicodeScalarLiteral: "c", with: String.self)
verify(unicodeScalarLiteral: "∂", with: StaticString.self)

verify(extendedGraphemeClusterLiteral: "a", with: Character.self)
verify(extendedGraphemeClusterLiteral: "❄︎", with: String.self)
verify(extendedGraphemeClusterLiteral: "김", with: StaticString.self)

verify(stringLiteral: "abc", with: String.self)
verify(stringLiteral: "∂éƒg", with: StaticString.self)

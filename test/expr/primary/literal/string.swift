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

verify(unicodeScalarLiteral: "å", with: UnicodeScalar.self) // expected-warning {{double quotes deprecated in favour of single quotes to express 'CustomUnicodeScalar<UnicodeScalar>' (aka 'CustomUnicodeScalar<Unicode.Scalar>')}}
verify(unicodeScalarLiteral: "ß", with: Character.self) // expected-warning {{double quotes deprecated in favour of single quotes to express 'CustomUnicodeScalar<Character>'}}
verify(unicodeScalarLiteral: "c", with: String.self) // expected-warning {{double quotes deprecated in favour of single quotes to express 'CustomUnicodeScalar<String>'}}
verify(unicodeScalarLiteral: "∂", with: StaticString.self) // expected-warning {{double quotes deprecated in favour of single quotes to express 'CustomUnicodeScalar<StaticString>'}}

verify(extendedGraphemeClusterLiteral: "a", with: Character.self) // expected-warning {{double quotes deprecated in favour of single quotes to express 'CustomExtendedGraphemeCluster<Character>'}}
verify(extendedGraphemeClusterLiteral: "❄︎", with: String.self) // expected-warning {{double quotes deprecated in favour of single quotes to express 'CustomExtendedGraphemeCluster<String>'}}
verify(extendedGraphemeClusterLiteral: "김", with: StaticString.self) // expected-warning {{double quotes deprecated in favour of single quotes to express 'CustomExtendedGraphemeCluster<StaticString>'}}

verify(stringLiteral: "abc", with: String.self)
verify(stringLiteral: "∂éƒg", with: StaticString.self)

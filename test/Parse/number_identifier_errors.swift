// RUN: %target-typecheck-verify-swift

// Per rdar://problem/32316666 , it is a common mistake for beginners
// to start a function name with a number, so it's worth
// special-casing the diagnostic to make it clearer.

func 1() {}
// expected-error@-1 {{function name can only start with a letter or underscore, not a number}}
func 2.0() {}
// expected-error@-1 {{function name can only start with a letter or underscore, not a number}}
func 3func() {}
// expected-error@-1 {{function name can only start with a letter or underscore, not a number}}
// expected-error@-2 {{'f' is not a valid digit in integer literal}}

protocol 4 {
  // expected-error@-1 {{protocol name can only start with a letter or underscore, not a number}}
  associatedtype 5
  // expected-error@-1 {{associatedtype name can only start with a letter or underscore, not a number}}
}
protocol 6.0 {
  // expected-error@-1 {{protocol name can only start with a letter or underscore, not a number}}
  associatedtype 7.0
  // expected-error@-1 {{associatedtype name can only start with a letter or underscore, not a number}}
}
protocol 8protocol {
  // expected-error@-1 {{protocol name can only start with a letter or underscore, not a number}}
  // expected-error@-2 {{'p' is not a valid digit in integer literal}}
  associatedtype 9associatedtype
  // expected-error@-1 {{associatedtype name can only start with a letter or underscore, not a number}}
  // expected-error@-2 {{'a' is not a valid digit in integer literal}}
}

typealias 10 = Int
// expected-error@-1 {{typealias name can only start with a letter or underscore, not a number}}
typealias 11.0 = Int
// expected-error@-1 {{typealias name can only start with a letter or underscore, not a number}}
typealias 12typealias = Int
// expected-error@-1 {{typealias name can only start with a letter or underscore, not a number}}
// expected-error@-2 {{'t' is not a valid digit in integer literal}}

struct 13 {}
// expected-error@-1 {{struct name can only start with a letter or underscore, not a number}}
struct 14.0 {}
// expected-error@-1 {{struct name can only start with a letter or underscore, not a number}}
struct 15struct {}
// expected-error@-1 {{struct name can only start with a letter or underscore, not a number}}
// expected-error@-2 {{'s' is not a valid digit in integer literal}}

enum 16 {}
// expected-error@-1 {{enum name can only start with a letter or underscore, not a number}}
enum 17.0 {}
// expected-error@-1 {{enum name can only start with a letter or underscore, not a number}}
enum 18enum {}
// expected-error@-1 {{enum name can only start with a letter or underscore, not a number}}
// expected-error@-2 {{'n' is not a valid digit in floating point exponent}}

class 19 {
  // expected-error@-1 {{class name can only start with a letter or underscore, not a number}}
  func 20() {}
  // expected-error@-1 {{function name can only start with a letter or underscore, not a number}}
}
class 21.0 {
  // expected-error@-1 {{class name can only start with a letter or underscore, not a number}}
  func 22.0() {}
  // expected-error@-1 {{function name can only start with a letter or underscore, not a number}}
}

class 23class {
  // expected-error@-1 {{class name can only start with a letter or underscore, not a number}}
  // expected-error@-2 {{'c' is not a valid digit in integer literal}}
  func 24method() {}
  // expected-error@-1 {{function name can only start with a letter or underscore, not a number}}
  // expected-error@-2 {{'m' is not a valid digit in integer literal}}
}

// RUN: %swift -parse -verify %s

enum Foo {
  case A, B
}

if Foo.A == .B { }
var aHash: Int = Foo.A.hashValue()

enum Generic<T> {
  case A, B

  func method() -> Int {
    if A == B { }
    return A.hashValue()
  }
}

if Generic<Foo>.A == .B { }
var gaHash: Int = Generic<Foo>.A.hashValue()

func localEnum() -> Bool {
  enum Local {
    case A, B
  }

  return Local.A == .B
}

enum CustomHashable {
  case A, B

  func hashValue() -> Int { return 0 }
}
func ==(x: CustomHashable, y: CustomHashable) -> Bool {
  return true
}

if CustomHashable.A == .B { }
var custHash: Int = CustomHashable.A.hashValue()

// We still synthesize conforming overloads of '==' and 'hashValue' if
// explicit definitions don't satisfy the protocol requirements. Probably
// not what we actually want. 
enum InvalidCustomHashable {
  case A, B

  func hashValue() -> String { return "" }
}
func ==(x: InvalidCustomHashable, y: InvalidCustomHashable) -> String {
  return ""
}
if InvalidCustomHashable.A == .B { }
var s: String = InvalidCustomHashable.A == .B
s = InvalidCustomHashable.A.hashValue()
var i: Int = InvalidCustomHashable.A.hashValue()

// Complex enums are not implicitly Equatable or Hashable.
enum Complex {
  case A(Int)
  case B
}

if Complex.A(1) == .B { } // expected-error{{does not type-check}}

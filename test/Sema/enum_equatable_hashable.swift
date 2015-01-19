// RUN: rm -rf %t && mkdir -p %t
// RUN: cp %s %t/main.swift
// RUN: %target-swift-frontend -parse -verify -primary-file %t/main.swift %S/Inputs/enum_equatable_hashable_other.swift

enum Foo {
  case A, B
}

if Foo.A == .B { }
var aHash: Int = Foo.A.hashValue

enum Generic<T> {
  case A, B

  func method() -> Int {
    if A == B { }
    return A.hashValue
  }
}

if Generic<Foo>.A == .B { }
var gaHash: Int = Generic<Foo>.A.hashValue

func localEnum() -> Bool {
  enum Local {
    case A, B
  }

  return Local.A == .B
}

enum CustomHashable {
  case A, B

  var hashValue: Int { return 0 }
}
func ==(x: CustomHashable, y: CustomHashable) -> Bool {
  return true
}

if CustomHashable.A == .B { }
var custHash: Int = CustomHashable.A.hashValue

// We still synthesize conforming overloads of '==' and 'hashValue' if
// explicit definitions don't satisfy the protocol requirements. Probably
// not what we actually want. 
enum InvalidCustomHashable {
  case A, B

  var hashValue: String { return "" } // expected-note{{previously declared here}}
}
func ==(x: InvalidCustomHashable, y: InvalidCustomHashable) -> String {
  return ""
}
if InvalidCustomHashable.A == .B { }
var s: String = InvalidCustomHashable.A == .B // expected-error {{could not find member 'B'}}
s = InvalidCustomHashable.A.hashValue
var i: Int = InvalidCustomHashable.A.hashValue // expected-error{{'String' is not convertible to 'Int'}}


// Check use of an enum's synthesized members before the enum is actually declared.
struct UseEnumBeforeDeclaration {
  let eqValue = EnumToUseBeforeDeclaration.A == .A
  let hashValue = EnumToUseBeforeDeclaration.A.hashValue
}
enum EnumToUseBeforeDeclaration {
  case A
}

// Check enums from another file in the same module.
if FromOtherFile.A == .A {}
let _: Int = FromOtherFile.A.hashValue

func getFromOtherFile() -> AlsoFromOtherFile { return .A }
if .A == getFromOtherFile() {}

// FIXME: This should work.
func overloadFromOtherFile() -> YetAnotherFromOtherFile { return .A }
func overloadFromOtherFile() -> Bool { return false }
if .A == overloadFromOtherFile() {} // expected-error {{could not find member 'A'}}


// Complex enums are not implicitly Equatable or Hashable.
enum Complex {
  case A(Int)
  case B
}

if Complex.A(1) == .B { } // expected-error{{could not find member 'B'}}

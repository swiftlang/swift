// RUN: %target-typecheck-verify-swift

protocol P {
  static var foo: Int { get }
}

let _: KeyPath<P.Type, P.Type> = \.self

func ordinaryLookup(_ type: P.Type) -> Int {
  type.foo
}

let kp: KeyPath<P.Type, Int> = \.foo

func takesKeyPath(_ kp: KeyPath<P.Type, Int>) {}

takesKeyPath(\.foo)

protocol Scope {
  static var keys: [Int] { get }
  static var counter: Int { get set }
  static subscript(i: Int) -> Int { get }
}
extension Scope {
  static var tag: Int { 0 }
}
protocol StaticY {
  static var y: Int { get }
}

let _: KeyPath<any Scope.Type, [Int]> = \.keys
let _: KeyPath<any Scope.Type, Int> = \.keys.count
let _: KeyPath<any Scope.Type, Int> = \.tag
let _: KeyPath<any Scope.Type, Int> = \.[0]

let _: ReferenceWritableKeyPath<any Scope.Type, Int> = \.counter
let _: PartialKeyPath<any Scope.Type> = \.keys
let _: (any Scope.Type) -> [Int] = \.keys
let _: KeyPath<any (Scope & StaticY).Type, Int> = \.y

let _: KeyPath<any Scope.Type, Int> = \.keys
// expected-error@-1 {{cannot assign value of type 'KeyPath<any Scope.Type, [Int]>' to type 'KeyPath<any Scope.Type, Int>'}}
// expected-note@-2 {{arguments to generic parameter 'Value' ('[Int]' and 'Int') are expected to be equal}}
let _: KeyPath<any Scope, [Int]> = \.keys
// expected-error@-1 {{static member 'keys' cannot be used on instance of type 'any Scope'}}

class Base {
  static let staticN = 42
  static var staticMutable = 0
  class var overridable: Int { 1 }
}
protocol Derived: Base {
  static var k: Int { get }
}
protocol Marker {}

let _: KeyPath<any Derived.Type, Int> = \.staticN
let _: ReferenceWritableKeyPath<any Derived.Type, Int> = \.staticMutable
let _: KeyPath<any Derived.Type, Int> = \.overridable
let _: KeyPath<any Derived.Type, Int> = \.k
let _: KeyPath<any (Base & Marker).Type, Int> = \.staticN
let _: KeyPath<any (Base & Marker).Type, Int> = \.overridable

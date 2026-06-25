// RUN: %target-typecheck-verify-swift

protocol P {
  static var foo: Int { get }
}

let _: KeyPath<P.Type, P.Type> = \.self

func ordinaryLookup(_ type: P.Type) -> Int {
  type.foo
}

let kp: KeyPath<P.Type, Int> = \.foo
// expected-error@-1 {{key path cannot refer to static member 'foo' of protocol type 'P'}}

func takesKeyPath(_ kp: KeyPath<P.Type, Int>) {}

takesKeyPath(\.foo)
// expected-error@-1 {{key path cannot refer to static member 'foo' of protocol type 'P'}}

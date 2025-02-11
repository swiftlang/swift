// RUN: %target-typecheck-verify-swift -target %target-swift-5.9-abi-triple -enable-upcoming-feature ExistentialAny

// REQUIRES: swift_feature_ExistentialAny

do {
  protocol P {
    typealias PAlias = P
    
    func f() -> any PAlias
    func g<T>(_: T) -> any PAlias
  }
}
do {
  protocol P {
    associatedtype A
    
    func f1() -> any A
    // expected-error@-1:18 {{'any' has no effect on type parameter 'Self.A'}}
    
    typealias Alias = A
    
    func f2() -> any Alias
    // expected-error@-1:18 {{'any' has no effect on type parameter 'Self.Alias' (aka 'Self.A')}}
  }
}
do {
  protocol P {
    associatedtype A where A == Int
    
    func f1() -> any A
    // expected-error@-1:18 {{'any' has no effect on concrete type 'Int'}}
    func g1<T>(_: T) -> any A
    // expected-error@-1:25 {{'any' has no effect on concrete type 'Int'}}
    
    typealias Alias = A
    
    func f2() -> any Alias
    // expected-error@-1:18 {{'any' has no effect on concrete type 'Self.Alias' (aka 'Int')}}
    func g2<T>(_: T) -> any Alias
    // expected-error@-1:25 {{'any' has no effect on concrete type 'Self.Alias' (aka 'Int')}}
  }
}
do {
  protocol P {
    associatedtype A where A == Invalid
    // expected-error@-1 {{cannot find type 'Invalid' in scope}}
    
    func f() -> any A
  }
}

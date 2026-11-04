// RUN: %target-typecheck-verify-swift  -target %target-swift-5.1-abi-triple

// REQUIRES: concurrency

actor MyActor { }

class MyActorSubclass1: MyActor { }
// expected-error@-1{{actor types do not support inheritance}}
// expected-error@-2{{type 'MyActorSubclass1' cannot conform to the 'Actor' protocol}}
// expected-warning@-3 {{non-final class 'MyActorSubclass1' cannot conform to the 'Sendable' protocol; this is an error in the Swift 6 language mode}}

actor MyActorSubclass2: MyActor { } // expected-error{{actor types do not support inheritance}}

// expected-error@+1{{keyword 'class' cannot be used as an identifier here}}
actor class MyActorClass { } // expected-astgen-error {{unexpected code 'MyActorClass' in actor}}

class NonActor { }

actor NonActorSubclass : NonActor { } // expected-error{{actor types do not support inheritance}}

// expected-error@+1{{keyword 'class' cannot be used as an identifier here}}
public actor class BobHope {} // expected-astgen-error {{unexpected code 'BobHope' in actor}}
// expected-error@+1{{keyword 'public' cannot be used as an identifier here}}
actor public class BarbraStreisand {} // expected-astgen-error {{expected '{' in actor}}
// expected-error@+1{{keyword 'struct' cannot be used as an identifier here}}
public actor struct JulieAndrews {} // expected-astgen-error {{unexpected code 'JulieAndrews' in actor}}
// expected-error@+1{{keyword 'public' cannot be used as an identifier here}}
actor public enum TomHanks {} // expected-astgen-error {{expected '{' in actor}}

open actor A1 { } // expected-cparser-error{{only classes and overridable class members can be declared 'open'; use 'public'}}

actor A2 {
  required init() { } // expected-cparser-error{{'required' initializer in non-class type 'A2'}}
  open func f() { } // expected-cparser-error{{only classes and overridable class members can be declared 'open'; use 'public'}}

  final func g() { } // okay for now
  class func h() { } // expected-cparser-error{{class methods are only allowed within classes; use 'static' to declare a static method}}
  static func i() { } // okay

  class var someProp: Int { 0 } // expected-cparser-error{{class properties are only allowed within classes; use 'static' to declare a static property}}
}

extension A2 {
  class func h2() { } // expected-cparser-error{{class methods are only allowed within classes; use 'static' to declare a static method}}
  static func i2() { } // okay

  class subscript(i: Int) -> Int { i } // expected-cparser-error{{class subscripts are only allowed within classes; use 'static' to declare a static subscript}}
  static subscript(s: String) -> String { s }

  init(delegates: ()) {
    self.init()
  }

  init(doesNotDelegate: ()) {} // expected-cparser-error {{designated initializer cannot be declared in an extension of 'A2'}}
}

nonisolated actor A3 {} // expected-cparser-error {{'nonisolated' modifier cannot be applied to this declaration}}{{1-13=}} expected-astgen-error {{expected '}' to end actor}} expected-astgen-error {{expected '}' to end actor}}

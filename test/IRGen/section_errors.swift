// RUN: %target-swift-frontend -enable-experimental-feature SymbolLinkageMarkers -parse-as-library -emit-sil %s -o /dev/null -verify

// REQUIRES: swift_in_compiler
// REQUIRES: swift_feature_SymbolLinkageMarkers

@_used @_section("__TEXT,__mysection") var g0: Int = 1 // ok

struct MyStruct {
  @_used @_section("__TEXT,__mysection") static var static0: Int = 1 // ok
}

struct MyStruct2 {
  @_section("__TEXT,__mysection") var member0: Int = 1 // expected-error {{properties with attribute @_section must be static}}

  @_section("__TEXT,__mysection") static var static1: Int { return 1 } // expected-error {{'@_section' must not be used on computed properties}}
}

struct MyStruct3<T> {
  static var member1: Int = 1 // expected-error {{static stored properties not supported in generic types}}

  @_section("__TEXT,__mysection") func foo() {} // expected-error {{attribute @_section cannot be used in a generic context}}
}

struct MyStruct4<T> {
  struct InnerStruct {
    static var member2: Int = 1 // expected-error {{static stored properties not supported in generic types}}

    @_section("__TEXT,__mysection") static var member3: Int = 1 // expected-error {{static stored properties not supported in generic types}}
    // expected-error@-1 {{attribute @_section cannot be used in a generic context}}

    @_section("__TEXT,__mysection") func foo() {} // expected-error {{attribute @_section cannot be used in a generic context}}
  }
}

struct MyStruct5<T> {
}

extension MyStruct5 where T == Never {
  @_used @_section("__TEXT,__mysection") static let static3: Int = 1 // ok
}

@_section("__TEXT,__mysection") // expected-error {{'@_section' attribute cannot be applied to this declaration}}
struct SomeStruct {}

@_section("") var g1: Int = 1 // expected-error {{'@_section' section name cannot be empty}}

func function() {
  @_section("__TEXT,__mysection") var l0: Int = 1 // expected-error {{attribute '_section' can only be used in a non-local scope}}
  l0 += 1
  _ = l0

  @_used var l1: Int = 1 // expected-error {{attribute @_used can only be used in a non-local scope}}
  l1 += 1
  _ = l1
}

func function_with_type() {
  class MyClass {
    @_section("__TEXT,__mysection") static var member: Int = 1 // ok
  }

  do {
    class MyClass {
      @_section("__TEXT,__mysection") static var member: Int = 1 // ok
    }
  }
}

func function_with_type_generic<T>() -> T {
  class MyClass { // expected-error {{type 'MyClass' cannot be nested in generic function}}
    @_section("__TEXT,__mysection") static var member: Int = 1 // expected-error {{static stored properties not supported in generic types}}
    // expected-error@-1 {{attribute @_section cannot be used in a generic context}}
  }
}

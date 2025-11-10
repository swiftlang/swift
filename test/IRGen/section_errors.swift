// RUN: %target-swift-frontend -enable-experimental-feature CompileTimeValuesPreview -parse-as-library -emit-sil %s -o /dev/null -verify

// REQUIRES: swift_in_compiler
// REQUIRES: swift_feature_CompileTimeValuesPreview

@used @section("__TEXT,__mysection") var g0: Int = 1 // ok

struct MyStruct {
  @used @section("__TEXT,__mysection") static var static0: Int = 1 // ok
}

struct MyStruct2 {
  @section("__TEXT,__mysection") var member0: Int = 1 // expected-error {{properties with attribute @section must be static}}

  @section("__TEXT,__mysection") static var static1: Int { return 1 } // expected-error {{'@section' must not be used on computed properties}}
}

struct MyStruct3<T> {
  static var member1: Int = 1 // expected-error {{static stored properties not supported in generic types}}

  @section("__TEXT,__mysection") func foo() {} // expected-error {{attribute @section cannot be used in a generic context}}
}

struct MyStruct4<T> {
  struct InnerStruct {
    static var member2: Int = 1 // expected-error {{static stored properties not supported in generic types}}

    @section("__TEXT,__mysection") static var member3: Int = 1 // expected-error {{static stored properties not supported in generic types}}
    // expected-error@-1 {{attribute @section cannot be used in a generic context}}

    @section("__TEXT,__mysection") func foo() {} // expected-error {{attribute @section cannot be used in a generic context}}
  }
}

struct MyStruct5<T> {
}

extension MyStruct5 where T == Never {
  @used @section("__TEXT,__mysection") static let static3: Int = 1 // ok
}

@section("__TEXT,__mysection") // expected-error {{'@section' attribute cannot be applied to this declaration}}
struct SomeStruct {}

@section("") var g1: Int = 1 // expected-error {{'@section' section name cannot be empty}}

func function() {
  @section("__TEXT,__mysection") var l0: Int = 1 // expected-error {{attribute 'section' can only be used in a non-local scope}}
  l0 += 1
  _ = l0

  @used var l1: Int = 1 // expected-error {{attribute @used can only be used in a non-local scope}}
  l1 += 1
  _ = l1
}

func function_with_type() {
  class MyClass {
    @section("__TEXT,__mysection") static var member: Int = 1 // ok
  }

  do {
    class MyClass {
      @section("__TEXT,__mysection") static var member: Int = 1 // ok
    }
  }
}

func function_with_type_generic<T>() -> T {
  class MyClass { // expected-error {{type 'MyClass' cannot be nested in generic function}}
    @section("__TEXT,__mysection") static var member: Int = 1 // expected-error {{static stored properties not supported in generic types}}
    // expected-error@-1 {{attribute @section cannot be used in a generic context}}
  }
}

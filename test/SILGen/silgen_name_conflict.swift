// RUN: %target-swift-emit-silgen %s -enable-experimental-feature Extern -verify

// REQUIRES: swift_feature_Extern

@_silgen_name("my_extern_func")
func my_extern_func1() // expected-note {{function declared here}}

@_silgen_name("my_extern_func")
func my_extern_func2(x: Int)

@_extern(c, "my_other_extern_func")
func my_other_extern_func1() // expected-note {{function declared here}}

@_extern(c, "my_other_extern_func")
func my_other_extern_func2(x: Int)

public func foo() {
    my_extern_func1()
    my_extern_func2(x: 42) // expected-error {{function type mismatch, declared as '@convention(thin) () -> ()' but used as '@convention(thin) (Int) -> ()'}}

    my_other_extern_func1()
    my_other_extern_func2(x: 42) // expected-error {{function type mismatch, declared as '@convention(c) () -> ()' but used as '@convention(c) (Int) -> ()'}}
}

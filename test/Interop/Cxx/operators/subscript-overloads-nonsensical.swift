// RUN: split-file %s %t
// RUN: %target-swift-frontend -typecheck -verify -suppress-notes \
// RUN:    -cxx-interoperability-mode=default \
// RUN:    -I %t/Inputs -module-name main %t/main.swift
//
// RUN: %target-swift-ide-test -print-module -source-filename=x \
// RUN:   -cxx-interoperability-mode=default -I %t/Inputs \
// RUN:   -module-to-print=FBMod | %FileCheck %s

/// Torture test of mismatched operator[] overloads: same argument type,
/// different this-constness. In this test we stop caring about how operator[]
/// should be used and make sure it can be abused without crashing the compiler.
///
/// Currently, operator[] overload selection prefers the const overload, so that
/// is the behavior we look for in this test.

//--- Inputs/module.modulemap
module FBMod {
    header "fb.h"
}

//--- Inputs/fb.h
struct FooBar;

struct Foo {};
struct Bar {};

struct Val {};
struct Ptr {};
struct Ref {};

// CHECK: struct FooBar {
struct FooBar {
  Foo operator[](Val i) { return {}; }
  Bar operator[](Val i) const { return {}; }
  // CHECK: subscript(i: Val) -> Bar { get }

  Foo *operator[](Ptr i) { static Foo f; return &f; }
  Bar *operator[](Ptr i) const { static Bar b; return &b; }
  // CHECK: subscript(i: Ptr) -> UnsafeMutablePointer<Bar>! { mutating get set }

  Foo &operator[](Ref i) { static Foo f; return f; }
  Bar &operator[](Ref i) const { static Bar b; return b; }
  // CHECK: subscript(i: Ref) -> Bar { mutating get set }
};
// CHECK: }

//--- main.swift
import FBMod

var fbv = FooBar()
let fbu = FooBar()

let _: Bar = fbv[Val()]
fbv[Val()] = Foo() // expected-error {{no exact matches in call to subscript}}
                   // expected-error@-1 {{cannot assign value of type}}
fbv[Val()] = Bar() // expected-error {{no exact matches in call to subscript}}

let _: Bar = fbu[Val()]
fbu[Val()] = Foo()      // expected-error {{cannot assign through subscript: subscript is get-only}}
                        // expected-error@-1 {{cannot assign value of type}}
fbu[Val()] = Bar()      // expected-error {{cannot assign through subscript: subscript is get-only}}

let fbvp: UnsafeMutablePointer<Bar>? = fbv[Ptr()]
fbv[Ptr()] = fbvp!

let fbup: UnsafeMutablePointer<Bar>? = fbu[Ptr()] // expected-error {{cannot use mutating getter on immutable value: 'fbu' is a 'let'}}
fbu[Ptr()] = fbup! // expected-error {{cannot use mutating getter on immutable value: 'fbu' is a 'let'}}
                   // FIXME: ^this should complain about get-only setter rather than mutating getter

let _: Bar = fbv[Ref()]
fbv[Ref()] = Bar()

let _: Bar = fbu[Ref()] // expected-error {{cannot use mutating getter on immutable value: 'fbu' is a 'let'}}
fbu[Ref()] = Bar()      // expected-error {{failed to produce diagnostic for expression}}
                        // FIXME: ^this should complain about get-only setter

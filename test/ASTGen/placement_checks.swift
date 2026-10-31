// Placement rules the C++ parser enforces while parsing declarations and
// types, checked against both parsers.

// RUN: %target-typecheck-verify-swift
// RUN: %target-typecheck-verify-swift -enable-experimental-feature ParserASTGen

// REQUIRES: swift_swift_parser
// REQUIRES: swift_feature_ParserASTGen

// A 'class' requirement belongs first, once, and only in protocols.
protocol P1: class {}
protocol P2 {}
protocol P3: P2, class {} // expected-error {{'class' must come first in the requirement list}}
protocol P4: class, class {} // expected-error {{redundant 'class' requirement}}
struct S1: class {} // expected-error {{'class' constraint can only appear on protocol declarations}}

// 'class' members only exist in classes.
struct S2 {
  class func f() {} // expected-error {{class methods are only allowed within classes; use 'static' to declare a static method}} {{3-8=static}}
  class var v: Int { 0 } // expected-error {{class properties are only allowed within classes; use 'static' to declare a static property}} {{3-8=static}}
}

// Function types have no argument labels, and a tuple element has one label.
typealias F = (a: Int) -> Void // expected-error {{function types cannot have argument labels; use '_' before 'a'}} {{16-16=_ }}
typealias T = (a b: Int, Int) // expected-error {{tuple element cannot have two labels}}

// Initializers belong in types.
init() {} // expected-error {{initializers may only be declared within a type}}

// A protocol subscript requirement has a getter.
protocol P5 {
  subscript(i: Int) -> Int {} // expected-error {{subscript declarations must have a getter}}
}

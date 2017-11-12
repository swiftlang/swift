// RUN: %target-swift-frontend -module-name TestModule -typecheck -verify %s

// Function type with various forms of argument label.
typealias Function1 = (a: Int,   // expected-error{{function types cannot have argument labels; use '_' before 'a'}}{{24-24=_ }}
                       _ b: Int, // okay
                       c d: Int, // expected-error{{function types cannot have argument labels; use '_' before 'c'}}{{24-25=_}}
                       e _: Int, // expected-error{{function types cannot have argument labels; use '_' before 'e'}}{{24-29=}}
                       _: Int,   // okay
                       Int)      // okay
    -> Int

// Throwing versions.
typealias Function2 = (a: Int,   // expected-error{{function types cannot have argument labels; use '_' before 'a'}}{{24-24=_ }}
                       _ b: Int, // okay
                       c d: Int, // expected-error{{function types cannot have argument labels; use '_' before 'c'}}{{24-25=_}}
                       e _: Int, // expected-error{{function types cannot have argument labels; use '_' before 'e'}}{{24-29=}}
                       _: Int,   // okay
                       Int)      // okay
    throws -> Int

typealias Function3 = (a: Int,   // expected-error{{function types cannot have argument labels; use '_' before 'a'}}{{24-24=_ }}
                       _ b: Int, // okay
                       c d: Int, // expected-error{{function types cannot have argument labels; use '_' before 'c'}}{{24-25=_}}
                       e _: Int, // expected-error{{function types cannot have argument labels; use '_' before 'e'}}{{24-29=}}
                       _: Int,   // okay
                       Int)      // okay
    rethrows -> Int              // expected-error{{only function declarations may be marked 'rethrows'}}



struct S2<X> { }

func testTentativeParsing() {
  let _ = S2<(_ x: Int, _ y: Double) -> Int>()
}

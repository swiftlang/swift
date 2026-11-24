// RUN: %target-swift-emit-ir %s -enable-experimental-feature Embedded -parse-as-library -module-name main | %FileCheck -check-prefix=IR %s

// RUN: %target-run-simple-swift( -enable-experimental-feature Embedded -parse-as-library -wmo %target-embedded-posix-shim -g) | %FileCheck %s --check-prefix=OUTPUT
// RUN: %target-run-simple-swift( -enable-experimental-feature Embedded -parse-as-library -wmo %target-embedded-posix-shim -O) | %FileCheck %s --check-prefix=OUTPUT

// REQUIRES: executable_test
// REQUIRES: optimized_stdlib
// REQUIRES: swift_feature_Embedded

// An `is` test of an existential is a `checked_cast_addr_br test_only`, which
// IRGen lowers to swift_dynamicCastTest. The Embedded runtime has to provide
// it: answer the question without producing a value or disturbing the source.

protocol P { }
protocol CP: AnyObject { }

struct S: P { var x = 1 }
struct T: P { }
// Too big for the existential's inline buffer, so it is stored out of line.
struct Big: P { var a = 1, b = 2, c = 3, d = 4 }

class Base: CP { }
class Derived: Base { }
class Other: CP { }

// IR-LABEL: define {{.*}} @"$e4main3isSySbAA1P_pF"
// IR: call {{.*}} @swift_dynamicCastTest
@inline(never) func isS(_ p: any P) -> Bool { p is S }

@inline(never) func isBig(_ p: any P) -> Bool { p is Big }

@inline(never) func isDerived(_ p: any CP) -> Bool { p is Derived }

// A zero-sized cast destination can have a null address (IRGen passes `undef`),
// which must not be mistaken for a test. (concurrency-taskgroup.swift is where
// it reliably turns out to be null.)
enum Empty: P { case only }
@inline(never) func asEmpty(_ p: any P) -> Bool {
  if let _ = p as? Empty { return true }
  return false
}

// A failed test leaves the source intact for a later cast.
@inline(never) func testThenCast(_ p: any P) -> Int {
  if p is T { return -1 }
  if let s = p as? S { return s.x }
  return 0
}

func yn(_ b: Bool) -> StaticString { b ? "yes" : "no" }

@main struct Main {
  static func main() {
    // OUTPUT: yes
    // OUTPUT-NEXT: no
    // OUTPUT-NEXT: no
    print(yn(isS(S())))
    print(yn(isS(T())))
    print(yn(isS(Big())))
    // OUTPUT-NEXT: yes
    // OUTPUT-NEXT: no
    print(yn(isBig(Big())))
    print(yn(isBig(S())))
    // OUTPUT-NEXT: yes
    // OUTPUT-NEXT: no
    // OUTPUT-NEXT: no
    print(yn(isDerived(Derived())))
    print(yn(isDerived(Base())))
    print(yn(isDerived(Other())))
    // OUTPUT-NEXT: 1
    // OUTPUT-NEXT: -1
    print(testThenCast(S()))
    print(testThenCast(T()))
    // OUTPUT-NEXT: yes
    // OUTPUT-NEXT: no
    print(yn(asEmpty(Empty.only)))
    print(yn(asEmpty(T())))
  }
}

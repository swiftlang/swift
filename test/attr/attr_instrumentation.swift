// RUN: %target-typecheck-verify-swift -parse-as-library


@instrumentation(disable: address) func f1() {}
@instrumentation(disable: thread) func f2() {}
@instrumentation(disable: memtagStack) func f3() {}
@instrumentation(disable: coverage) func f4() {}
@instrumentation(disable: address, thread, memtagStack, coverage) func f5() {}
@instrumentation(default) func f6() {}
@instrumentation(default, disable: thread) func f7() {}
@instrumentation(disable: address, default) func f8() {}
@instrumentation(disable: address, default, disable: thread) func f9() {}

// Stacking is allowed.
@instrumentation(disable: address)
@instrumentation(disable: thread)
func stacked() {}

struct S: ~Copyable {
  @instrumentation(disable: address) init() {}
  @instrumentation(disable: address) deinit {}
  @instrumentation(disable: address) func method() {}
  @instrumentation(disable: address) static func staticMethod() {}

  var status: Int {
    @instrumentation(disable: address) get { 0 }
    @instrumentation(disable: address) set {}
  }

  @instrumentation(disable: address)
  subscript(i: Int) -> Int { i }
}

func closures() {
  let _ = { @instrumentation(disable: address) in }
  let _ = { @instrumentation(default) in }
  let _ = { @instrumentation(default, disable: thread) () -> Int in 0 }
  let _ = { @instrumentation(disable: coverage) (x: Int) in _ = x }
}

// Not applicable to these declarations.
@instrumentation(disable: address) struct NotAFunction {} // expected-error {{@instrumentation' attribute cannot be applied to this declaration}}
@instrumentation(disable: address) var notAFunction = 0 // expected-error {{@instrumentation' attribute cannot be applied to this declaration}}

// Malformed arguments.
@instrumentation func f10() {} // expected-error {{expected '(' in 'instrumentation' attribute}}
@instrumentation() func f11() {} // expected-error {{expected 'default' or 'disable:' in '@instrumentation' attribute}}
@instrumentation(address) func f12() {} // expected-error {{expected 'default' or 'disable:' in '@instrumentation' attribute}}
@instrumentation(disable) func f13() {} // expected-error {{expected 'default' or 'disable:' in '@instrumentation' attribute}}
@instrumentation(disable: ) func f14() {} // expected-error {{expected instrumentation kind in '@instrumentation' attribute}}
@instrumentation(disable: bogus) func f15() {} // expected-error {{unknown option 'bogus' for attribute 'instrumentation'}}
@instrumentation(disable: memtag) func f16() {} // expected-error {{unknown option 'memtag' for attribute 'instrumentation'}}
@instrumentation(disable: address, ) func f17() {} // expected-error {{expected instrumentation kind in '@instrumentation' attribute}}

// '@inline(always)' demands inlining, which contradicts disabling
// instrumentation in the body.
@inline(always) // expected-error {{'@inline(always)' cannot be combined with '@instrumentation(disable:)'}}
@instrumentation(disable: address) // expected-note {{'@instrumentation(disable:)' declared here}}
func inlineAlways1() {} // expected-note@-2 {{use '#if moduleInstrumentation(...)' to choose between '@inline(always)' and '@instrumentation(disable:)'}}

@instrumentation(disable: address, thread) // expected-note {{'@instrumentation(disable:)' declared here}}
@inline(always) // expected-error {{'@inline(always)' cannot be combined with '@instrumentation(disable:)'}}
func inlineAlways2() {} // expected-note@-1 {{use '#if moduleInstrumentation(...)' to choose between '@inline(always)' and '@instrumentation(disable:)'}}

// ... but '@instrumentation(default)' disables nothing, and the other
// '@inline' modes are only hints.
@inline(always) @instrumentation(default) func inlineAlwaysDefault() {}
@inline(__always) @instrumentation(disable: address) func inlineUnderscoreAlways() {}
@inline(never) @instrumentation(disable: address) func inlineNever() {}

struct T {
  @instrumentation(disable: address) // expected-note {{'@instrumentation(disable:)' declared here}}
  subscript(i: Int) -> Int {
    @inline(always) get { i } // expected-error {{'@inline(always)' cannot be combined with '@instrumentation(disable:)'}}
    // expected-note@-1 {{use '#if moduleInstrumentation(...)' to choose between '@inline(always)' and '@instrumentation(disable:)'}}
  }
}

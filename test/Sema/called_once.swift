// RUN: %target-typecheck-verify-swift -enable-experimental-feature CalledAttribute

// REQUIRES: swift_feature_CalledAttribute

// contextual type
do {
  func fn() {}

  // To @called(once)
  let _: @called(once) () -> Void = fn // Ok
  // Closure assumes `@called(once)`
  var once: @called(once) () -> Void = { } // Ok

  // From called(once)
  var _: () -> Void = once
  // expected-error@-1 {{invalid conversion from '@called(once)' function of type '@called(once) () -> Void' to function type '() -> Void'}}

  let onceOpt: (@called(once) () -> Void)? = fn // Ok
  let _: (() -> Void)? = onceOpt
  // expected-error@-1 {{invalid conversion from '@called(once)' function of type '@called(once) () -> Void' to function type '() -> Void'}}
}

// Argument conversions
func argumentConversions(fn: @escaping () -> Void, once: @called(once) () -> Void) {
  func onceFn(_ f: @called(once) () -> Void) {}
  func plainFn(_ f: () -> Void) {}

  onceFn(fn) // Ok
  onceFn(once) // Ok

  plainFn(once) // expected-error {{invalid conversion from '@called(once)' function of type '@called(once) () -> Void' to function type '() -> Void'}}
  onceFn({ }) // Ok
}

func contravariant() {
  func consumes(_: () -> Void) {}
  func consumesCalledOnce(_: @called(once) () -> Void) {}

  var wrapperFn: (@escaping () -> Void) -> Void = consumes
  var wrapperOnce: (@called(once) () -> Void) -> Void = consumesCalledOnce

  wrapperFn = wrapperOnce // Ok
  wrapperOnce = wrapperFn
  // expected-error@-1 {{invalid conversion from '@called(once)' function of type '@called(once) () -> Void' to function type '() -> Void'}}
}

func impliedEscaping(onceFn: @called(once) () -> Void) {
  func takesEscaping(_ f: @escaping () -> Void) {}
  takesEscaping(onceFn) // no diagnostics about `@escaping`
  // expected-error@-1 {{invalid conversion from '@called(once)' function of type '@called(once) () -> Void' to function type '() -> Void'}}
}

protocol P {
  func run(_: @called(once) () -> Void)
  // expected-note@-1 {{protocol requires function 'run' with type '(consuming @escaping @called(once) () -> Void) -> ()'}}
}

struct S1: P { // expected-error {{type 'S1' does not conform to protocol 'P'}} expected-note {{add stubs for conformance}}
  func run(_: () -> Void) {} // expected-note {{candidate has non-matching type '(() -> Void) -> ()'}}
}

struct S2: P {
  func run(_: @called(once) () -> Void) {} // Ok
}

protocol Q {
  func run(_: @escaping () -> Void)
}

struct S3: Q {
  func run(_: @called(once) () -> Void) {} // Ok (because `@called(once)` is more narrow then plain escaping type.
}

func testClosures() {
  let _: @called(once) () -> Void = { } // Ok

  let fn = { @called(once) in }
  let _: () -> Void = fn
  // expected-error@-1 {{invalid conversion from '@called(once)' function of type '@called(once) () -> ()' to function type '() -> Void'}}

  func once(_: consuming (@called(once) () -> Void)?) {}

  once { } // Ok
  once { @called(once) in // Ok
  }

  func plain(_: () -> Void) {}
  func plainEscaping(_: @escaping () -> Void) {}

  plain { @called(once) in
    // expected-error@-1 {{invalid conversion from '@called(once)' function of type '@called(once) () -> ()' to function type '() -> Void'}}
  }
  plainEscaping { @called(once) in
    // expected-error@-1 {{invalid conversion from '@called(once)' function of type '@called(once) () -> ()' to function type '() -> Void'}}
  }

  func generic<T>(_: T) {} // expected-note 2 {{required by local function 'generic' where 'T' = '@called(once) () -> ()'}}

  generic(fn)
  // expected-error@-1 {{type '@called(once) () -> ()' cannot conform to 'Copyable'}}
  // expected-note@-2 {{only concrete types such as structs, enums and classes can conform to protocols}}

  generic { @called(once) in }
  // expected-error@-1 {{type '@called(once) () -> ()' cannot conform to 'Copyable'}}
  // expected-note@-2 {{only concrete types such as structs, enums and classes can conform to protocols}}

  func genericNC<T: ~Copyable>(_: consuming T) {}

  genericNC(fn) // Ok
  genericNC { @called(once) in } // Ok
}

// RUN: %target-typecheck-verify-swift -enable-experimental-move-only

// Coverage for check that requires some ownership specifier to be written
// when a move-only / noncopyable type appears as a parameter of a function.

@_moveOnly
struct MO {
    var x = 0
}

struct Box<T> { var val: T }

@_moveOnly
public struct NoncopyableWrapper<T> {
  var x: T
}

class Inspector {
  func inspect(_ hasIt: inout MO, _ mo: MO, _ hasItAgain: __owned MO) {}
  // expected-error@-1 {{noncopyable parameter must specify its ownership}}
  // expected-note@-2 {{add '__shared' for an immutable reference}}{{41-41=__shared }}
  // expected-note@-3 {{add 'inout' for a mutable reference}}{{41-41=inout }}
  // expected-note@-4 {{add '__owned' to take the value from callers}}{{41-41=__owned }}
}

// expected-error@+4 {{noncopyable parameter must specify its ownership}}
// expected-note@+3 {{add '__shared' for an immutable reference}}{{20-20=__shared }}
// expected-note@+2 {{add 'inout' for a mutable reference}}{{20-20=inout }}
// expected-note@+1 {{add '__owned' to take the value from callers}}{{20-20=__owned }}
func applier(_ f: (MO) -> (),
             _ v: MO) {}
// expected-error@-1 {{noncopyable parameter must specify its ownership}}
// expected-note@-2 {{add '__shared' for an immutable reference}}{{19-19=__shared }}
// expected-note@-3 {{add 'inout' for a mutable reference}}{{19-19=inout }}
// expected-note@-4 {{add '__owned' to take the value from callers}}{{19-19=__owned }}

func caller() {
  let f = { (_ mo1: MO, _ mo2: MO) in () }
  // expected-error@-1 2{{noncopyable parameter must specify its ownership}}
  // expected-note@-2 {{add '__shared' for an immutable reference}}{{21-21=__shared }}
  // expected-note@-3 {{add '__shared' for an immutable reference}}{{32-32=__shared }}
  // expected-note@-4 {{add 'inout' for a mutable reference}}{{21-21=inout }}
  // expected-note@-5 {{add 'inout' for a mutable reference}}{{32-32=inout }}
  // expected-note@-6 {{add '__owned' to take the value from callers}}{{21-21=__owned }}
  // expected-note@-7 {{add '__owned' to take the value from callers}}{{32-32=__owned }}

  let g: (MO, MO) -> () = f
  // expected-error@-1 2{{noncopyable parameter must specify its ownership}}
  // expected-note@-2 {{add '__shared' for an immutable reference}}{{11-11=__shared }}
  // expected-note@-3 {{add '__shared' for an immutable reference}}{{15-15=__shared }}
  // expected-note@-4 {{add 'inout' for a mutable reference}}{{11-11=inout }}
  // expected-note@-5 {{add 'inout' for a mutable reference}}{{15-15=inout }}
  // expected-note@-6 {{add '__owned' to take the value from callers}}{{11-11=__owned }}
  // expected-note@-7 {{add '__owned' to take the value from callers}}{{15-15=__owned }}

  let partialG = { g($0, MO()) }

  let _: Box<(MO) -> ()> = Box(val: partialG)
  // expected-error@-1 {{noncopyable parameter must specify its ownership}}
  // expected-note@-2 {{add '__shared' for an immutable reference}}{{15-15=__shared }}
  // expected-note@-3 {{add 'inout' for a mutable reference}}{{15-15=inout }}
  // expected-note@-4 {{add '__owned' to take the value from callers}}{{15-15=__owned }}

  let _: Box<(inout MO) -> ()>? = nil
  let _: Box<(__shared MO) -> ()>? = nil

  let _: Box<(MO) -> ()>? = nil
  // expected-error@-1 {{noncopyable parameter must specify its ownership}}
  // expected-note@-2 {{add '__shared' for an immutable reference}}{{15-15=__shared }}
  // expected-note@-3 {{add 'inout' for a mutable reference}}{{15-15=inout }}
  // expected-note@-4 {{add '__owned' to take the value from callers}}{{15-15=__owned }}

  applier(partialG, MO())
}

func takeGeneric<T>(_ x: NoncopyableWrapper<T>) {}
// expected-error@-1 {{noncopyable parameter must specify its ownership}}
// expected-note@-2 {{add '__shared' for an immutable reference}}{{26-26=__shared }}
// expected-note@-3 {{add 'inout' for a mutable reference}}{{26-26=inout }}
// expected-note@-4 {{add '__owned' to take the value from callers}}{{26-26=__owned }}

func takeInstantiated(_ x: NoncopyableWrapper<Int>) {}
// expected-error@-1 {{noncopyable parameter must specify its ownership}}
// expected-note@-2 {{add '__shared' for an immutable reference}}{{28-28=__shared }}
// expected-note@-3 {{add 'inout' for a mutable reference}}{{28-28=inout }}
// expected-note@-4 {{add '__owned' to take the value from callers}}{{28-28=__owned }}


// >> First verify that when building the stdlib, we do have the copyable constraint. Note the module-name!!
// RUN: %target-swift-frontend -DSWIFT_MODULE -typecheck -verify -parse-stdlib -module-name Swift %s

// Demonstrate that plain -parse-stdlib, such as in some arbitrary test, does still get the Copyable constraint
// RUN: %target-swift-frontend -typecheck -verify -parse-stdlib %s

#if SWIFT_MODULE
  @_marker public protocol Copyable {}
  struct MO: ~Copyable {}
#else
  // NOTE: when building _without_ being the Swift module, we get a Builtin.Copyable instead of Swift.Copyable
  struct MO: ~Builtin.Copyable {}
#endif

func nextTime<T>(_ t: T) {} // expected-note {{Copyable' is implicit here}}

nextTime(MO()) // expected-error {{global function 'nextTime' requires that 'MO' conform to}}

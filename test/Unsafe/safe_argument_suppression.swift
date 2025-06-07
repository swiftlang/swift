// RUN: %target-typecheck-verify-swift -strict-memory-safety

@unsafe
class NotSafe {
  @safe var okay: Int { 0 }

  @safe var safeSelf: NotSafe { unsafe self }

  @safe func memberFunc(_: NotSafe) { }

  @safe subscript(ns: NotSafe) -> Int { 5 }

  @safe static func doStatically(_: NotSafe.Type) { }
  
  @safe static subscript(ns: NotSafe) -> Int { 5 }

  @safe init(_: NotSafe) { }

  func stillUnsafe() { }
}

@unsafe
class NotSafeSubclass: NotSafe {
}

@safe func okayFunc(_ ns: NotSafe) { }

@safe func testImpliedSafety(ns: NotSafe) {
  _ = ns.okay
  _ = ns.safeSelf.okay
  ns.memberFunc(ns)
  okayFunc(ns)
  _ = ns[ns]

  _ = NotSafe(ns)
  _ = NotSafe[ns]
  NotSafe.doStatically(NotSafe.self)

  ns.stillUnsafe() // expected-warning{{expression uses unsafe constructs but is not marked with 'unsafe'}}{{documentation-file=strict-memory-safety}}
  // expected-note@-1{{reference to parameter 'ns' involves unsafe type 'NotSafe'}}
  // expected-note@-2{{argument 'self' in call to instance method 'stillUnsafe' has unsafe type 'NotSafe'}}
}

@safe func testImpliedSafetySubclass(ns: NotSafeSubclass) {
  _ = ns.okay
  _ = ns.safeSelf.okay
  ns.memberFunc(ns)
  okayFunc(ns)
  _ = ns[ns]
}

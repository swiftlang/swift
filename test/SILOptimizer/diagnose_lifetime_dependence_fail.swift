// RUN: %target-swift-frontend -module-name test -emit-sil %s -o /dev/null -verify

// REQUIRES: diagnose_lifetime_dependence

public class C {
  public let i: Int64

  init() {
    self.i = 0
  }
}

@_nonEscapable public struct NoEscStructI {
  public let i: Int64

  @_unsafeNonEscapableResult
  init() {
    self.i = 0
  }

  @_unsafeNonEscapableResult
  static func make() -> NoEscStructI {
    return NoEscStructI()
  }
}

@_nonEscapable public struct NoEscStructC {
  public let c: C

  // FIXME: resultDependsOn
  @_unsafeNonEscapableResult
  init(c: C) {
    self.c = c
  }
}

@_nonEscapable public class NoEscClass {
  @_unsafeNonEscapableResult
  init() {}
}

// =============================================================================
// Unexpected Pass

// -----------------------------------------------------------------------------
// NonEscapingScope.caller

func passNoEscStructIReturn(nes: NoEscStructI) -> NoEscStructI {
  return nes
}

// =============================================================================
// Expected Fail - Incorrect Diagnostic

// FIXME: do not consider the assigns escapes. Only the return escapes.
func createNoEscStructIVarReturn() -> NoEscStructI {
  var nes = NoEscStructI.make() // expected-error {{lifetime-dependent value escapes its scope}}
  // expcted-note @-1 {{this use causes the lifetime-dependent value to escape}}
  nes = NoEscStructI.make() // expected-error {{lifetime-dependent value escapes its scope}}
  // expcted-note @-1 {{this use causes the lifetime-dependent value to escape}}
  return nes
}

// =============================================================================
// Unexpected Fail

// -----------------------------------------------------------------------------
// NonEscapingScope.orphan

// Returns trivial 'nes.i', which is not an escape.
//
// FIXME: do not consider the assigns escapes
func createNoEscStructIVar() -> Int64 {
  var nes = NoEscStructI.make()
  nes = NoEscStructI.make()
  return nes.i
}

// -----------------------------------------------------------------------------
// NonEscapingScope.caller

// FIXME: do not consider the assigns escapes
func passNoEscStructIVar(nes: inout NoEscStructI) -> Int64 {
  nes = NoEscStructI.make()
  return nes.i
}

// -----------------------------------------------------------------------------
// NonEscapingScope.owned

// 'c' is an owned scope.
// 'nes.c' is used within 'c'.
//
// FIXME: nec.c.i should not be an escaping use.
func createNewNoEscStructC() -> Int64 {
  let c = C()
  let nes = NoEscStructC(c: c)
  return nes.c.i
}

// 'c' is an owned scope.
// 'nes.c' escapes via return.
func createNewNoEscStructCReturn() -> C {
  let c = C()
  let nes = NoEscStructC(c: c)
  return nes.c
}

// -----------------------------------------------------------------------------
// NonEscapingScope.scoped

func createNoEscStructCAccess() -> C {
  var c = C()
  _ = { c = C() }
  let nes = NoEscStructC(c: c)
  return nes.c
}

// RUN: %target-swift-frontend -module-name test -emit-sil %s -o /dev/null -verify

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
// Expected Pass

// -----------------------------------------------------------------------------
// NonEscapingScope.orphan

// Returns trivial 'nes.i', which is not an escape.
//
// ### Diagnosing lifetime dependence on scope: orphan(  %2 = apply %1(%0) : $@convention(method) (@thin NES.Type) -> NES
func createNoEscStructI() -> Int64 {
  let nes = NoEscStructI.make()
  return nes.i
}

// -----------------------------------------------------------------------------
// NonEscapingScope.caller

// Returns a nontrivial member, which is an escape.
//
func createArgNoEscStructCArg(c: C) -> C {
  let nes = NoEscStructC(c: c)
  return nes.c
}

func passNoEscStructI(nes: NoEscStructI) -> Int64 {
  return nes.i
}

func passNoEscStructILet(nes: NoEscStructI) -> Int64 {
  let nes = nes
  return nes.i
}

// =============================================================================
// Expected Fail

// -----------------------------------------------------------------------------
// NonEscapingScope.orphan

// Escapes a new non-escapable orphan.
//
func createNoEscStructIReturn() -> NoEscStructI {
  return NoEscStructI.make() // expected-error {{lifetime-dependent value escapes its scope}}
  // expcted-note @-1 {{this use of the lifetime-dependent value is out of scope}}
}

// Escapes a new non-escapable orphan.
//
// FIXME: diagnostic should be escapes its scope
func createNoEscStructILetReturn() -> NoEscStructI {
  let nes = NoEscStructI.make() 
  return nes
}

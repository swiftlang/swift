// RUN: %target-typecheck-verify-swift -Wwarning EmbeddedRestrictions -verify-additional-prefix nonembedded-
// RUN: %target-typecheck-verify-swift -enable-experimental-feature Embedded -verify-additional-prefix embedded-

// REQUIRES: swift_in_compiler
// REQUIRES: swift_feature_Embedded

// ---------------------------------------------------------------------------
// Untyped throws
// ---------------------------------------------------------------------------

enum MyError: Error {
case failed
}

// expected-warning@+1{{untyped throws is not available in Embedded Swift; add a thrown error type with '(type)'}}{{28-28=(<#thrown error type#>)}}
func untypedThrows() throws { }

// expected-warning@+1{{untyped throws is not available in Embedded Swift; add a thrown error type with '(type)'}}{{41-41=(<#thrown error type#>)}}
func rethrowingFunction(param: () throws -> Void) rethrows { }

// expected-warning@+1{{untyped throws is not available in Embedded Swift; add a thrown error type with '(type)'}}{{29-29=(<#thrown error type#>)}}
typealias FnType = () throws -> Void

func untypedThrowsInBody() {
  // expected-warning@+1{{untyped throws is not available in Embedded Swift; add a thrown error type with '(type)'}}{{12-12=(<#thrown error type#>)}}
  do throws {
    throw MyError.failed
  } catch {
  }

  // expected-warning@+1{{untyped throws is not available in Embedded Swift; add a thrown error type with '(type)'}}{{19-19=(<#thrown error type#>)}}
  _ = { (x) throws in x + 1 }
}

struct SomeStruct {
  // expected-warning@+1{{untyped throws is not available in Embedded Swift; add a thrown error type with '(type)'}}{{16-16=(<#thrown error type#>)}}
  init() throws { }

  var value: Int {
  // expected-warning@+1{{untyped throws is not available in Embedded Swift; add a thrown error type with '(type)'}}{{15-15=(<#thrown error type#>)}}
    get throws {
      0
    }
  }
}

// ---------------------------------------------------------------------------
// weak/unowned references
// ---------------------------------------------------------------------------

// Note: this have always been an error in Embedded Swift. Make sure they stay
// that way, but are emitted as warnings when the restrictions are enabled.

public class MyClass { }

public struct MyStruct {
  var normalVar: MyClass
  weak var weakVar: MyClass? // expected-nonembedded-warning {{attribute 'weak' cannot be used in Embedded Swift}}
  // expected-embedded-error@-1 {{attribute 'weak' cannot be used in Embedded Swift}}

  unowned var unownedVar: MyClass // expected-nonembedded-warning {{attribute 'unowned' cannot be used in Embedded Swift}}
  // expected-embedded-error @-1{{attribute 'unowned' cannot be used in Embedded Swift}}

  unowned(unsafe) var unownedUnsafe: MyClass
}

// ---------------------------------------------------------------------------
// #if handling to suppress diagnostics for non-Embedded-only code
// ---------------------------------------------------------------------------

#if $Embedded

// expected-embedded-warning@+1{{untyped throws is not available in Embedded Swift; add a thrown error type with '(type)'}}{{31-31=(<#thrown error type#>)}}
func stillProblematic() throws { }

#else

func notProblematicAtAll() throws { }

#endif

#if !hasFeature(Embedded)
func stillNotProblematicAtAll() throws { }
#endif

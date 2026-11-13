// RUN: %target-typecheck-verify-swift
// RUN: %target-typecheck-verify-swift -D FILE_DEFAULT
// RUN: %target-typecheck-verify-swift -verify-additional-prefix enabled- -strict-memory-safety
// RUN: %target-typecheck-verify-swift -verify-additional-prefix enabled- -strict-memory-safety:migrate
// RUN: %target-typecheck-verify-swift -verify-additional-prefix enabled- -Wwarning StrictMemorySafety
// RUN: %target-typecheck-verify-swift -verify-additional-prefix enabled- -Werror StrictMemorySafety

#if FILE_DEFAULT
default @diagnose(StrictMemorySafety, as: warning)
#endif

// Can use @unsafe and @safe without strict memory safety being enabled.
@unsafe func f() { }
@safe func g(_: UnsafeRawPointer) { }

protocol P {
  func f()
}

struct X: @unsafe P {
  @unsafe func f() { }
}

// The feature is enabled exactly when the command line enables strict memory
// safety diagnostics for the whole module.
#if hasFeature(StrictMemorySafety)
#warning("strict memory safety is enabled")
// expected-enabled-warning@-1:10 {{strict memory safety is enabled}}
#endif

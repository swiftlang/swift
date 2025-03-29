// RUN: %target-typecheck-verify-swift


// Can use @unsafe and @safe without strict memory safety being enabled.
@unsafe func f() { }
@safe func g(_: UnsafeRawPointer) { }

protocol P {
  func f()
}

struct X: @unsafe P {
  @unsafe func f() { }
}

// The feature flag is not enabled, though.
#if hasFeature(StrictMemorySafety)
#error("Strict memory safety is not enabled!")
#endif

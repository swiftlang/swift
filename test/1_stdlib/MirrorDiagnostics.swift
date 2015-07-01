// RUN: %target-parse-verify-swift

func useQuickLookObject(object: QuickLookObject) {} // expected-error {{'QuickLookObject' has been renamed to 'PlaygroundQuickLook'}}

func useReflecting() {
  reflect(1) // expected-error {{'reflect' is unavailable: call the 'Mirror(reflecting:)' initializer}}
}


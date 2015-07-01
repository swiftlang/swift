// RUN: %target-parse-verify-swift

func useQuickLookObject(object: QuickLookObject) {} // expected-error {{'QuickLookObject' has been renamed to 'PlaygroundQuickLook'}}


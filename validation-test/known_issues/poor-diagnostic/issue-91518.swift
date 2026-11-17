// rdar://185017252 (https://github.com/swiftlang/swift/issues/91518)
// RUN: %target-typecheck-verify-swift

// REQUIRES: objc_interop

var stuff = Set(["red", 2])
// expected-error@-1 {{generic parameter 'Element' could not be inferred}}
// expected-note@-2 {{explicitly specify the generic arguments to fix this issue}}
// expected-error@-3 {{missing argument label '_immutableCocoaSet:' in call}}
// expected-error@-4 {{argument type '[Any]' expected to be an instance of a class or class-constrained type}}

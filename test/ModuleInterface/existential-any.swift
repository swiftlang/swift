// Verify that `any` is not required in swiftinterfaces.
// RUN: %target-swift-frontend -typecheck-module-from-interface %S/Inputs/existential-any-ignore-missing-in-interface.swiftinterface -module-name ExistentialAnyMissing

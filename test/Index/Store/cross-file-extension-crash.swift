// Ensure that we don't crash due to resolving an extension in a non-primary
// file.

// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -index-store-path %t/idx -o %t/file.o -typecheck -primary-file %s %S/Inputs/cross-file-extension-crash-other.swift -verify

extension X {
  func foo() { }
}

// RUN: %empty-directory(%t)

// RUN: %target-swift-frontend -module-name errors -emit-module -o %t/errors.swiftmodule -experimental-allow-module-with-compiler-errors %s

// @discardableResult is not allowed on a struct, make sure we don't crash
// when allowing errors

@discardableResult
struct SomeStruct {}

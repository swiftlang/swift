// RUN: %empty-directory(%t)

// RUN: %target-swift-frontend -module-name errors -emit-module -o %t/errors.swiftmodule -experimental-allow-module-with-compiler-errors %s

// deinit is only valid on a class, make sure we don't crash when allowing
// errors

deinit {}

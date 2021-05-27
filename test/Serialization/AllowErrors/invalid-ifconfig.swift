// RUN: %empty-directory(%t)

// RUN: %target-swift-frontend -module-name errors -emit-module -o %t/mods/errors.swiftmodule -experimental-skip-all-function-bodies -experimental-allow-module-with-compiler-errors %s

// Performing AST verification would crash here, so check that we don't verify
// when allowing errors
#if os(
struct Anything {

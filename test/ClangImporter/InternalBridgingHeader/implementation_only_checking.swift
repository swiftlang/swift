// RUN: %empty-directory(%t)
// RUN: mkdir -p %t/tmp

// Test with the normal bridging header.
// RUN: %target-typecheck-verify-swift -internal-import-bridging-header %S/../Inputs/c-bridging-header.h -sdk %clang-importer-sdk -verify-ignore-unknown

// RUN: not %target-swift-frontend -typecheck -internal-import-bridging-header %S/../Inputs/c-bridging-header.h -sdk %clang-importer-sdk %s 2>&1 | %FileCheck %s

// Test with a precompiled bridging header.
// RUN: %target-swift-frontend -emit-pch -o %t/c-bridging-header.pch %S/../Inputs/c-bridging-header.h -sdk %clang-importer-sdk
// RUN: %target-typecheck-verify-swift -internal-import-bridging-header %t/c-bridging-header.pch -sdk %clang-importer-sdk -verify-ignore-unknown


@inlinable
public func f() -> Any {
  return red
  // expected-error@-1{{var 'red' is internal and cannot be referenced from an '@inlinable' function}}
  // expected-warning@-2{{getter for var 'red' is internal and should not be referenced from an '@inlinable' function}}
}

// CHECK: var 'red' imported as 'internal' from bridging header

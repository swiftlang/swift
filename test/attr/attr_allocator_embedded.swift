// RUN: %target-typecheck-verify-swift -parse-stdlib -enable-experimental-feature Embedded -enable-experimental-feature ClassAllocators
// RUN: not %target-swift-frontend -typecheck %s -parse-stdlib -enable-experimental-feature Embedded -enable-experimental-feature ClassAllocators -module-name M -default-class-allocator M.Pool 2>&1 | %FileCheck %s --check-prefix=DEFAULT

// REQUIRES: swift_feature_Embedded
// REQUIRES: swift_feature_ClassAllocators

// Class allocators aren't supported in Embedded Swift yet.

enum Pool {}

@allocator(Pool) // expected-error {{'@allocator' is not supported in Embedded Swift}}
final class C {}

// DEFAULT: error: '-default-class-allocator' is not supported in Embedded Swift

// RUN: %empty-directory(%t)
// RUN: echo 'C is for Cookie' > %t/Garbage.swiftmodule
// RUN: %target-swift-frontend %s -typecheck -I %t 2>&1 | %FileCheck %s

// Invalid .swiftmodule found during the `#if canImport` needs to warn, but that
// reenters `#if` region resolution to determine the behavior of that warning. Test
// that it doesn't recurse forever!

// CHECK: {{.*}} warning: canImport() evaluated to false due to invalid swiftmodule: {{.*}}Garbage.swiftmodule

#if canImport(Garbage)
  import Garbage
#endif

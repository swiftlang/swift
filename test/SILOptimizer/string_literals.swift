// RUN: %target-swift-frontend -parse-as-library -O -emit-ir  %s | %FileCheck %s
// RUN: %target-swift-frontend -parse-as-library -Osize -emit-ir  %s | %FileCheck %s
// REQUIRES: swift_stdlib_no_asserts,optimized_stdlib
// REQUIRES: swift_in_compiler

// The required relocation format for a single return LLVM instruction are not necessarily
// supported on object file formats other than Mach-O.
// REQUIRES: OS=macosx || OS=ios || OS=tvos || OS=watchOS || OS=xros

// FIXME(rdar://problem/45856408): The 7-bit discriminator complicates codegen
// on 32-bit platforms.
// UNSUPPORTED: PTRSIZE=32

// This is an end-to-end test to ensure that the optimizer generates
// optimal code for string literals

// CHECK-LABEL: define {{.*}}test_create_verysmallstring
// CHECK:      entry:
// CHECK-NEXT:   ret {{.*}}
// CHECK-NEXT: }
public func test_create_verysmallstring() -> String {
  return "a"
}

// CHECK-LABEL: define {{.*}}test_create_smallstring
// CHECK:      entry:
// CHECK-NEXT:   ret {{.*}}
// CHECK-NEXT: }
public func test_create_smallstring() -> String {
  return "abcdefghijkl012"
}

// CHECK-LABEL: define {{.*}}test_create_largestring
// CHECK-NOT:    load
// CHECK-NOT:    call
// CHECK:        ret
public func test_create_largestring() -> String {
  return "abcdefghijkl012qwerqwer"
}

// CHECK-LABEL: define {{.*}}test_create_unicode
// CHECK-NOT:    load
// CHECK-NOT:    call
// CHECK:        ret
public func test_create_unicode() -> String {
  return "❄️gastroperiodyni"
}

// RUN: %empty-directory(%t)

// RUN: %target-swift-frontend -enable-experimental-feature Embedded -enable-experimental-feature SymbolLinkageMarkers -module-name main -mergeable-symbols -O %s -emit-ir | %FileCheck %s --check-prefix=CHECK-IR
// RUN: %target-swift-frontend -enable-experimental-feature Embedded -enable-experimental-feature SymbolLinkageMarkers -module-name main -mergeable-symbols -O %s -c -o %t/a.o
// RUN: %target-clang %target-clang-resource-dir-opt %t/a.o -o %t/a.out -dead_strip
// RUN: %llvm-nm --defined-only --format=just-symbols --demangle %t/a.out | sort | %FileCheck %s --check-prefix=CHECK-NM
// RUN: %target-run %t/a.out | %FileCheck %s

// REQUIRES: swift_in_compiler
// REQUIRES: executable_test
// REQUIRES: swift_feature_Embedded
// REQUIRES: swift_feature_SymbolLinkageMarkers

public func a_this_is_unused() { }

@_used
public func b_this_is_unused_but_explicitly_retained() { }

// CHECK-IR: define weak_odr {{.*}}@"$e4main16a_this_is_unusedyyF"()
// CHECK-IR: define weak_odr {{.*}}@"$e4main40b_this_is_unused_but_explicitly_retainedyyF"()

// CHECK-NM-NOT: $e4main14this_is_unusedyyF
// CHECK-NM: $e4main40b_this_is_unused_but_explicitly_retainedyyF

print("Hello")
// CHECK: Hello

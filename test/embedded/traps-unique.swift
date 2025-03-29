// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend %s -Osize -enable-experimental-feature Embedded -emit-ir -Xllvm -link-embedded-runtime=0 -disable-access-control | %FileCheck %s

// REQUIRES: swift_in_compiler
// REQUIRES: executable_test
// REQUIRES: optimized_stdlib
// REQUIRES: OS=macosx || OS=linux-gnu
// REQUIRES: swift_feature_Embedded

public func foobar(i: Int) {
  if i == 1 { _assertionFailure("prefix", "message 1", file: "", line: 0, flags: 0) }
  if i == 2 { _assertionFailure("prefix", "message 2", file: "", line: 0, flags: 0) }
}

// CHECK: define {{.*}}@"$e4main6foobar1iySi_tF"{{.*}} {
// CHECK: entry:
// CHECK:   switch i64 %0, label %3 [
// CHECK:     i64 1, label %1
// CHECK:     i64 2, label %2
// CHECK:   ]
// CHECK: 1:
// CHECK:   tail call void asm sideeffect ""
// CHECK:   tail call void @llvm.trap()
// CHECK:   unreachable
// CHECK: 2:
// CHECK:   tail call void asm sideeffect ""
// CHECK:   tail call void @llvm.trap()
// CHECK:   unreachable
// CHECK: 3:
// CHECK:   ret void
// CHECK: }

// We should not see a call to _asssertionFailure in IR because that means such basic block can be merged with another
// and break the expectations that each trap point is unique.
// CHECK-NOT: @"$es17_assertionFailure__4file4line5flagss5NeverOs12StaticStringV_A2HSus6UInt32VtF"
// CHECK-NOT: @"$ss17_assertionFailure__4file4line5flagss5NeverOs12StaticStringV_A2HSus6UInt32VtF"

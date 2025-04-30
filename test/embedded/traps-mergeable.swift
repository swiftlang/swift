// RUN: %target-swift-emit-ir -enable-experimental-feature Extern -enable-experimental-feature Embedded -mergeable-traps -wmo -Xllvm -link-embedded-runtime=0 %s -O | %FileCheck %s

// REQUIRES: swift_in_compiler
// REQUIRES: optimized_stdlib
// REQUIRES: swift_feature_Embedded
// REQUIRES: swift_feature_Extern

@_extern(c)
public func external()

public func test(i: Int, j: Int) {
     precondition(i != 0, "precondition 1")
     external()
     precondition(j != 1, "precondition 2")
}

// CHECK-NOT: call void asm sideeffect ""

// CHECK: define {{.*}}@"$e4main4test1i1jySi_SitF"
// CHECK:   tail call void @llvm.trap()
// CHECK:   unreachable
// CHECK:   tail call void @llvm.trap()
// CHECK:   unreachable
// CHECK: }

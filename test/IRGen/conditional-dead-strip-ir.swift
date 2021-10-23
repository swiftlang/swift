// Tests that with -conditional-runtime-records, IRGen marks class, struct,
// enum, protocol, and protocol conformance records as conditionally removable
// via !llvm.used.conditional metadata.

// RUN: %target-build-swift -Xfrontend -disable-objc-interop -Xfrontend -conditional-runtime-records \
// RUN:    %s -emit-ir -o - | %FileCheck %s

public protocol TheProtocol {
}

public class Class: TheProtocol {
}

public struct Struct {
}

public enum Enum {
}

// CHECK:      @llvm.{{(compiler.)?}}used = appending global [
// CHECK-SAME:   @"$s4main11TheProtocolHr"
// CHECK-SAME:   @"$s4main5ClassCAA11TheProtocolAAHc"
// CHECK-SAME:   @"$s4main5ClassCHn"
// CHECK-SAME:   @"$s4main6StructVHn"
// CHECK-SAME:   @"$s4main4EnumOHn"
// CHECK-SAME: ], section "llvm.metadata"

// CHECK:      !llvm.used.conditional = !{[[C1:!.*]], [[C2:!.*]], [[C3:!.*]], [[C4:!.*]], [[C5:!.*]]}

// CHECK:      [[C1]]  = !{{{.*}} @"$s4main11TheProtocolHr", i32 0, [[C1A:!.*]]}
// CHECK:      [[C1A]] = !{{{.*}} @"$s4main11TheProtocolMp"}

// CHECK:      [[C2]]  = !{{{.*}} @"$s4main5ClassCAA11TheProtocolAAHc", i32 1, [[C2A:!.*]]}
// CHECK:      [[C2A]] = !{{{.*}} @"$s4main11TheProtocolMp", {{.*}} @"$s4main5ClassCMn"}

// CHECK:      [[C3]]  = !{{{.*}} @"$s4main5ClassCHn", i32 0, [[C3A:!.*]]}
// CHECK:      [[C3A]] = !{{{.*}} @"$s4main5ClassCMn"}

// CHECK:      [[C4]]  = !{{{.*}} @"$s4main6StructVHn", i32 0, [[C4A:!.*]]}
// CHECK:      [[C4A]] = !{{{.*}} @"$s4main6StructVMn"}

// CHECK:      [[C5]]  = !{{{.*}} @"$s4main4EnumOHn", i32 0, [[C5A:!.*]]}
// CHECK:      [[C5A]] = !{{{.*}} @"$s4main4EnumOMn"}

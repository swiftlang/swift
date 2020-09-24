// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -emit-ir %s | %FileCheck %s --check-prefix DIRECT-CHECK
// RUN: %target-swift-frontend -emit-sib %s -o %t/access-level.sib
// RUN: %target-swift-frontend -emit-ir %t/access-level.sib | %FileCheck %s --check-prefix FROM-SIB-CHECK

// Ensure that the method linkage is default external when lowered from .swift directly
// DIRECT-CHECK-NOT: define{{.*}}internal{{.*}}swiftcc{{.*}}void @"$s4main7VisitorC13visitExprImpl33_205B03B83823935B4865F4617387553BLLyyF"

// Ensure that the method linkage is default external when lowered from .swift -> .sib -> .ll
// FROM-SIB-CHECK-NOT: define{{.*}}internal{{.*}}swiftcc{{.*}}void @"$s4main7VisitorC13visitExprImpl33_205B03B83823935B4865F4617387553BLLyyF"

open class Visitor {
    public func visit() {
        visitExprImpl()
    }
    @_optimize(none)
    private func visitExprImpl() {
    }
}

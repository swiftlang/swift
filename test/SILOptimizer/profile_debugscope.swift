// RUN: %target-swift-frontend -Onone -emit-silgen -profile-generate \
// RUN:   -sil-verify-all -Xllvm -sil-print-debuginfo %s | %FileCheck %s

// <rdar://problem/46686369>
// REQUIRES: objc_interop

// CHECK: bb4:
// CHECK: integer_literal $Builtin.Int64, 0, loc "{{.*}}":18:10, scope 2
// CHECK-NEXT:  integer_literal $Builtin.Int32, 2, loc "{{.*}}":18:10, scope 2
// CHECK-NEXT:  integer_literal $Builtin.Int32, 1, loc "{{.*}}":18:10, scope 2
// CHECK-NEXT:  builtin "int_instrprof_increment"(%31 : $Builtin.RawPointer, %32 : $Builtin.Int64, %33 : $Builtin.Int32, %34 : $Builtin.Int32) : $(), loc "{{.*}}":18:10, scope 2
// CHECK-NEXT:  unreachable , loc "{{.*}}":16:1, scope 2


import Foundation
guard let patatino: URL = {
    return nil
}() else {
    exit(0)
}

// RUN: %target-swift-frontend -emit-ir -g %s -o - | %FileCheck %s
// RUN: %target-swift-frontend -emit-sil -emit-verbose-sil -g %s -o - | %FileCheck %s --check-prefix=SIL-CHECK
// REQUIRES: objc_interop
import Foundation

class Foo : NSObject {
  dynamic func foo(_ f: (Int64) -> Int64, x: Int64) -> Int64 {
    return f(x)
  }
}

let foo = Foo()
let y = 3 as Int64
let i = foo.foo(-, x: y)

// CHECK: define {{.*}}@_TTRXFdCb_dVs5Int64_dS__XFo_dS__dS__
// CHECK-NOT: ret
// CHECK: call {{.*}}, !dbg ![[LOC:.*]]
// CHECK: ![[THUNK:.*]] = distinct !DISubprogram(linkageName: "_TTRXFdCb_dVs5Int64_dS__XFo_dS__dS__"
// CHECK-NOT:                           line:
// CHECK-SAME:                          ){{$}}
// CHECK: ![[LOC]] = !DILocation(line: 0, scope: ![[THUNK]])

// SIL-CHECK: sil shared {{.*}}@_TTRXFdCb_dVs5Int64_dS__XFo_dS__dS__
// SIL-CHECK-NOT: return
// SIL-CHECK: apply {{.*}}auto_gen

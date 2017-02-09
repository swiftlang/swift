// RUN: %target-swift-frontend -Xllvm -new-mangling-for-tests -emit-ir -g %s -o - | %FileCheck %s
// RUN: %target-swift-frontend -Xllvm -new-mangling-for-tests -emit-sil -emit-verbose-sil -g %s -o - | %FileCheck %s --check-prefix=SIL-CHECK
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

// CHECK: define {{.*}}@_T0s5Int64VABIyByd_AbBIxyd_TR
// CHECK-NOT: ret
// CHECK: call {{.*}}, !dbg ![[LOC:.*]]
// CHECK: ![[THUNK:.*]] = distinct !DISubprogram(linkageName: "_T0s5Int64VABIyByd_AbBIxyd_TR"
// CHECK-NOT:                           line:
// CHECK-SAME:                          ){{$}}
// CHECK: ![[LOC]] = !DILocation(line: 0, scope: ![[THUNK]])

// SIL-CHECK: sil shared {{.*}}@_T0s5Int64VABIyByd_AbBIxyd_TR
// SIL-CHECK-NOT: return
// SIL-CHECK: apply {{.*}}auto_gen

// RUN: %target-swift-frontend -emit-ir -g %s -o - | %FileCheck %s
// RUN: %target-swift-frontend -emit-sil -emit-verbose-sil -g %s -o - | %FileCheck %s --check-prefix=SIL-CHECK
// REQUIRES: objc_interop
import Foundation

class Foo : NSObject {
  @objc dynamic func foo(_ f: (Int64) -> Int64, x: Int64) -> Int64 {
    return f(x)
  }
}

let foo = Foo()
let y = 3 as Int64
let i = foo.foo(-, x: y)

// CHECK: define {{.*}}@"$Ss5Int64VABIyByd_A2BIegyd_TR"
// CHECK-NOT: ret
// CHECK: call {{.*}}, !dbg ![[LOC:.*]]
// CHECK: ![[FILE:[0-9]+]] = !DIFile(filename: "<compiler-generated>", directory: "")
// CHECK: ![[THUNK:.*]] = distinct !DISubprogram(linkageName: "$Ss5Int64VABIyByd_A2BIegyd_TR"
// CHECK-SAME:                          file: ![[FILE]]
// CHECK-NOT:                           line:
// CHECK-SAME:                          flags: DIFlagArtificial
// CHECK-SAME:                          ){{$}}
// CHECK: ![[LOC]] = !DILocation(line: 0, scope: ![[THUNK]])

// SIL-CHECK: sil shared {{.*}}@$Ss5Int64VABIyByd_A2BIegyd_TR
// SIL-CHECK-NOT: return
// SIL-CHECK: apply {{.*}}auto_gen

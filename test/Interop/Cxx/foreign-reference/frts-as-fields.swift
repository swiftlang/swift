// RUN: %target-run-simple-swift(-I %swift_src_root/lib/ClangImporter/SwiftBridging -I %S/Inputs -cxx-interoperability-mode=default -Xfrontend -disable-availability-checking -Onone) | %FileCheck %s

// REQUIRES: executable_test

import LoggingFrts

struct SwiftStruct {
    var frt: SharedFRT
    var token: MyToken
}

func go() {
    let frt = SharedFRT()
    let token = MyToken()
    let _ = SwiftStruct(frt: frt, token: token)
    let _ = SwiftStruct(frt: frt, token: token)
    let _ = SwiftStruct(frt: frt, token: token)
}

go()

// CHECK:      RefCount: 1, message: Ctor
// CHECK-NEXT: RefCount: 2, message: retain
// CHECK-NEXT: RefCount: 1, message: release
// CHECK-NEXT: RefCount: 2, message: retain
// CHECK-NEXT: RefCount: 1, message: release
// CHECK-NEXT: RefCount: 2, message: retain
// CHECK-NEXT: RefCount: 1, message: release
// CHECK-NEXT: RefCount: 0, message: release
// CHECK-NEXT: RefCount: 0, message: Dtor
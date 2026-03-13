// RUN: %target-run-simple-swift(-I %swift_src_root/lib/ClangImporter/SwiftBridging -I %S/Inputs -cxx-interoperability-mode=default -Xfrontend -disable-availability-checking -Onone) | %FileCheck %s

// REQUIRES: executable_test

import LoggingFrts

func go() {
    let frt = DerivedFRT()
    let copy = frt
}

go()

// CHECK: RefCount: 1, message: Ctor
// CHECK: RefCount: 2, message: retain
// CHECK: RefCount: 1, message: release
// CHECK: RefCount: 0, message: release
// CHECK: RefCount: 0, message: Dtor

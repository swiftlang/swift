// RUN: %target-run-simple-swift(-I %swift_src_root/lib/ClangImporter/SwiftBridging -I %S/Inputs -cxx-interoperability-mode=default -Xfrontend -disable-availability-checking -O) | %FileCheck %s

// REQUIRES: executable_test

import LoggingFrts

@inline(never)
func use(_ x: CInt) {
    print("Value is \(x).")
}

func testRefIssues() {
    var a2 = Optional.some(Payload.create(0));
    let b2 = a2!.ptr();
    a2 = Optional.none;
    let v2 = b2.value();
    use(v2)
}
testRefIssues()

// CHECK:      RefCount: 1, message: Ctor
// CHECK-NEXT: RefCount: 2, message: retain
// CHECK-NEXT: RefCount: 1, message: release
// CHECK-NEXT: Value is 0.
// CHECK-NEXT: RefCount: 0, message: release
// CHECK-NEXT: RefCount: 0, message: Dtor

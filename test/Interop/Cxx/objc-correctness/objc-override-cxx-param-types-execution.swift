// RUN: %empty-directory(%t2)

// RUN: %target-interop-build-clangxx -c %S/Inputs/objc-base-with-cxx-params.mm -o %t2/objc-base-impl.o -fobjc-arc

// RUN: %target-run-simple-swift(-I %S/Inputs -cxx-interoperability-mode=default -Xcc -fignore-exceptions -Xlinker %t2/objc-base-impl.o) | %FileCheck %s

// REQUIRES: executable_test
// REQUIRES: objc_interop

import Foundation
import ObjCBaseWithCxxParams

class SwiftSub: ObjCBase {
    override func getRecord() -> CxxRecord {
        return CxxRecord(42)
    }

    override func processRecord(_ record: CxxRecord) {
        print("processRecord: \(record.value)")
    }

    override func transform(_ input: CxxRecord) -> CxxRecord {
        return CxxRecord(input.value + 1)
    }
}

func testDynamicDispatch() {
    let obj: ObjCBase = SwiftSub()

    let r = obj.getRecord()
    print("getRecord: \(r.value)")

    obj.processRecord(CxxRecord(99))

    let t = obj.transform(CxxRecord(10))
    print("transformRecord: \(t.value)")
}

func testBaseClass() {
    let base = ObjCBase()

    let r = base.getRecord()
    print("base getRecord: \(r.value)")

    let t = base.transform(CxxRecord(10))
    print("base transformRecord: \(t.value)")
}

testDynamicDispatch()
testBaseClass()

// CHECK: getRecord: 42
// CHECK-NEXT: processRecord: 99
// CHECK-NEXT: transformRecord: 11
// CHECK-NEXT: base getRecord: 100
// CHECK-NEXT: base transformRecord: 20

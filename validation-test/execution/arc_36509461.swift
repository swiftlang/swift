// RUN: %empty-directory(%t)
// RUN: %target-clang -x objective-c -c %S/Inputs/arc_36509461.m  -o %t/arc_36509461.m.o
// RUN: %target-swift-frontend -c -O -import-objc-header %S/Inputs/arc_36509461.h -sanitize=address %s -o %t/arc_36509461.swift.o
// RUN: %target-build-swift %t/arc_36509461.m.o %t/arc_36509461.swift.o -sanitize=address -o %t/arc_36509461
// RUN: %target-codesign %t/arc_36509461
// RUN: %target-run %t/arc_36509461

// REQUIRES: executable_test
// REQUIRES: asan_runtime
// REQUIRES: objc_interop
// REQUIRES: OS=macosx

// rdar://problem/47367694 tracks re-enabling this test for backward deployment.
// UNSUPPORTED: remote_run

import Foundation

struct FakeUUID {
  var bigTuple: (UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8)

  init() {
    bigTuple = (0, 0, 0, 0, 0, 0, 0, 0)
  }
}

struct Record {
  let name: String
  let uuid: FakeUUID
  let storage: NSObject

  init(storage: NSObject, name: String, uuid: FakeUUID) {
     self.name = name
     self.uuid = uuid
     self.storage = storage
  }

  func copy() -> Record {
    let copiedNSObject = NSObject()
        
    fake_apply(self.storage) { (key, value) -> Bool in
      let x = copiedNSObject
      return true
    }

    var record = Record(storage: copiedNSObject, name: self.name, uuid: self.uuid)
    return record
  }
}

@inline(never)
func foo(record: Record) -> Record {
  return record.copy()
}

func main() {
     let record = Record(storage: NSObject(), name: "", uuid: FakeUUID())
     _ = foo(record: record)
}

main()

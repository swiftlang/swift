// RUN: %target-run-simple-swift | %FileCheck %s
// REQUIRES: leak-checker

import StdlibUnittest

class VictimObject {
  var field1, field2: String
  var block: () -> () = {}

  init(field1: String, field2: String) {
    self.field1 = field1
    self.field2 = field2
    self.block = {
      self.field1 = self.field1 + self.field2
      self.field2 = self.field2 + self.field1
    }
  }
}

let LeaksTests = TestSuite("Leaks")

// CHECK: [ RUN      ] Leaks.Known leak
// CHECK: {"name":"Leaks", "swift_count": 2, "objc_count": 0, "swift_objects": [{"type": "nominal", "name": "C4main12VictimObject", "kind": "Class"},{"type": "unknown", "kind": "HeapLocalVariable"}], "objc_objects": []}
// CHECK: [       OK ] Leaks.Known leak
LeaksTests.test("Known leak") {
  _ = VictimObject(field1: "Leak", field2: "Checker")
}

runAllTests()


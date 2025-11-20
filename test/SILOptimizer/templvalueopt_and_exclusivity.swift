// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// RUN: %target-build-swift %t/module.swift -parse-as-library -O -enable-library-evolution -module-name=Module -emit-module -emit-module-path %t/Module.swiftmodule
// RUN: %target-build-swift %t/module.swift -parse-as-library -O -enable-library-evolution -module-name=Module -c -o %t/module.o
// RUN: %target-build-swift -parse-as-library -O %t/main.swift -I%t %t/module.o -o %t/a.out
// RUN: %target-codesign %t/a.out
// RUN: %target-run %t/a.out | %FileCheck %s

// REQUIRES: executable_test


// Check that TempLValueElimination does not cause an exclusivity violation at runtime.

//--- module.swift

public struct ResilientStruct {
  public init() {}
}

//--- main.swift

import Module

struct MyState {
  var _prop: String
  var prop: String
  {
    @storageRestrictions(initializes: _prop)
    init(initialValue) {
      _prop = initialValue
    }
    get {
      return _prop
    }
  }

  let resilient = ResilientStruct()

  init(prop: String) {
    self.prop = prop
  }
}

class Node {
  var state = MyState(prop: "initial value")

  func update(_ body: (inout MyState) -> Void) {
    body(&state)
  }
}

@inline(never)
func test_exclusivity() {
  let node = Node()
  node.update { state in
    state = MyState(prop: "new value")
  }
}

@main
enum App {
  static func main() {
    test_exclusivity()
    // CHECK: success
    print("success")
  }
}

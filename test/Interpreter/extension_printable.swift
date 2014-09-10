// RUN: %target-run-simple-swift | FileCheck %s
// XFAIL: interpret

// CHECK: start
println("start")

extension Int {
  class ExtensionClass : Printable {
    var description: String { return "abc" }
  }

  private class PrivateExtensionClass : Printable {
    var description: String { return "def" }
  }
}

@inline(never)
func testPrint(obj: AnyObject) {
  println(obj)
}

// CHECK: {{^}}abc{{$}}
testPrint(Int.ExtensionClass())
// CHECK-NEXT: {{^}}def{{$}}
testPrint(Int.PrivateExtensionClass())

// CHECK-NEXT: end
println("end")

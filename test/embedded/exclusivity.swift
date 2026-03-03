// RUN: %target-swift-frontend -emit-ir %s -enforce-exclusivity=checked -enable-experimental-feature Embedded -enable-experimental-feature EmbeddedDynamicExclusivity -parse-as-library | %FileCheck -check-prefix DYNAMIC %s
// RUN: %target-swift-frontend -parse-as-library -emit-ir %s -enforce-exclusivity=unchecked -enable-experimental-feature Embedded | %FileCheck -check-prefix STATIC-ONLY %s
// RUN: %target-swift-frontend -parse-as-library -emit-ir %s -enforce-exclusivity=checked -enable-experimental-feature Embedded | %FileCheck -check-prefix STATIC-ONLY %s
// RUN: %target-swift-frontend -parse-as-library -emit-ir %s -enable-experimental-feature Embedded | %FileCheck -check-prefix STATIC-ONLY %s

// REQUIRES: swift_in_compiler
// REQUIRES: swift_feature_Embedded
// REQUIRES: swift_feature_EmbeddedDynamicExclusivity

func f() -> Bool? { return nil }

public class MyClass {
  var handler: (()->())? = nil
  func foo() {
    handler?()
  }
  deinit { print("deinit") }
}

@main
struct Main {
  static var o: MyClass? = nil

  // CHECK: 4main
  static func main() {
    // DYNAMIC: call void @swift_beginAccess
    // STATIC-ONLY-NOT: @swift_beginAccess
    o = MyClass()
    o!.handler = { print("no captures") }
    o!.foo() // CHECK: no captures
    o = nil // CHECK: deinit

    var local = 42
    o = MyClass()
    o!.handler = { print("capture local"); local += 1 }
    o!.foo() // CHECK: capture local
    print(local == 43 ? "43" : "???") // CHECK: 43
    o = nil // CHECK: deinit

    let closure = {
         guard var b = f() else { print("success"); return }
         let c = { b = true }
         _ = (b, c)
    }
    closure()   // CHECK: success
  }
}

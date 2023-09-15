// RUN: %target-swiftc_driver %s -g -Onone -emit-ir | %FileCheck %s

// Check that unused functions are preserved at Onone.
func unused() {
}

// Property wrappers generate transparent getters, which we would like to check still exist at Onone.
@propertyWrapper
struct IntWrapper {
  private var storage = 42
    var wrappedValue: Int {
      return storage
    }
}

public class User {
  @IntWrapper private var number: Int

  func f() {
    // Force the generation of the getter
    _ = self.number 
  }
}
let c = User()
c.f()

// CHECK: !DISubprogram(name: "unused", linkageName: "$s21preserve_for_debugger6unusedyyF"
// CHECK: !DISubprogram(name: "number.get"

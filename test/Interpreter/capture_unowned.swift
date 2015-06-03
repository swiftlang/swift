// RUN: %target-run-simple-swift | FileCheck %s
// REQUIRES: executable_test

class C {
   let name: String
   init(name: String) { self.name = name }
   lazy var asString: () -> String = { [unowned self] in return self.name }
   deinit { print("deinitializing...") }
}
var c: C? = C(name: "I am a C")
print(c!.asString())
c = nil

// CHECK: I am a C
// CHECK-NEXT: deinitializing...

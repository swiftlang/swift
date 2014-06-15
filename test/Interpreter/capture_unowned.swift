// RUN: %target-run-simple-swift | FileCheck %s

class C {
   let name: String
   init(name: String) { self.name = name }
   @lazy var asString: () -> String = { [unowned self] in return self.name }
   deinit { println("deinitializing...") }
}
var c: C? = C(name: "I am a C")
println(c!.asString())
c = nil

// CHECK: I am a C
// CHECK-NEXT: deinitializing...

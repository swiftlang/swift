// RUN: %swift-ide-test -structure -source-filename %s | FileCheck %s

// CHECK: Class at [[@LINE+1]]:1 - [[@LINE+33]]:2, name at [[@LINE+1]]:7 - [[@LINE+1]]:12, inherited types at [[@LINE+1]]:15 - [[@LINE+1]]:25
class MyCls : OtherClass {
  // CHECK: Property at [[@LINE+1]]:3 - [[@LINE+1]]:16, name at [[@LINE+1]]:7 - [[@LINE+1]]:10
  var bar : Int

  // CHECK: Property at [[@LINE+1]]:3 - [[@LINE+1]]:28, name at [[@LINE+1]]:7 - [[@LINE+1]]:17
  var anotherBar : Int = 42

  // CHECK: CVar at [[@LINE+1]]:3 - [[@LINE+1]]:27, name at [[@LINE+1]]:13 - [[@LINE+1]]:17
  class var cbar : Int = 0

  // CHECK: IFunc at [[@LINE+4]]:3 - [[@LINE+14]]:4, name at [[@LINE+4]]:8 - [[@LINE+4]]:55
  // CHECK: Parameter at [[@LINE+3]]:12 - [[@LINE+3]]:16
  // CHECK: Parameter at [[@LINE+2]]:23 - [[@LINE+2]]:27, name at [[@LINE+2]]:23 - [[@LINE+2]]:27
  // CHECK: Parameter at [[@LINE+1]]:37 - [[@LINE+1]]:46, name at [[@LINE+1]]:37 - [[@LINE+1]]:42
  func foo(arg1: Int, name: String, param par: String) {
    var abc
    // CHECK: Brace at [[@LINE+1]]:10 - [[@LINE+7]]:6
    if 1 {
      // CHECK: Call at [[@LINE+4]]:7 - [[@LINE+4]]:41, name at [[@LINE+4]]:7 - [[@LINE+4]]:10
      // CHECK: Parameter at [[@LINE+3]]:11 - [[@LINE+3]]:12
      // CHECK: Parameter at [[@LINE+2]]:14 - [[@LINE+2]]:19, name at [[@LINE+2]]:14 - [[@LINE+2]]:18
      // CHECK: Parameter at [[@LINE+1]]:27 - [[@LINE+1]]:33, name at [[@LINE+1]]:27 - [[@LINE+1]]:32
      foo(1, name:"test", param:"test2")
    }
  }

  // CHECK: IFunc at [[@LINE+2]]:3 - [[@LINE+2]]:16, name at [[@LINE+2]]:3 - [[@LINE+2]]:16
  // CHECK: Parameter at [[@LINE+1]]:9 - [[@LINE+1]]:10, name at [[@LINE+1]]:9 - [[@LINE+1]]:10
  init (x: Int)

  // CHECK: CFunc at [[@LINE+1]]:3 - [[@LINE+1]]:20, name at [[@LINE+1]]:14 - [[@LINE+1]]:20
  class func cfoo()
}

//CHECK: Struct at [[@LINE+1]]:1 - [[@LINE+10]]:2, name at [[@LINE+1]]:8 - [[@LINE+1]]:15
struct MyStruc {
    //CHECK: Property at [[@LINE+1]]:5 - [[@LINE+1]]:19, name at [[@LINE+1]]:9 - [[@LINE+1]]:14
    var myVar: Int

    // CHECK: SVar at [[@LINE+1]]:5 - [[@LINE+1]]:30, name at [[@LINE+1]]:16 - [[@LINE+1]]:20
    static var sbar : Int = 0

    // CHECK: SFunc at [[@LINE+1]]:5 - [[@LINE+1]]:23, name at [[@LINE+1]]:17 - [[@LINE+1]]:23
    static func cfoo()
}

//CHECK: Protocol at [[@LINE+1]]:1 - [[@LINE+4]]:2, name at [[@LINE+1]]:10 - [[@LINE+1]]:16
protocol MyProt {
    //CHECK: IFunc at [[@LINE+1]]:5 - [[@LINE+1]]:15, name at [[@LINE+1]]:10 - [[@LINE+1]]:15
    func foo()
}

//CHECK: Extension at [[@LINE+1]]:1 - [[@LINE+5]]:2, name at [[@LINE+1]]:11 - [[@LINE+1]]:18
extension MyStruc {
    //CHECK: IFunc at [[@LINE+1]]:5 - [[@LINE+2]]:6, name at [[@LINE+1]]:10 - [[@LINE+1]]:15
    func foo() {
    }
}

// CHECK: GVar at [[@LINE+1]]:1 - [[@LINE+1]]:19, name at [[@LINE+1]]:5 - [[@LINE+1]]:9
var gvar : Int = 0

// CHECK: FFunc at [[@LINE+1]]:1 - [[@LINE+1]]:15, name at [[@LINE+1]]:6 - [[@LINE+1]]:12
func ffoo() {}

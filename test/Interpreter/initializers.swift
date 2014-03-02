// RUN: %target-run-simple-swift | FileCheck %s

// Test initialization and initializer inheritance.
var depth = 0

func printAtDepth(s: String) {
  for i in 0...depth { print("*") }
  println(s)
}

class A {
  var int: Int
  var string: String

  init() -> Self {
    printAtDepth("Starting A.init()")
    ++depth
    self.init(withInt:5)
    --depth
    printAtDepth("Ending A.init()")
  }

  init withInt(i:Int) -> Self {
    printAtDepth("Starting A.init withInt(\(i))")
    ++depth
    self.init(withInt:i, string:"hello")
    --depth
    printAtDepth("Ending A.init withInt(\(i))")
  }

  init withInt(i:Int) string(String) {
    printAtDepth("Starting A.init withInt(\(i)) string(\(string))")
    self.int = i
    self.string = string
    printAtDepth("Ending A.init withInt(\(i)) string(\(string))")
  }

  deinit {
    printAtDepth("A.deinit")
  }
}

class B : A {
  var double: Double

  init withInt(i:Int) string(String) -> Self {
    printAtDepth("Starting B.init withInt(\(i)) string(\(string))")
    ++depth
    self.init(withInt: i, string:string, double:3.14159)
    --depth
    printAtDepth("Ending B.init withInt(\(i)) string(\(string))")
  }

  init withInt(i:Int) string(String) double(Double) {
    printAtDepth("Starting B.init withInt(\(i)) string(\(string)) double(\(double))")
    self.double = double
    ++depth
    super.init(withInt: i, string: string)
    --depth
    printAtDepth("Ending B.init withInt(\(i)) string(\(string)) double(\(double))")
  }

  deinit {
    printAtDepth("B.deinit")
  }
}

class C : B {
  init withInt(i:Int) string(String) double(Double) {
    printAtDepth("Starting C.init withInt(\(i)) string(\(string)) double(\(double))")
    ++depth
    super.init(withInt: i, string: string, double: double)
    --depth
    printAtDepth("Ending C.init withInt(\(i)) string(\(string)) double(\(double))")
  }

  deinit {
    printAtDepth("C.deinit")
  }
}

println("-----Constructing C()-----")
// CHECK: Starting A.init()
// CHECK: *Starting A.init withInt(5)
// CHECK: **Starting B.init withInt(5) string(hello)
// CHECK: ***Starting C.init withInt(5) string(hello) double(3.14159)
// CHECK: ****Starting B.init withInt(5) string(hello) double(3.14159)
// CHECK: *****Starting A.init withInt(5) string(hello)
// CHECK: *****Ending A.init withInt(5) string(hello)
// CHECK: ****Ending B.init withInt(5) string(hello) double(3.14159)
// CHECK: ***Ending C.init withInt(5) string(hello) double(3.14159)
// CHECK: **Ending B.init withInt(5) string(hello)
// CHECK: *Ending A.init withInt(5)
// CHECK: Ending A.init()
// CHECK: C.deinit
// CHECK: B.deinit
// CHECK: A.deinit
C()

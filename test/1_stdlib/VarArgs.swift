// RUN: %swift -interpret -parse-stdlib -sdk %sdk -target %target-triple %s | FileCheck %s

// REQUIRES: swift_interpreter
// XFAIL: linux

// FIXME: iOS fails: target-run-stdlib-swift gets 'unknown identifier VarArgs'

import Swift

@asmname("vprintf")
func c_vprintf(format: UnsafePointer<Int8>, args: CVaListPointer)

func printf(format: String, arguments: CVarArgType...) {
  withVaList(arguments) {
    c_vprintf(format, $0)
  }
}

func test_varArgs0() {
  // CHECK: The answer to life and everything is 42, 42, -42, 3.14
  VarArgs.printf(
    "The answer to life and everything is %ld, %u, %d, %f\n",
    42, UInt32(42), Int16(-42), 3.14159279)
}
test_varArgs0()

func test_varArgs1() {
  var args = [CVarArgType]()

  var format = "dig it: "
  for i in 0..<12 {
    args.append(Int16(-i))
    args.append(Float(i))
    format += "%d %2g "
  }
  
  // CHECK: dig it: 0  0 -1  1 -2  2 -3  3 -4  4 -5  5 -6  6 -7  7 -8  8 -9  9 -10 10 -11 11
  withVaList(args) {
    c_vprintf(format + "\n", $0)
  }
}
test_varArgs1()

func test_varArgs3() {
  var args = [CVarArgType]()

  let format = "pointers: '%p' '%p' '%p' '%p' '%p'\n"
  args.append(COpaquePointer(bitPattern: 0x1234_5670))
  args.append(CFunctionPointer<() -> ()>(COpaquePointer(bitPattern: 0x1234_5671)))
  args.append(UnsafePointer<Int>(bitPattern: 0x1234_5672))
  args.append(UnsafeMutablePointer<Float>(bitPattern: 0x1234_5673))
  args.append(AutoreleasingUnsafeMutablePointer<AnyObject>(
        UnsafeMutablePointer<AnyObject>(bitPattern: 0x1234_5674)))

  // CHECK: {{pointers: '(0x)?0*12345670' '(0x)?0*12345671' '(0x)?0*12345672' '(0x)?0*12345673' '(0x)?0*12345674'}}
  withVaList(args) {
    c_vprintf(format, $0)
  }
}
test_varArgs3()

// CHECK: done.
println("done.")

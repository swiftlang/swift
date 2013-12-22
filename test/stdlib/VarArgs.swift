// RUN: rm -rf %t/clang-module-cache
// RUN: %swift -i -parse-stdlib -module-cache-path=%t/clang-module-cache -sdk=%sdk %s | FileCheck %s
// REQUIRES: swift_interpreter
import swift

func hexAddr(x: AnyObject) -> String {
  return hexAddr(Builtin.bridgeToRawPointer(Builtin.castToObjectPointer(x)))
}

func hexAddr(x: Builtin.RawPointer) -> String {
  return "@0x" + Int(Builtin.ptrtoint_Int64(x)).format('x', "")
}

func hexAddr<T>(p: UnsafePointer<T>) -> String {
  return hexAddr(p.value)
}

func hexAddr(p: COpaquePointer) -> String {
  return hexAddr(p.value)
}

@asmname="vprintf"
func c_vprintf(format: CString, args: COpaquePointer)

func printf(format: String, arguments: CVarArg...) {
  format.withCString {
    format in
    withVaList(arguments) {
      arguments in
      c_vprintf(format, arguments)
    }
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
  var args = makeC_va_list()

  var format = "dig it: "
  for i in 0..12 {
    args.append(Int16(-i))
    args.append(Float(i))
    format += "%d %2g "
  }
  // CHECK: dig it: 0  0 -1  1 -2  2 -3  3 -4  4 -5  5 -6  6 -7  7 -8  8 -9  9 -10 10 -11 11
  (format + "\n").withCString {
    c_vprintf($0, args)
  }
}
test_varArgs1()

// CHECK: done.
println("done.")

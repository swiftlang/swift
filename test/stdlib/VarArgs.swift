// RUN: rm -rf %t/clang-module-cache
// RUN: %swift -i -parse-stdlib -module-cache-path %t/clang-module-cache -sdk %sdk %s | FileCheck %s
// REQUIRES: swift_interpreter

// FIXME: iOS fails: target-run-stdlib-swift gets 'unknown identifier VarArgs'

import Swift

func hexAddr(x: AnyObject) -> String {
  return hexAddr(Builtin.bridgeToRawPointer(Builtin.castToNativeObject(x)))
}

func hexAddr(x: Builtin.RawPointer) -> String {
  return "@0x" + _uint64ToString(UInt64(UWord(Builtin.ptrtoint_Word(x))), radix: 16)
}

func hexAddr<T>(p: UnsafePointer<T>) -> String {
  return hexAddr(p.value)
}

func hexAddr(p: COpaquePointer) -> String {
  return hexAddr(p.value)
}

@asmname("vprintf")
func c_vprintf(format: CString, args: CVaListPointer)

func printf(format: String, arguments: CVarArg...) {
  format.withCString {
    format in
    withVaList(arguments) {
      c_vprintf(format, $0)
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
  var args = VaListBuilder()

  var format = "dig it: "
  for i in 0..12 {
    args.append(Int16(-i))
    args.append(Float(i))
    format += "%d %2g "
  }
  
  // CHECK: dig it: 0  0 -1  1 -2  2 -3  3 -4  4 -5  5 -6  6 -7  7 -8  8 -9  9 -10 10 -11 11
  (format + "\n").withCString {
    formatString in
    withVaList(args) {
      c_vprintf(formatString, $0)
    }
  }
}
test_varArgs1()

// CHECK: done.
println("done.")

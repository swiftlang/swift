// RUN: %target-run-stdlib-swift -parse-stdlib %s | FileCheck %s
// REQUIRES: executable_test

// XFAIL: linux

import Swift
import CoreGraphics

@asmname("vprintf")
func c_vprintf(format: UnsafePointer<Int8>, _ args: CVaListPointer)

func my_printf(format: String, _ arguments: CVarArgType...) {
  withVaList(arguments) {
    c_vprintf(format, $0)
  }
}

func test_varArgs0() {
  // CHECK: The answer to life and everything is 42, 42, -42, 3.14
  my_printf(
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
  args.append(COpaquePointer(bitPattern: 0x1234_5671))
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

func test_varArgs4() {
  // Verify alignment of va_list contents.
  // On some architectures some types are better-
  // aligned than Word and must be packaged with care.

  let  i8 = Int8(1)
  let i16 = Int16(2)
  let i32 = Int32(3)
  let i64 = 4444444444444444 as Int64

  let  u8 = UInt8(10)
  let u16 = UInt16(20)
  let u32 = UInt32(30)
  let u64 = 4040404040404040 as UInt64

  let f32 = Float(1.1)
  let f64 = Double(2.2)
  let fCG = CGFloat(3.3)

  my_printf("a %g %d %g %d %g %d a\n",            f32, i8, f64, i8, fCG, i8)
  my_printf("b %d %g %d %g %d %g %d b\n",     i8, f32, i8, f64, i8, fCG, i8)
  my_printf("c %d %d %d %d %d %lld %d c\n",       i8, i16, i8, i32, i8, i64, i8)
  my_printf("d %d %d %d %d %d %d %lld %d d\n",i8, i8, i16, i8, i32, i8, i64, i8)
  my_printf("e %u %u %u %u %u %llu %u e\n",       u8, u16, u8, u32, u8, u64, u8)
  my_printf("f %u %u %u %u %u %u %llu %u f\n",u8, u8, u16, u8, u32, u8, u64, u8)
  // CHECK: a 1.1 1 2.2 1 3.3 1 a
  // CHECK: b 1 1.1 1 2.2 1 3.3 1 b
  // CHECK: c 1 2 1 3 1 4444444444444444 1 c
  // CHECK: d 1 1 2 1 3 1 4444444444444444 1 d
  // CHECK: e 10 20 10 30 10 4040404040404040 10 e
  // CHECK: f 10 10 20 10 30 10 4040404040404040 10 f
}
test_varArgs4()

// CHECK: done.
print("done.")

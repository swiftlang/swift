// RUN: %empty-directory(%t)
//
// RUN: %target-clang -x c %S/Inputs/sensitive_c_functions.c -c -o %t/sensitive_c_functions.o
//
// RUN: %target-build-swift -target %module-target-future -module-name=test -enable-experimental-feature Sensitive -parse-as-library -Onone -import-objc-header %S/Inputs/sensitive.h %s %t/sensitive_c_functions.o -o %t/a.out
// RUN: %target-codesign %t/a.out
// RUN: %target-run %t/a.out | %FileCheck %s
//
// RUN: %target-build-swift -target %module-target-future -module-name=test -enable-experimental-feature Sensitive -parse-as-library -O -import-objc-header %S/Inputs/sensitive.h %s %t/sensitive_c_functions.o -o %t/ao.out
// RUN: %target-codesign %t/ao.out
// RUN: %target-run %t/ao.out | %FileCheck %s
//
// Test with memset_s which uses the compiler if of swift_clearSensitive is not available:
// RUN: %target-build-swift -module-name=test -enable-experimental-feature Sensitive -parse-as-library -Onone -import-objc-header %S/Inputs/sensitive.h %s %t/sensitive_c_functions.o -o %t/a.out
// RUN: %target-codesign %t/a.out
// RUN: %target-run %t/a.out | %FileCheck %s
//
// REQUIRES: executable_test
// REQUIRES: swift_feature_Sensitive
// UNSUPPORTED: use_os_stdlib

var checkBuffer: UnsafeBufferPointer<UInt32>?

@inline(never)
func checkLater<T>(_ t: inout T) {
  withUnsafePointer(to: &t) {
    let size = MemoryLayout<T>.size / MemoryLayout<UInt32>.size
    $0.withMemoryRebound(to: UInt32.self, capacity: size) {
      checkBuffer = UnsafeBufferPointer(start: $0, count: size)
    }
  }
}

@inline(never)
func printit<T>(_ t: inout T) {
  print(t)
}

@inline(never)
func consumeAndPrint<T>(_ t: consuming T) {
  print(t)
}

@inline(never)
func check() {
  for b in checkBuffer! {
    precondition(b != 0xdeadbeaf)
  }
}

@inline(never)
func printSensitive<T>(_ t: T) {
  do {
    var x: T = t
    checkLater(&x)
    printit(&x)
  }
  check()
  print() // to prevent tail call of `check()`
}

@inline(never)
func printConsumed<T>(_ t: T) {
  do {
    var x: T = t
    checkLater(&x)
    consumeAndPrint(x)
  }
  check()
  print() // to prevent tail call of `check()`
}


@inline(never)
func checkSmallCStructs() {
  do {
    var x = getSmallStruct(27)
    checkLater(&x)
    x = forwardSmallStruct(x);
    printSmallStruct(27, x, 28)
  }
  check()
  print() // to prevent tail call of `check()`
}

@inline(never)
func checkLargeCStructs() {
  do {
    var x = getLargeStruct(29)
    checkLater(&x)
    x = forwardLargeStruct(x);
    printLargeStruct(30, x, 31)
  }
  check()
  print() // to prevent tail call of `check()`
}

protocol P {}

@sensitive
struct SensitiveStruct {
  var a: UInt32 = 0xdeadbeaf
  var b: UInt32 = 0xdeadbeaf
  var c: UInt32 = 0xdeadbeaf
}

// A struct which would be address-only also without @sensitive
@sensitive
struct NonLoadableSensitiveStruct {
  var a: UInt32 = 0xdeadbeaf
  var b: UInt32 = 0xdeadbeaf
  var c: UInt32 = 0xdeadbeaf
  let p: P
}

struct X : P {}

struct Container<T> {
  var x = 123
  let t: T
  var y = 456
}

@main struct Main {
  static func main() {
    // CHECK: SensitiveStruct(a: 3735928495, b: 3735928495, c: 3735928495)
    printSensitive(SensitiveStruct())

    // CHECK: SensitiveStruct(a: 3735928495, b: 3735928495, c: 3735928495)
    printConsumed(SensitiveStruct())

    // CHECK: Optional(test.SensitiveStruct(a: 3735928495, b: 3735928495, c: 3735928495))
    printSensitive(Optional(SensitiveStruct()))

    // CHECK: NonLoadableSensitiveStruct(a: 3735928495, b: 3735928495, c: 3735928495, p: test.X())
    printSensitive(NonLoadableSensitiveStruct(p: X()))

    // CHECK: Container<SensitiveStruct>(x: 123, t: test.SensitiveStruct(a: 3735928495, b: 3735928495, c: 3735928495), y: 456)
    printSensitive(Container(t: SensitiveStruct()))

    // CHECK: x = 27
    // CHECK-NEXT: x = 27, y = 28
    // CHECK-NEXT: s = (3735928495, 3735928495, 3735928495)
    checkSmallCStructs();

    // CHECK: x = 29
    // CHECK-NEXT: x = 30, y = 31
    // CHECK-NEXT: s = (3735928495, 3735928495, 3735928495, 3735928495, 3735928495, 3735928495, 3735928495)
    checkLargeCStructs();
  }
}

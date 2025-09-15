// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend %s -enable-experimental-feature Extern -enable-experimental-feature Embedded -c -o %t/main.o
// RUN: %target-clang -x c -c %S/Inputs/print.c -o %t/print.o
// RUN: %target-clang %t/main.o %t/print.o -o %t/a.out -dead_strip
// RUN: %target-run %t/a.out | %FileCheck %s

// REQUIRES: swift_in_compiler
// REQUIRES: executable_test
// REQUIRES: optimized_stdlib
// REQUIRES: OS=macosx || OS=linux-gnu
// REQUIRES: swift_feature_Embedded
// REQUIRES: swift_feature_Extern

@_extern(c, "putchar")
@discardableResult
func putchar(_: CInt) -> CInt

public func print(_ s: StaticString, terminator: StaticString = "\n") {
  var p = s.utf8Start
  while p.pointee != 0 {
    putchar(CInt(p.pointee))
    p += 1
  }
  p = terminator.utf8Start
  while p.pointee != 0 {
    putchar(CInt(p.pointee))
    p += 1
  }
}

@_silgen_name("print_long")
func print_long(_: Int)

public func print(_ n: Int, terminator: StaticString = "\n") {
    print_long(n)
    print("", terminator: terminator)
}

func print(_ array: [Int]) {
    print("[", terminator: "")
    for i in 0 ..< array.count {
        print_long(array[i])
        if i != array.count - 1 { print(", ", terminator: "") }
    }
    print("]")
}

@_silgen_name("memcmp")
func memcmp(_: UnsafeRawPointer, _: UnsafeRawPointer, _: Int) -> CInt

@_silgen_name("memcpy")
func memcpy(_: UnsafeMutableRawPointer, _: UnsafeRawPointer, _: Int) -> UnsafeRawPointer

func test() {
  print("Calling memcmp")
  // CHECK: Calling memcmp

  // const arrays
  print(Int(memcmp([1, 2, 3], [1, 2, 3], MemoryLayout<Int>.size * 3)))
  print(Int(memcmp([1, 2, 3], [1, 2, 4], MemoryLayout<Int>.size * 3)))
  // CHECK-NEXT: 0
  // CHECK-NEXT: 1

  let a1 = [1, 2, 3]
  let a2 = [1, 2, 4]
  print(Int(memcmp(a1, a2, MemoryLayout<Int>.size * 3)))
  // CHECK-NEXT: 1

  // mutable array
  var arr = [1, 2, 3, 4, 5]
  var int42 = 42
  memcpy(&arr, &int42, MemoryLayout<Int>.size)
  print(arr)
  // CHECK-NEXT: [42, 2, 3, 4, 5]
}

test()

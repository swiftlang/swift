// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -enable-experimental-feature Extern %s -enable-experimental-feature Embedded -c -o %t/main.o
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

func isArray<S: Sequence>(_ s: S) -> Bool where S.Element == Int {
  return s is [Int]
}

public func testIsArray() -> Bool {
  return isArray(Array<Int>())
}

public func testIsNotArray() -> Bool {
  return isArray(Array<Int>().lazy)
}

func test() {
  var a = [1, 2, 3]
  a.append(8)
  a.append(contentsOf: [5, 4])
  let b = a.sorted()
  var c = b
  c = c.reversed().filter { $0 % 2 == 0 }
  print(c) // CHECK: [8, 4, 2]

  // CHECK: yes
  print(testIsArray() ? "yes" : "no")
  // CHECK: no
  print(testIsNotArray() ? "yes" : "no")
}

test()

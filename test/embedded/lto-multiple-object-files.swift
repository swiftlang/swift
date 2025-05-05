// Regression test for a runtime failure that used to happen under some very specific conditions: (1) LTO, (2) Swift
// compiler is used with -num-threads to emit multiple .o files even in WMO mode, (3) the first listed source file
// contains no code that would trigger the use of swift_initStaticObject runtime function (e.g. no array literals), (4)
// another source file does use swift_initStaticObject that doesn't get optimized away (e.g. an array literal in a
// never-inline function, but not an empty array). In that case, we used to have a mismatching calling convention on
// swift_initStaticObject that the LTO pipeline would conclude is invalid IR and replace it with an unconditional trap.

// RUN: %empty-directory(%t)
// RUN: %{python} %utils/split_file.py -o %t %s

// RUN: %target-swift-frontend -Osize -lto=llvm-full %t/MyFile1.swift %t/MyFile2.swift -enable-experimental-feature Embedded -emit-bc -num-threads 2 -o %t/MyFile1.o -o %t/MyFile2.o
// RUN: %target-clang -Oz %t/MyFile1.o %t/MyFile2.o -o %t/a.out
// RUN: %target-run %t/a.out | %FileCheck %s

// REQUIRES: swift_in_compiler
// REQUIRES: executable_test
// REQUIRES: optimized_stdlib

// For LTO, the linker dlopen()'s the libLTO library, which is a scenario that
// ASan cannot work in ("Interceptors are not working, AddressSanitizer is
// loaded too late").
// REQUIRES: no_asan
// REQUIRES: swift_feature_Embedded

// BEGIN MyFile1.swift

public func foo() {
  print("foo")
}

// BEGIN MyFile2.swift

public func bar() {
  print("bar")
}

@inline(never)
public func baz(array: [Int]) -> ([Int], [Int]) {
  let x = [1, 2, 3]
  let y = [3, 4, 5]
  return (x, y)
}

@main
struct Main {
  static func main() {
    foo()
    let (x, y) = baz(array: Array<Int>.init(repeating: 0, count: 8))
    let c = x.count + y.count
    print(c)
    bar()
  }
}

// CHECK: foo
// CHECK-NEXT: 6
// CHECK-NEXT: bar

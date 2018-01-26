// RUN: %target-swift-frontend  -primary-file %s -O -sil-verify-all -Xllvm -sil-disable-pass=FunctionSignatureOpts -module-name=test -emit-sil | %FileCheck %s

// Also do an end-to-end test to check all components, including IRGen.
// RUN: %empty-directory(%t) 
// RUN: %target-build-swift -O -Xllvm -sil-disable-pass=FunctionSignatureOpts -module-name=test %s -o %t/a.out
// RUN: %target-run %t/a.out | %FileCheck %s -check-prefix=CHECK-OUTPUT
// REQUIRES: executable_test,swift_stdlib_no_asserts,optimized_stdlib

// Check if the optimizer is able to convert array literals to statically initialized arrays.

// CHECK-LABEL: outlined variable #0 of arrayLookup(_:)
// CHECK-NEXT:  sil_global private @{{.*}}arrayLookup{{.*}} = {
// CHECK-DAG:     integer_literal $Builtin.Int{{[0-9]+}}, 10
// CHECK-DAG:     integer_literal $Builtin.Int{{[0-9]+}}, 11
// CHECK-DAG:     integer_literal $Builtin.Int{{[0-9]+}}, 12
// CHECK:         object {{.*}} ({{[^,]*}}, [tail_elems] {{[^,]*}}, {{[^,]*}}, {{[^,]*}})
// CHECK-NEXT:  }

// CHECK-LABEL: outlined variable #0 of returnArray()
// CHECK-NEXT:  sil_global private @{{.*}}returnArray{{.*}} = {
// CHECK-DAG:     integer_literal $Builtin.Int{{[0-9]+}}, 20
// CHECK-DAG:     integer_literal $Builtin.Int{{[0-9]+}}, 21
// CHECK:         object {{.*}} ({{[^,]*}}, [tail_elems] {{[^,]*}}, {{[^,]*}})
// CHECK-NEXT:  }

// CHECK-LABEL: outlined variable #0 of returnStaticStringArray()
// CHECK-NEXT:  sil_global private @{{.*}}returnStaticStringArray{{.*}} = {
// CHECK-DAG:     string_literal utf8 "a"
// CHECK-DAG:     string_literal utf8 "b"
// CHECK:         object {{.*}} ({{[^,]*}}, [tail_elems] {{[^,]*}}, {{[^,]*}})
// CHECK-NEXT:  }

// CHECK-LABEL: outlined variable #0 of passArray()
// CHECK-NEXT:  sil_global private @{{.*}}passArray{{.*}} = {
// CHECK-DAG:     integer_literal $Builtin.Int{{[0-9]+}}, 27
// CHECK-DAG:     integer_literal $Builtin.Int{{[0-9]+}}, 28
// CHECK:         object {{.*}} ({{[^,]*}}, [tail_elems] {{[^,]*}}, {{[^,]*}})
// CHECK-NEXT:  }

// CHECK-LABEL: outlined variable #1 of passArray()
// CHECK-NEXT:  sil_global private @{{.*}}passArray{{.*}} = {
// CHECK:         integer_literal $Builtin.Int{{[0-9]+}}, 29
// CHECK:         object {{.*}} ({{[^,]*}}, [tail_elems] {{[^,]*}})
// CHECK-NEXT:  }

// CHECK-LABEL: outlined variable #0 of storeArray()
// CHECK-NEXT:  sil_global private @{{.*}}storeArray{{.*}} = {
// CHECK-DAG:     integer_literal $Builtin.Int{{[0-9]+}}, 227
// CHECK-DAG:     integer_literal $Builtin.Int{{[0-9]+}}, 228
// CHECK:         object {{.*}} ({{[^,]*}}, [tail_elems] {{[^,]*}}, {{[^,]*}})
// CHECK-NEXT:  }

// CHECK-LABEL: sil_global private @{{.*}}main{{.*}} = {
// CHECK-DAG:     integer_literal $Builtin.Int{{[0-9]+}}, 100
// CHECK-DAG:     integer_literal $Builtin.Int{{[0-9]+}}, 101
// CHECK-DAG:     integer_literal $Builtin.Int{{[0-9]+}}, 102
// CHECK:         object {{.*}} ({{[^,]*}}, [tail_elems] {{[^,]*}}, {{[^,]*}}, {{[^,]*}})
// CHECK-NEXT:  }

// CHECK-LABEL: sil @main
// CHECK:   global_value @{{.*}}main{{.*}}
// CHECK:   return
public let globalVariable = [ 100, 101, 102 ]

// CHECK-LABEL: sil {{.*}}arrayLookup{{.*}} : $@convention(thin) (Int) -> Int {
// CHECK:   global_value @{{.*}}arrayLookup{{.*}}
// CHECK:   return
@inline(never)
public func arrayLookup(_ i: Int) -> Int {
  let lookupTable = [10, 11, 12]
  return lookupTable[i]
}

// CHECK-LABEL: sil {{.*}}returnArray{{.*}} : $@convention(thin) () -> @owned Array<Int> {
// CHECK:   global_value @{{.*}}returnArray{{.*}}
// CHECK:   return
@inline(never)
public func returnArray() -> [Int] {
  return [20, 21]
}

// CHECK-LABEL: sil {{.*}}returnStaticStringArray{{.*}} : $@convention(thin) () -> @owned Array<StaticString> {
// CHECK:   global_value @{{.*}}returnStaticStringArray{{.*}}
// CHECK:   return
@inline(never)
public func returnStaticStringArray() -> [StaticString] {
  return ["a", "b"]
}

public var gg: [Int]?

@inline(never)
public func receiveArray(_ a: [Int]) {
  gg = a
}

// CHECK-LABEL: sil {{.*}}passArray{{.*}} : $@convention(thin) () -> () {
// CHECK:   global_value @{{.*}}passArray{{.*}}
// CHECK:   global_value @{{.*}}passArray{{.*}}
// CHECK:   return
@inline(never)
public func passArray() {
  receiveArray([27, 28])
  receiveArray([29])
}

// CHECK-LABEL: sil {{.*}}storeArray{{.*}} : $@convention(thin) () -> () {
// CHECK:   global_value @{{.*}}storeArray{{.*}}
// CHECK:   return
@inline(never)
public func storeArray() {
  gg = [227, 228]
}

struct Empty { }

// CHECK-LABEL: sil {{.*}}arrayWithEmptyElements{{.*}} : $@convention(thin) () -> @owned Array<Empty> {
func arrayWithEmptyElements() -> [Empty] {
  // CHECK:    global_value @{{.*}}arrayWithEmptyElements{{.*}}
  // CHECK:    return
  return [Empty()]
}

// CHECK-OUTPUT:      [100, 101, 102]
print(globalVariable)
// CHECK-OUTPUT-NEXT: 11
print(arrayLookup(1))
// CHECK-OUTPUT-NEXT: [20, 21]
print(returnArray())
// CHECK-OUTPUT-NEXT: ["a", "b"]
print(returnStaticStringArray())
passArray()
// CHECK-OUTPUT-NEXT: [29]
print(gg!)
storeArray()
// CHECK-OUTPUT-NEXT: [227, 228]
print(gg!)






public class SwiftClass {}

@inline(never)
func takeUnsafePointer(ptr : UnsafePointer<SwiftClass>, len: Int) {
  print(ptr, len)  // Use the arguments somehow so they don't get removed.
}

// This should be a single basic block, and the array should end up being stack
// allocated.
//
// CHECK-LABEL: sil @{{.*}}passArrayOfClasses
// CHECK: bb0(%0 : $SwiftClass, %1 : $SwiftClass, %2 : $SwiftClass):
// CHECK-NOT: bb1(
// CHECK: alloc_ref {{.*}}[tail_elems $SwiftClass *
// CHECK-NOT: bb1(
// CHECK:   return
public func passArrayOfClasses(a: SwiftClass, b: SwiftClass, c: SwiftClass) {
  let arr = [a, b, c]
  takeUnsafePointer(ptr: arr, len: arr.count)
}





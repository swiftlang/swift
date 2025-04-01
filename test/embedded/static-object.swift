// RUN: %target-swift-frontend -parse-as-library -enable-experimental-feature Embedded %s -O -wmo -sil-verify-all -module-name=test -emit-ir | %FileCheck %s

// Also do an end-to-end test to check all components, including IRGen.
// RUN: %empty-directory(%t) 
// RUN: %target-build-swift -parse-as-library -enable-experimental-feature Embedded -O -wmo -module-name=test %s -o %t/a.out
// RUN: %target-run %t/a.out | %FileCheck %s -check-prefix=CHECK-OUTPUT

// REQUIRES: executable_test,swift_stdlib_no_asserts,optimized_stdlib

// Check if the optimizer is able to convert array literals to constant statically initialized arrays.

// CHECK-DAG: @"$e4test11arrayLookupyS2iFTv_r" = {{.*}} constant {{.*}} @"$es20__StaticArrayStorageCN", {{.*}} -1
// CHECK-DAG: @"$e4test11returnArraySaySiGyFTv_r" = {{.*}} constant {{.*}} @"$es20__StaticArrayStorageCN", {{.*}} -1
// CHECK-DAG: @"$e4test9passArrayyyFTv_r" = {{.*}} constant {{.*}} @"$es20__StaticArrayStorageCN", {{.*}} -1
// CHECK-DAG: @"$e4test9passArrayyyFTv0_r" = {{.*}} constant {{.*}} @"$es20__StaticArrayStorageCN", {{.*}} -1
// CHECK-DAG: @"$e4test10storeArrayyyFTv_r" = {{.*}} constant {{.*}} @"$es20__StaticArrayStorageCN", {{.*}} -1
// CHECK-DAG: @"$e4test3StrV9staticLet_WZTv_r" = {{.*}} constant {{.*}} @"$es20__StaticArrayStorageCN", {{.*}} -1
// CHECK-DAG: @"$e4test3StrV9staticVar_WZTv_r" = {{.*}} constant {{.*}} @"$es20__StaticArrayStorageCN", {{.*}} -1
// CHECK-DAG: @"$e4test3StrV9staticVarSaySiGvpZ" = global {{.*}} ptr @"$e4test3StrV9staticVar_WZTv_r"
// CHECK-DAG: @"$e4test3StrV14twoDimensionalSaySaySiGGvpZ" = global {{.*}} ptr @"$e4test3StrV14twoDimensional_WZTv{{[0-9]*}}_r"

// REQUIRES: swift_feature_Embedded


public struct Str {
  public static let staticLet = [ 200, 201, 202 ]
  public static var staticVar = [ 300, 301, 302 ]
  public static var twoDimensional = [[1, 2], [3, 4], [5, 6]]
}

@inline(never)
public func arrayLookup(_ i: Int) -> Int {
  let lookupTable = [10, 11, 12]
  return lookupTable[i]
}

@inline(never)
public func returnArray() -> [Int] {
  return [20, 21]
}

@inline(never)
public func modifyArray() -> [Int] {
  var a = returnArray()
  a[1] = 27
  return a
}

public var gg: [Int]?

@inline(never)
public func receiveArray(_ a: [Int]) {
  gg = a
}

@inline(never)
public func passArray() {
  receiveArray([27, 28])
  receiveArray([29])
}

@inline(never)
public func storeArray() {
  gg = [227, 228]
}

public func stringArray() -> [StaticString] {
  return ["a", "b", "c", "d"]
}

@main struct Main {
  static func main() {

    // CHECK-OUTPUT:      [200, 201, 202]
    printArray(Str.staticLet)

    // CHECK-OUTPUT:      [300, 301, 302]
    printArray(Str.staticVar)

    // CHECK-OUTPUT:      [1, 2]
    // CHECK-OUTPUT-NEXT: [3, 4]
    // CHECK-OUTPUT-NEXT: [5, 6]
    for x in Str.twoDimensional {
      printArray(x)
    }

    // CHECK-OUTPUT-NEXT: 11
    print(arrayLookup(1))

    // CHECK-OUTPUT-NEXT: [20, 21]
    printArray(returnArray())

    // CHECK-OUTPUT-NEXT: [20, 27]
    // CHECK-OUTPUT-NEXT: [20, 27]
    // CHECK-OUTPUT-NEXT: [20, 27]
    // CHECK-OUTPUT-NEXT: [20, 27]
    // CHECK-OUTPUT-NEXT: [20, 27]
    for _ in 0..<5 {
      printArray(modifyArray())
    }

    passArray()
    // CHECK-OUTPUT-NEXT: [29]
    printArray(gg!)

    storeArray()
    // CHECK-OUTPUT-NEXT: [227, 228]
    printArray(gg!)

    for c in stringArray() {
      // CHECK-OUTPUT-NEXT: a
      // CHECK-OUTPUT-NEXT: b
      // CHECK-OUTPUT-NEXT: c
      // CHECK-OUTPUT-NEXT: d
      print(c)
    }
  }
}

func printArray(_ a: [Int]) {
  print("[", terminator: "")
  for (i, x) in a.enumerated() {
    print(x, terminator: i == a.count - 1 ? "" :  ", ")
  }
  print("]")
}


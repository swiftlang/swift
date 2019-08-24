// RUN: %empty-directory(%t)
// RUN: %target-build-swift %s -o %t/main
// RUN: %target-build-swift %s -o %t/main-optimized
// RUN: %target-codesign %t/main
// RUN: %target-codesign %t/main-optimized
// RUN: %target-run %t/main | %FileCheck %s
// RUN: %target-run %t/main-optimized | %FileCheck %s
// REQUIRES: executable_test

protocol Preening {
  func preen()
}

struct A : Preening, Hashable, Equatable {
  private var value: Int

  init(_ value: Int) { self.value = value }
  func preen() {
    print("A\(value)")
  }

  static func ==(lhs: A, rhs: A) -> Bool {
    return lhs.value == rhs.value
  }

  func hash(into hasher: inout Hasher) {
    hasher.combine(value)
  }
}

do {

print("Arrays.")
// CHECK: Arrays.

let a_array = [ A(5), A(10), A(20) ]
a_array.forEach { $0.preen() }
// CHECK-NEXT: A5
// CHECK-NEXT: A10
// CHECK-NEXT: A20

let preening_array_1 = a_array as [Preening]
preening_array_1.forEach { $0.preen() }
// CHECK-NEXT: A5
// CHECK-NEXT: A10
// CHECK-NEXT: A20

let any_array_1 = preening_array_1 as [Any]
print(any_array_1.count)
// CHECK-NEXT: 3

let preening_array_2 = any_array_1 as! [Preening]
preening_array_2.forEach { $0.preen() }
// CHECK-NEXT: A5
// CHECK-NEXT: A10
// CHECK-NEXT: A20

let preening_array_3 = any_array_1 as? [Preening]
preening_array_3?.forEach { $0.preen() }
// CHECK-NEXT: A5
// CHECK-NEXT: A10
// CHECK-NEXT: A20

let a_array_2 = any_array_1 as! [A]
a_array_2.forEach { $0.preen() }
// CHECK-NEXT: A5
// CHECK-NEXT: A10
// CHECK-NEXT: A20

let a_array_3 = any_array_1 as? [Preening]
a_array_3?.forEach { $0.preen() }
// CHECK-NEXT: A5
// CHECK-NEXT: A10
// CHECK-NEXT: A20

}

do {

print("Dictionaries.")
// CHECK-NEXT: Dictionaries.

let a_dict = ["one" : A(1), "two" : A(2), "three" : A(3)]
print("begin")
a_dict.forEach { $0.1.preen() }
print("end")
// CHECK-NEXT: begin
// CHECK-DAG: A1
// CHECK-DAG: A2
// CHECK-DAG: A3
// CHECK-NEXT: end

let preening_dict_1 = a_dict as [String: Preening]
print("begin")
preening_dict_1.forEach { $0.1.preen() }
print("end")
// CHECK-NEXT: begin
// CHECK-DAG: A1
// CHECK-DAG: A2
// CHECK-DAG: A3
// CHECK-NEXT: end

let any_dict_1 = preening_dict_1 as [String: Any]
print(any_dict_1.count)
// CHECK-NEXT: 3

let preening_dict_2 = any_dict_1 as! [String: Preening]
print("begin")
preening_dict_2.forEach { $0.1.preen() }
print("end")
// CHECK-NEXT: begin
// CHECK-DAG: A1
// CHECK-DAG: A2
// CHECK-DAG: A3
// CHECK-NEXT: end

let preening_dict_3 = any_dict_1 as? [String: Preening]
print("begin")
preening_dict_3?.forEach { $0.1.preen() }
print("end")
// CHECK-NEXT: begin
// CHECK-DAG: A1
// CHECK-DAG: A2
// CHECK-DAG: A3
// CHECK-NEXT: end

let a_dict_2 = any_dict_1 as! [String: A]
print("begin")
a_dict_2.forEach { $0.1.preen() }
print("end")
// CHECK-NEXT: begin
// CHECK-DAG: A1
// CHECK-DAG: A2
// CHECK-DAG: A3
// CHECK-NEXT: end

let a_dict_3 = any_dict_1 as? [String: A]
print("begin")
a_dict_3?.forEach { $0.1.preen() }
print("end")
// CHECK-NEXT: begin
// CHECK-DAG: A1
// CHECK-DAG: A2
// CHECK-DAG: A3
// CHECK-NEXT: end

}

// TODO: I can't think of any way to do this for sets and dictionary
// keys that doesn't involve bridging.

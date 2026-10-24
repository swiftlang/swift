// RUN: %target-run-simple-swift | %FileCheck %s
// REQUIRES: executable_test

protocol R1: A.P {}
struct S1: R1 { typealias T = Int }
extension S1.T {}

protocol R2: R1 {}
struct S2: R2 { typealias T = Int }
extension S2.T {}

enum A {}
extension A {
  protocol P { func p() -> String }
}
extension A.P { func p() -> String { "P.p" } }

func p(_ value: any A.P) -> String { value.p() }
func generic<T: R2>(_ value: T) -> String { value.p() }

// CHECK: P.p P.p P.p
print(p(S1()), p(S2()), generic(S2()))
// CHECK: true true
let values: [Any] = [S1(), S2()]
print(values.allSatisfy { $0 is any A.P }, values.allSatisfy { $0 is any R1 })

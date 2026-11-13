// RUN: %target-run-simple-swift | %FileCheck %s
// REQUIRES: executable_test

protocol Top { func top() -> String }
class Base: Top {
  func top() -> String { "Base.top" }
  func base() -> String { "Base.base" }
}

class C1: A.BaseAlias { typealias T = Int; override func base() -> String { "C1.base" } }
extension C1.T {}

class C2: C1 { typealias T = Int }
extension C2.T {}

enum A {}
extension A { typealias BaseAlias = Base }

func top(_ value: any Top) -> String { value.top() }
func base(_ value: Base) -> String { value.base() }

// CHECK: Base.top C1.base
print(top(C1()), base(C1()))
// CHECK: Base.top C1.base
print(top(C2()), base(C2()))
// CHECK: true true true
let values: [Any] = [C1(), C2()]
print(values.allSatisfy { $0 is any Top }, values.allSatisfy { $0 is Base },
      (C2() as AnyObject) is C1)

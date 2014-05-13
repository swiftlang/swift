// RUN: %target-run-simple-swift | FileCheck %s

class A {
  func printA() { print("A") }
}
class B : A {
  override func printA() { print("B") }
}

func printA(v: A) { v.printA() }
func printOpt<T>(subprint: T->())(x: T?) {
  switch (x) {
  case .Some(let y): print(".Some("); subprint(y); print(")")
  case .None: print(".None")
  }
}

func test(v: A????, cast: (A????) -> B?) {
  printOpt(printOpt(printOpt(printOpt(printA))))(x: v)
  print(" as B: ")
  printOpt(printA)(x: cast(v))
  print("\n")
}
test(.Some(.Some(.Some(.Some(A())))), { $0 as B })
test(.Some(.Some(.Some(.Some(B())))), { $0 as B })
test(.Some(.Some(.Some(.None))), { $0 as B })
test(.Some(.Some(.None)), { $0 as B })
test(.Some(.None), { $0 as B })
test(.None, { $0 as B })
// CHECK: .Some(.Some(.Some(.Some(A)))) as B: .None
// CHECK: .Some(.Some(.Some(.Some(B)))) as B: .Some(B)
// CHECK: .Some(.Some(.Some(.None))) as B: .None
// CHECK: .Some(.Some(.None)) as B: .None
// CHECK: .Some(.None) as B: .None
// CHECK: .None as B: .None

func test(v: A????, cast: (A????) -> B??) {
  printOpt(printOpt(printOpt(printOpt(printA))))(x: v)
  print(" as B?: ")
  printOpt(printOpt(printA))(x: cast(v))
  print("\n")
}
test(.Some(.Some(.Some(.Some(A())))), { $0 as B? })
test(.Some(.Some(.Some(.Some(B())))), { $0 as B? })
test(.Some(.Some(.Some(.None))), { $0 as B? })
test(.Some(.Some(.None)), { $0 as B? })
test(.Some(.None), { $0 as B? })
test(.None, { $0 as B? })
// CHECK: .Some(.Some(.Some(.Some(A)))) as B?: .None
// CHECK: .Some(.Some(.Some(.Some(B)))) as B?: .Some(.Some(B))
// CHECK: .Some(.Some(.Some(.None))) as B?: .Some(.None)
// CHECK: .Some(.Some(.None)) as B?: .None
// CHECK: .Some(.None) as B?: .None
// CHECK: .None as B?: .None

func test(v: A????, cast: (A????) -> B???) {
  printOpt(printOpt(printOpt(printOpt(printA))))(x: v)
  print(" as B??: ")
  printOpt(printOpt(printOpt(printA)))(x: cast(v))
  print("\n")
}
test(.Some(.Some(.Some(.Some(A())))), { $0 as B?? })
test(.Some(.Some(.Some(.Some(B())))), { $0 as B?? })
test(.Some(.Some(.Some(.None))), { $0 as B?? })
test(.Some(.Some(.None)), { $0 as B?? })
test(.Some(.None), { $0 as B?? })
test(.None, { $0 as B?? })
// CHECK: .Some(.Some(.Some(.Some(A)))) as B??: .None
// CHECK: .Some(.Some(.Some(.Some(B)))) as B??: .Some(.Some(.Some(B)))
// CHECK: .Some(.Some(.Some(.None))) as B??: .Some(.Some(.None))
// CHECK: .Some(.Some(.None)) as B??: .Some(.None)
// CHECK: .Some(.None) as B??: .None
// CHECK: .None as B??: .None


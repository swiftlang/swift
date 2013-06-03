// RUN: %swift -i %s | FileCheck %s

struct IsSpace : Predicate {
  typealias Arguments = CodePoint
  typealias Result = Bool
  func __call__(c: CodePoint) -> Result {
    return c.isSpace()
  }
}

func testApply() {
  // CHECK: false
  println(apply(IsSpace(), 'x'))

  // CHECK: true
  println(apply(IsSpace(), '\n'))
}
testApply()

func foo() -> Int{
  var aaa = 1 + 2
  aaa = aaa + 3
  if aaa == 3 { aaa = 4 }
  return aaa
}

func foo1() throws {}

enum MyError : Error {
  case E1
  case E2
}

func foo2() throws {
  try foo1()
  try! foo1()
  do {
    try foo1()
  } catch {}
  do {
    try foo1()
  } catch MyError.E1 {}
  do {
    throw MyError.E1
  } catch MyError.E1 {}
  do {
    do {
      throw MyError.E1
    } catch MyError.E1 {}
  } catch {}
}

// RUN: %target-swift-ide-test -range -pos=2:1 -end-pos 4:26 -source-filename %s | %FileCheck %s -check-prefix=CHECK-NO-THROW
// RUN: %target-swift-ide-test -range -pos=16:1 -end-pos 16:13 -source-filename %s | %FileCheck %s -check-prefix=CHECK-THROW
// RUN: %target-swift-ide-test -range -pos=17:1 -end-pos 17:14 -source-filename %s | %FileCheck %s -check-prefix=CHECK-NO-THROW
// RUN: %target-swift-ide-test -range -pos=18:1 -end-pos 20:13 -source-filename %s | %FileCheck %s -check-prefix=CHECK-NO-THROW
// RUN: %target-swift-ide-test -range -pos=21:1 -end-pos 23:24 -source-filename %s | %FileCheck %s -check-prefix=CHECK-THROW
// RUN: %target-swift-ide-test -range -pos=24:1 -end-pos 26:24 -source-filename %s | %FileCheck %s -check-prefix=CHECK-THROW
// RUN: %target-swift-ide-test -range -pos=25:1 -end-pos 25:21 -source-filename %s | %FileCheck %s -check-prefix=CHECK-THROW
// RUN: %target-swift-ide-test -range -pos=28:1 -end-pos 30:26 -source-filename %s | %FileCheck %s -check-prefix=CHECK-THROW
// RUN: %target-swift-ide-test -range -pos=27:1 -end-pos 31:13 -source-filename %s | %FileCheck %s -check-prefix=CHECK-NO-THROW

// CHECK-THROW: <Error>Throwing</Error>
// CHECK-NO-THROW-NOT: <Error>Throwing</Error>

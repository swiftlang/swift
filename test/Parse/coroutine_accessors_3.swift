// RUN: %empty-directory(%t)
// RUN: %target-build-swift %s -o %t/a.out \
// RUN:    -enable-experimental-feature CoroutineAccessors
// RUN: %target-codesign %t/a.out
// RUN: %target-run %t/a.out | %FileCheck %s

public class Klass {
 var _i: Int = 0
}

public struct NonTrivial: CustomStringConvertible {
  public var k: Klass
  public var description: String { k._i.description }
}

protocol P {
 var id: NonTrivial {yielding borrow yielding mutate}
}

public class C: P {
 public init(id: NonTrivial) {
   _id = id
 }
 public var _id: NonTrivial

 public var id: NonTrivial {
  yielding borrow {
   yield _id
  }
  yielding mutate {
   yield &_id
  }
 }
}

func main() {
  let k = Klass()
  let n = NonTrivial(k: k)
  let c = C(id: n)

  print("Test 1: \(c.id)")
  // CHECK: Test 1: 0

  c.id.k._i = 12
  print("Test 2: \(c.id)")
  // CHECK: Test 2: 12

  let p = c as P
  print("Test 3: \(c.id)")
  // CHECK: Test 3: 12

  p.id.k._i = 47
  print("Test 4: \(c.id)")
  // CHECK: Test 4: 47
}

main()

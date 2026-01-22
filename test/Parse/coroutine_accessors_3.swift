// RUN: %empty-directory(%t)
// RUN: %target-build-swift %s -o %t/a.out \
// RUN:    -enable-experimental-feature CoroutineAccessors
// RUN: %target-codesign %t/a.out
// RUN: %target-run %t/a.out | %FileCheck %s

// REQUIRES: swift_feature_CoroutineAccessors
// REQUIRES: executable_test

public class Klass {
  var _i: Int = 0
  init(i: Int) { _i = i }
}

public struct NonTrivial: CustomStringConvertible {
  public var k: Klass
  public var description: String { k._i.description }
  init(i: Int) { k = Klass(i: i) }
}

// Note: `yielding mutate` is explicitly not supported
// in protocols by SE-0474.
// But `set` can be synthesized.
protocol P {
  var id: NonTrivial {yielding borrow set}
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
  let c = C(id: NonTrivial(i: 0))

  print("Test 1: \(c.id)")
  // CHECK: Test 1: 0

  c.id = NonTrivial(i: 12)
  print("Test 2: \(c.id)")
  // CHECK: Test 2: 12

  var p = c as P
  print("Test 3: \(p.id)")
  // CHECK: Test 3: 12

  p.id = NonTrivial(i: 47)
  print("Test 4: \(p.id)")
  // CHECK: Test 4: 47
}

main()

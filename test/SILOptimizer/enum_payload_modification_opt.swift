// RUN: %empty-directory(%t) 
// RUN: %target-build-swift -O -module-name=test %s -o %t/a.out
// RUN: %target-run %t/a.out | %FileCheck %s

// REQUIRES: executable_test,swift_stdlib_no_asserts,optimized_stdlib

final class Storage {
  var v : Int
  init(_ v : Int) {
    self.v = v
  }
}

struct IntBox {
  var s : Storage

  init(_ x : Int) {
    s = Storage(x)
  }

  var value: Int { return s.v }

  mutating func increment(_ delta: Int = 1) {
    if (!isKnownUniquelyReferenced(&s)) {
      // We should never see this message
      print("## copy on write")
      s = Storage(s.v)
    }
    s.v += delta
  }
}

enum E: CustomStringConvertible {
  case value(IntBox)
  case none

  @inline(never)
  mutating func simpleIncrement() {
    switch self {
    case .value(var i):
      i.increment()
      self = .value(i)
    case .none:
      break
    }
  }

  @inline(never)
  mutating func incrementWithControlFlow(_ n: Int, _ c: Bool) {
    switch self {
    case .value(var i):
      i.increment()
      for _ in [0..<n] {
        print("  loop iter")
      }
      if c {
        i.increment(10)
        self = .value(i)
      } else {
        i.increment(20)
        self = .value(i)
      }
    case .none:
      break
    }
  }

  var description: String {
    switch self {
    case .value(let i):
      return i.s.v.description
    case .none:
      return "none"
    }
  }
}

struct ContainingStruct {
  var e: E = .value(IntBox(27))

  @inline(never)
  mutating func doSomething() {
    switch self.e {
    case .value(var i):
      i.increment()
      self.e = .value(i)
    case .none:
      break
    }
  }
}

// CHECK:      simpleIncrement start
print("simpleIncrement start")
var e1 = E.value(IntBox(27))
e1.simpleIncrement()
// CHECK-NEXT: simpleIncrement end: 28
print("simpleIncrement end: \(e1)")


// CHECK-NEXT: incrementWithControlFlow start
print("incrementWithControlFlow start")
var e2 = E.value(IntBox(27))
// CHECK-NEXT:   loop iter
e2.incrementWithControlFlow(1, true)
// CHECK-NEXT: incrementWithControlFlow end: 38
print("incrementWithControlFlow end: \(e2)")

// CHECK-NEXT: ContainingStruct start
print("ContainingStruct start")
var s = ContainingStruct()
s.doSomething()
// CHECK-NEXT: ContainingStruct end: 28
print("ContainingStruct end: \(s.e)")


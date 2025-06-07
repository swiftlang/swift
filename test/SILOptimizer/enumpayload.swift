// RUN: %empty-directory(%t) 
// RUN: %target-build-swift -O -module-name=test %s -o %t/a.out
// RUN: %target-codesign %t/a.out
// RUN: %target-run %t/a.out | %FileCheck %s

// REQUIRES: executable_test,optimized_stdlib

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

  mutating func increment(_ delta: Int) {
    if (!isKnownUniquelyReferenced(&s)) {
      // We should never see this message
      print("## copy on write")
      s = Storage(s.v)
    }
    s.v += delta
  }
}

enum E {
  case value(IntBox)
  case none

  @inline(never)
  mutating func simpleIncrement() {
    switch self {
    case .value(var i):
      i.increment(1)
      self = .value(i)
    case .none:
      break
    }
  }
}

func test_enum() {
  // CHECK:      test_enum() simpleIncrement start
  print(#function, "simpleIncrement start")
  var e1 = E.value(IntBox(27))
  e1.simpleIncrement()
  // CHECK-NEXT: test_enum() simpleIncrement end: 28
  switch e1 {
  case .value(let i):
    print(#function, "simpleIncrement end:", i.s.v.description)
  case .none:
    print("none")
  }
}

test_enum()

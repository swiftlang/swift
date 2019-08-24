// RUN: %target-run-simple-swift | %FileCheck %s
// REQUIRES: executable_test

class Interval {
  var lo, hi : Int

  init(_ lo:Int, _ hi:Int) {
    self.lo = lo
    self.hi = hi
  }

  func show() {
    print("[\(lo), \(hi)]")
  }
  
  class func like(_ lo: Int, _ hi: Int) -> Interval {
    return Interval(lo, hi)
  }
}

class OpenInterval : Interval {
  override init(_ lo:Int, _ hi:Int) {
    super.init(lo, hi)
  }

  override func show() {
    print("(\(lo), \(hi))")
  }

  override class func like(_ lo:Int, _ hi:Int) -> Interval {
    return OpenInterval(lo, hi)
  }
}

func +(a: Interval, b: Interval) -> Interval {
  return Interval(a.lo + b.lo, a.hi + b.hi)
}

func -(a: Interval, b: Interval) -> Interval {
  return Interval(a.lo - b.hi, a.hi - b.lo)
}

prefix func -(a: Interval) -> Interval {
  return type(of: a).like(-a.hi, -a.lo)
}

// CHECK: [-2, -1]
(-Interval(1,2)).show()
// CHECK: [4, 6]
(Interval(1,2) + Interval(3,4)).show()
// CHECK: [1, 3]
(Interval(3,4) - Interval(1,2)).show()
// CHECK: (-1, 1)
(OpenInterval(-1,1)).show()
// CHECK: (-3, 2)
(-OpenInterval(-2,3)).show()

// CHECK: false
print(Interval(1,2) is OpenInterval)
// CHECK: true
var i12 : Interval = OpenInterval(1,2)
print(i12 is OpenInterval)

class RDar16563763_A {}
class RDar16563763_B : RDar16563763_A {}
print("self is Type = \(RDar16563763_A.self is RDar16563763_B.Type)")
// CHECK: self is Type = false

//
// rdar://problem/19321484
//

class Base {
  func makePossibleString() -> String? {
    return "Base"
  }
}

/* inherits from Base with method that returns a String instead of an Optional
 * String */
class CompilerCrasher : Base {
  override func makePossibleString() -> String {
    return "CompilerCrasher"
  }
}

class SonOfCompilerCrasher: CompilerCrasher {}

class ReturnOfCompilerCrasher: CompilerCrasher {
  override func makePossibleString() -> String {
    return "ReturnOfCompilerCrasher"
  }
}

func testCrash(_ c: Base) -> String? {
  let s = c.makePossibleString()
  return s
}

public func driver() {
  var c = CompilerCrasher()
  var d = SonOfCompilerCrasher()
  var e = ReturnOfCompilerCrasher()
  var r = testCrash(c)
  print(r)
  r = testCrash(d)
  print(r)
  r = testCrash(e)
  print(r)
}
driver()
// CHECK: Optional("CompilerCrasher")
// CHECK-NEXT: Optional("CompilerCrasher")
// CHECK-NEXT: Optional("ReturnOfCompilerCrasher")

struct Account {
  var owner: String
}

class Bank {
  func transferMoney(_ from: Account?, to: Account!) -> Account! {
    return nil
  }

  func deposit(_ to: Account) -> Account? {
    return nil
  }
}

class DodgyBank : Bank {
  // Parameters: swap ? and !
  // Result: less optional
  override func transferMoney(_ from: Account!, to: Account?) -> Account {
    if let fromAccount = from {
      return fromAccount
    } else {
      return Account(owner: "Bank fees")
    }
  }

  // Parameter: more optional
  // Result: swap ? and !
  override func deposit(_ to: Account?) -> Account! {
    if let toAccount = to {
      if (toAccount.owner == "Cyberdyne Systems") {
        return nil
      }
    }
    return to
  }
}

// CHECK: Account(owner: "A")
// CHECK: Account(owner: "Bank fees")
// CHECK: nil
// CHECK: Optional(main.Account(owner: "A"))

let b = DodgyBank()

#if false
// FIXME: rdar://problem/21435542
print(b.transferMoney(Account(owner: "A"), to: Account(owner: "B")))
print(b.transferMoney(nil, to: nil))
print(b.deposit(Account(owner: "Cyberdyne Systems")))
print(b.deposit(Account(owner: "A")))
print(b.deposit(nil))
#endif

print((b as Bank).transferMoney(Account(owner: "A"), to: Account(owner: "B")))
print((b as Bank).transferMoney(nil, to: nil))
print((b as Bank).deposit(Account(owner: "Cyberdyne Systems")))
print((b as Bank).deposit(Account(owner: "A")))

// rdar://25412647

private class Parent<T> {
  required init() {}

  func doSomething() {
    overriddenMethod()
  }

  func overriddenMethod() {
    fatalError("You should override this method in child class")
  }
}

private class Child: Parent<String> {
  override func overriddenMethod() {
    print("Heaven!")
  }
}

Child().doSomething() // CHECK: Heaven!

// rdar://23376955

protocol Makeable {
  init()
  func doSomething()
}

extension Parent : Makeable {}

func makeOne<T : Makeable>(_: T.Type) -> T {
  return T()
}

makeOne(Child.self).doSomething() // CHECK: Heaven!

// https://bugs.swift.org/browse/SR-3840

class BaseProperty {
  var value: Int {
    get { fatalError() }
    set { fatalError() }
  }

  func increment() -> Self {
    value += 1
    return self
  }
}

class DerivedProperty : BaseProperty {
  override var value: Int {
    get { return _value }
    set { _value = newValue }
  }

  var _value: Int = 0
}

// CHECK: 1
print(DerivedProperty().increment().value)

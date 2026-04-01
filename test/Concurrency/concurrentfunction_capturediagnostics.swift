// RUN: %target-swift-frontend -target %target-swift-5.1-abi-triple -enable-experimental-flow-sensitive-concurrent-captures -verify -emit-sil %s -o - >/dev/null
// RUN: %target-swift-frontend -target %target-swift-5.1-abi-triple -enable-experimental-flow-sensitive-concurrent-captures -verify -emit-sil %s -o - >/dev/null -strict-concurrency=targeted
// RUN: %target-swift-frontend -target %target-swift-5.1-abi-triple -enable-experimental-flow-sensitive-concurrent-captures -verify -emit-sil %s -o - >/dev/null -strict-concurrency=complete

// REQUIRES: concurrency

func f(_: @escaping @Sendable () -> Void) { }
open class F {
  func useConcurrent(_: @escaping @Sendable () -> Void) { }
}

extension Int {
  mutating func addOne() {
    self += 1
  }
}

func inoutUserInt(_ t: inout Int) {}

func testCaseTrivialValue() {
  var i = 17
  f {
    print(i + 17)
    print(i + 18)
    print(i + 19)
  }

  i = 20
  i += 21

  // We only emit a warning here since we use the last write.
  //
  // TODO: Should we emit for all writes?
  i.addOne() // expected-warning {{'i' mutated after capture by sendable closure}}
             // expected-note @-14 {{variable defined here}}
             // expected-note @-14 {{variable captured by sendable closure}}
             // expected-note @-14 {{capturing use}}
             // expected-note @-14 {{capturing use}}
             // expected-note @-14 {{capturing use}}
}

func testCaseTrivialValue2() {
  var i2 = 17
  f {
    print(i2 + 17)
    print(i2 + 18)
    print(i2 + 19)
  }

  i2 = 20
  i2 += 21 // expected-warning {{'i2' mutated after capture by sendable closure}}
             // expected-note @-9 {{variable defined here}}
             // expected-note @-9 {{variable captured by sendable closure}}
             // expected-note @-9 {{capturing use}}
             // expected-note @-9 {{capturing use}}
             // expected-note @-9 {{capturing use}}
}

func testCaseTrivialValue3() {
  var i3 = 17
  f {
    print(i3 + 17)
    print(i3 + 18)
    print(i3 + 19)
  }

  i3 = 20 // expected-warning {{'i3' mutated after capture by sendable closure}}
          // expected-note @-8 {{variable defined here}}
          // expected-note @-8 {{variable captured by sendable closure}}
          // expected-note @-8 {{capturing use}}
          // expected-note @-8 {{capturing use}}
          // expected-note @-8 {{capturing use}}
}

func testCaseTrivialValue4() {
  var i4 = 17
  f {
    print(i4 + 17)
    print(i4 + 18)
    print(i4 + 19)
  }

  inoutUserInt(&i4) // expected-warning {{'i4' mutated after capture by sendable closure}}
                    // expected-note @-8 {{variable defined here}}
                    // expected-note @-8 {{variable captured by sendable closure}}
                    // expected-note @-8 {{capturing use}}
                    // expected-note @-8 {{capturing use}}
                    // expected-note @-8 {{capturing use}}
}

class Klass: UnsafeSendable { // expected-warning{{'UnsafeSendable' is deprecated: Use @unchecked Sendable instead}}{{documentation-file=deprecated-declaration}}
  var next: Klass? = nil
}
func inoutUserKlass(_ k: inout Klass) {}
func inoutUserOptKlass(_ k: inout Klass?) {}

func testCaseClass() {
  var i = Klass()
  f {
    print(i)
  }

  i = Klass() // expected-warning {{'i' mutated after capture by sendable closure}}
              // expected-note @-6 {{variable defined here}}
              // expected-note @-6 {{variable captured by sendable closure}}
              // expected-note @-6 {{capturing use}}
}

func testCaseClassInout() {
  var i2 = Klass()
  f {
    print(i2)
  }
  inoutUserKlass(&i2) // expected-warning {{'i2' mutated after capture by sendable closure}}
                      // expected-note @-5 {{variable defined here}}
                      // expected-note @-5 {{variable captured by sendable closure}}
                      // expected-note @-5 {{capturing use}}
}

func testCaseClassInoutField() {
  var i2 = Klass()
  i2 = Klass()
  f {
    print(i2)
  }
  // Since we are passing in .next inout and we are using a class this isn't a
  // violation.
  inoutUserOptKlass(&i2.next)
}

////////////////////////////
// Non Trivial Value Type //
////////////////////////////

struct NonTrivialValueType: Sendable {
  var i: Int
  var k: Klass? = nil

  init(_ inputI: Int, _ inputKlass: Klass) {
    self.i = inputI
    self.k = inputKlass
  }
}

func testCaseNonTrivialValue() {
  var i = NonTrivialValueType(17, Klass())
  f {
    print(i.i + 17)
    print(i.i + 18)
    print(i.i + 19)
  }

  i.i = 20
  i.i += 21

  // We only emit a warning here since we use the last write.
  //
  // TODO: Should we emit for all writes?
  i.i.addOne() // expected-warning {{'i' mutated after capture by sendable closure}}
               // expected-note @-14 {{variable defined here}}
               // expected-note @-14 {{variable captured by sendable closure}}
               // expected-note @-14 {{capturing use}}
               // expected-note @-14 {{capturing use}}
               // expected-note @-14 {{capturing use}}
}

func testCaseNonTrivialValueInout() {
  var i = NonTrivialValueType(17, Klass())
  f {
    print(i.i + 17)
    print(i.k ?? "none")
  }

  // We only emit a warning here since we use the last write.
  inoutUserOptKlass(&i.k) // expected-warning {{'i' mutated after capture by sendable closure}}
                          // expected-note @-8 {{variable defined here}}
                          // expected-note @-8 {{variable captured by sendable closure}}
                          // expected-note @-8 {{capturing use}}
                          // expected-note @-8 {{capturing use}}
}

protocol MyProt {
  var i: Int { get set }
  var k: Klass? { get set }
}

func testCaseAddressOnlyAllocBoxToStackable<T : MyProt & Sendable>(i : T) {
  var i2 = i
  f {
    print(i2.i + 17)
    print(i2.k ?? "none")
  }

  // TODO: Make sure we emit these once we support address only types!
  inoutUserOptKlass(&i2.k) // xpected-warning {{'i2' mutated after capture by sendable closure}}
                           // xpected-note @-8 {{variable defined here}}
                           // xpected-note @-8 {{variable captured by sendable closure}}
                           // xpected-note @-8 {{capturing use}}
                           // xpected-note @-8 {{capturing use}}
}

// Alloc box to stack can't handle this test case, so show off a bit and make
// sure we can emit a great diagnostic here!
func testCaseAddressOnlyNoAllocBoxToStackable<T : MyProt & Sendable>(i : T) {
  let f2 = F()
  var i2 = i
  f2.useConcurrent {
    print(i2.i + 17)
    print(i2.k ?? "none")
  }

  // TODO: Make sure we emit these once we support address only types!
  inoutUserOptKlass(&i2.k) // xpected-warning {{'i2' mutated after capture by sendable closure}}
                           // xpected-note @-8 {{variable defined here}}
                           // xpected-note @-8 {{variable captured by sendable closure}}
                           // xpected-note @-8 {{capturing use}}
                           // xpected-note @-8 {{capturing use}}
}


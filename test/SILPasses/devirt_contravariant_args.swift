// RUN: %target-swift-frontend -O -primary-file %s -emit-sil -sil-inline-threshold 1000 -sil-verify-all | FileCheck %s

// Make sure that we can dig all the way through the class hierarchy and
// protocol conformances.

// CHECK-LABEL: sil hidden @_TF25devirt_contravariant_args6driverFT_T_ : $@convention(thin) () -> () {
// CHECK: function_ref unknownC2
// CHECK: function_ref unknownC1
// CHECK: function_ref unknownC0
// CHECK: return
// CHECK-NEXT: }

@asmname("unknownC0")
func unknownC0(c : C0) -> ()
@asmname("unknownC1")
func unknownC1(c : C1) -> ()
@asmname("unknownC2")
func unknownC2(c : C2) -> ()

protocol P {}

class C0 : P {}
class C1 : C0 {}
class C2 : C1 {}

class B<T> {
  func performSomething(p : P) {
    doSomething(p as! C2)
  }

  func doSomething(c : C2) {
    unknownC2(c)
  }

  // See comment in protocol P
  //class func doSomethingMeta() {
  //  unknown1b()
  //}
}

class B2<T> : B<T> {
  override func performSomething(p : P) {
    doSomething(p as! C1)
  }

  // When we have covariance in protocols, change this to B2.
  // We do not specialize typealias correctly now.
  //typealias X = B
  override func doSomething(c : C1) {
    unknownC1(c)
  }

  // See comment in protocol P
  //override class func doSomethingMeta() {
  //  unknown2b()
  //}
}

class B3<T> : B2<T> {
  override func performSomething(p : P) {
    doSomething(p as! C0)
  }

  override func doSomething(c : C0) {
    unknownC0(c)
  }
}



func doSomething<T : P>(b : B<T>, _ t : T) {
  b.performSomething(t)
}

func driver() -> () {
  var b = B<C2>()
  var b2 = B2<C1>()
  var b3 = B3<C0>()

  var c0 = C0()
  var c1 = C1()
  var c2 = C2()

  doSomething(b, c2)
  doSomething(b2, c1)
  doSomething(b3, c0)
}

driver()

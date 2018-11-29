// RUN: %target-swift-frontend -O -Xllvm -sil-inline-generics=false -Xllvm -sil-disable-pass=ObjectOutliner %s -emit-sil -sil-verify-all | %FileCheck %s

// Make sure that we completely inline/devirtualize/substitute all the way down
// to unknown1.

// CHECK-LABEL: sil @main
// CHECK: bb0({{.*}}):
// CHECK: function_ref @unknown1
// CHECK: apply
// CHECK: apply
// CHECK: return

struct Int32 {}

@_silgen_name("unknown1")
func unknown1() -> ()

protocol P {
  func doSomething(_ x : Int32)
}

struct X {}

class B<T> : P {
  func doSomething(_ x : Int32) {
     unknown1()
   }
 }

func doSomething(_ p : P, _ x : Int32) {
  p.doSomething(x)
}
func doSomething2<T : P>(_ t : T, _ x : Int32) {
  t.doSomething(x)
}

func driver() {
  var b2 = B<X>()
  var x = Int32()
  doSomething(b2, x)
  doSomething2(b2, x)
}

driver()

// <rdar://problem/46322928> Failure to devirtualize a protocol method
// applied to an opened existential blocks implemention of
// DataProtocol.
public protocol ContiguousBytes {
    func withUnsafeBytes<R>(_ body: (UnsafeRawBufferPointer) throws -> R) rethrows -> R
}

extension Array : ContiguousBytes {}
extension ContiguousArray : ContiguousBytes {}

@inline(never)
func takesPointer(_ p: UnsafeRawBufferPointer) {}

// In specialized testWithUnsafeBytes<A>(_:), the conditional case and call to withUnsafeBytes must be eliminated.
// CHECK-LABEL: sil shared [noinline] @$s30devirt_specialized_conformance19testWithUnsafeBytesyyxlFSayypG_Tg5 : $@convention(thin) (@guaranteed Array<Any>) -> () {
// CHECK: bb0
// CHECK-NOT: witness_method
// CHECK: [[TAKES_PTR:%.*]] = function_ref @$s30devirt_specialized_conformance12takesPointeryySWF : $@convention(thin) (UnsafeRawBufferPointer) -> ()
// CHECK: apply [[TAKES_PTR]](%{{.*}}) : $@convention(thin) (UnsafeRawBufferPointer) -> ()
// CHECK-LABEL: } // end sil function '$s30devirt_specialized_conformance19testWithUnsafeBytesyyxlFSayypG_Tg5'
@inline(never)
func testWithUnsafeBytes<T>(_ t: T) {
  if let cb = t as? ContiguousBytes {
    cb.withUnsafeBytes { takesPointer($0) }
  }
}

testWithUnsafeBytes([])

// A statically-emitted key path must use the same component *identity* the
// runtime instantiator would compute, not the key path accessor thunk. The two
// are different symbols, and `AnyKeyPath.==` / `hash(into:)` compare the id, so
// getting it wrong makes a directly-written key path compare unequal to the
// same path assembled by `appending(path:)`.
//
// This is only observable when one side is statically emitted and the other is
// built at runtime, which is what `appending` does.

// RUN: %target-run-simple-swift | %FileCheck %s
// RUN: %target-run-simple-swift(-enable-experimental-feature StaticKeyPaths) | %FileCheck %s

// REQUIRES: executable_test
// REQUIRES: swift_feature_StaticKeyPaths

final class RefA {
  var readOnly: ValB { ValB() }
  var settable: ValB {
    get { ValB() }
    set { }
  }
}

struct ValB {
  var readOnly: RefA { RefA() }
  var mutating: RefA {
    get { RefA() }
    set { }
  }
  var nonmutating: RefA {
    get { RefA() }
    nonmutating set { }
  }
}

struct Plain {
  var stored: Int32 = 0
  var computed: Int32 {
    get { stored }
    set { stored = newValue }
  }
}

func check(_ name: String, _ direct: AnyKeyPath, _ appended: AnyKeyPath) {
  let ok = direct == appended && direct.hashValue == appended.hashValue
  print("\(ok ? "OK!" : "FAIL") \(name)")
}

// A computed component reached through another computed component: the first
// component's id is what differs if the thunk is used by mistake.
let a_readOnly = (\RefA.readOnly as AnyKeyPath) as! KeyPath<RefA, ValB>
let b_mutating = (\ValB.mutating as AnyKeyPath) as! WritableKeyPath<ValB, RefA>
let b_nonmutating =
  (\ValB.nonmutating as AnyKeyPath) as! ReferenceWritableKeyPath<ValB, RefA>
let b_readOnly = (\ValB.readOnly as AnyKeyPath) as! KeyPath<ValB, RefA>
let a_settable = (\RefA.settable as AnyKeyPath) as! WritableKeyPath<RefA, ValB>

// CHECK: OK! class computed -> struct mutating
check("class computed -> struct mutating",
      \RefA.readOnly.mutating, a_readOnly.appending(path: b_mutating))
// CHECK-NEXT: OK! class computed -> struct nonmutating
check("class computed -> struct nonmutating",
      \RefA.readOnly.nonmutating, a_readOnly.appending(path: b_nonmutating))
// CHECK-NEXT: OK! class settable -> struct readOnly
check("class settable -> struct readOnly",
      \RefA.settable.readOnly, a_settable.appending(path: b_readOnly))

// A struct computed property, whose id is the getter rather than a method
// descriptor.
let p_computed = (\Plain.computed as AnyKeyPath) as! WritableKeyPath<Plain, Int32>
let p_stored = (\Plain.stored as AnyKeyPath) as! WritableKeyPath<Plain, Int32>
// CHECK-NEXT: OK! struct computed identity
check("struct computed identity", \Plain.computed, p_computed)
// CHECK-NEXT: OK! struct stored identity
check("struct stored identity", \Plain.stored, p_stored)

// Distinct properties must still compare unequal.
print(p_computed != p_stored ? "OK! distinct" : "FAIL distinct")
// CHECK-NEXT: OK! distinct

// Reading and writing through the appended and direct forms agree.
var plain = Plain(stored: 3)
plain[keyPath: p_computed] = 8
print(plain.stored == 8 ? "OK! write" : "FAIL write")
// CHECK-NEXT: OK! write

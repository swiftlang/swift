// RUN: %target-run-simple-swift(-I %swift_src_root/lib/ClangImporter/SwiftBridging -I %S/Inputs -cxx-interoperability-mode=default -Xfrontend -disable-availability-checking -Onone) | %FileCheck %s

// REQUIRES: executable_test

import RefCountedSmartPtrs

func readPtrField() {
  print("readPtrField")
  let pair = makePairWithPtr()
  if let frt = pair.ptr.asReference {
    print(frt.method())
  }
  let nullPair = makePairWithNullPtr()
  if nullPair.ptr.asReference == nil {
    print("nil")
  }
}

func readConstRefField() {
  print("readConstRefField")
  let pair = makePairWithConstRef()
  let frt = pair.ref.asReference
  print(frt.method())
  print(pair.loadRef().method())
}

func passFieldImplicitlyBridged() {
  print("passFieldImplicitlyBridged")
  let pair = makePairWithConstRef()
  bridgedFunction(pair.ref.asReference)
}

func writePtrField(_ r: RefCountedBase) {
  print("writePtrField")
  var pair = makePairWithNullPtr()
  pair.ptr = PtrOfBase(r)
  print(pair.ptr.asReference!.method())
}

func writePtrViaSetter(_ r: RefCountedBase) {
  print("writePtrViaSetter")
  var pair = makePairWithNullPtr()
  pair.storePtr(r)
  print(pair.loadPtr()!.method())
}

func passStructWithSmartPtrField() {
  print("passStructWithSmartPtrField")
  let pair = makePairWithPtr()
  takesPairWithPtr(pair)
}

readPtrField()
// CHECK: readPtrField
// CHECK: 42
// CHECK: nil

readConstRefField()
// CHECK: readConstRefField
// CHECK: 42
// CHECK: 42

passFieldImplicitlyBridged()
// CHECK: passFieldImplicitlyBridged

writePtrField(RefCountedBase())
// CHECK: writePtrField
// CHECK: 42

writePtrViaSetter(RefCountedBase())
// CHECK: writePtrViaSetter
// CHECK: 42

passStructWithSmartPtrField()
// CHECK: passStructWithSmartPtrField

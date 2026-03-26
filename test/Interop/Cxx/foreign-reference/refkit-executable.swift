// RUN: %target-run-simple-swift(-I %S/Inputs -cxx-interoperability-mode=default -Xfrontend -disable-availability-checking -Xcc -DEXECUTABLE)
// RUN: %target-run-simple-swift(-I %S/Inputs -cxx-interoperability-mode=default -Xfrontend -disable-availability-checking -Xcc -DEXECUTABLE -DCHECK_REFCOUNTS)
// REQUIRES: executable_test

import RefKit
import StdlibUnittest

var RefKitTests = TestSuite("RefKit regression tests")

// Simulate some copy operations that cause retain/releases
@inline(never)
func clonePair<T: Copyable>(_ t: T) -> (T, T) { return (t, t) }

RefKitTests.test("Create a null ref without crashing") {
  defer { expectEqual(globalCount, 0, "There should be no shared objects") }

  let ro = RefObject.createNull()
  expectEqual(globalCount, 0, "There should be no shared objects")
  expectTrue(ro.isNull())
  expectNil(ro.getPtrUnretained(), "optional nullptr should be nil")
}

RefKitTests.test("Create reference-counted number object") {
  defer { expectEqual(globalCount, 0, "All objects should be deallocated") }

#if CHECK_REFCOUNTS
  var n = RefNumberObj.create(42.0)
  var nn = n
  // mutations require n and nn to be distinct, to ensure copy ctor is called
  nn.fakeMutation()
  n.fakeMutation()
  nn.fakeMutation()
#else
  let n = RefNumberObj.create(42.0)
  let nn = n
#endif

  let n1 = n.getPtrUnretained()!
  let n2 = n.getRefUnretained()
  let (n3, n4) = clonePair(n1)
  let (n5, n6) = clonePair(n2)

  expectEqual(n3.get(), 42.0)
  expectEqual(globalCount, 1, "There should only be one shared object")
#if CHECK_REFCOUNTS
  expectTrue(n1.refCount() >= 8)
#endif

  n5.set(-36.5)

  expectEqual(n6.get(), -36.5)
  expectEqual(n5.get(), -36.5)
  expectEqual(n4.get(), -36.5)
  expectEqual(n3.get(), -36.5)
  expectEqual(n2.get(), -36.5)
  expectEqual(n1.get(), -36.5)

  expectEqual(nn.getRefUnretained().get(), -36.5)
  expectEqual(n.getRefUnretained().get(), -36.5)
}

RefKitTests.test("Create reference-counted boxed float") {
  defer { expectEqual(globalCount, 0, "All objects should be deallocated") }

#if CHECK_REFCOUNTS
  var f = RefBoxedFloat.create(42.0)
  var ff = f
  // mutations require f and ff to be distinct, to ensure copy ctor is called
  ff.fakeMutation()
  f.fakeMutation()
  ff.fakeMutation()
#else
  let f = RefBoxedFloat.create(42.0)
  let ff = f
#endif

  let f1 = f.getPtrUnretained()!
  let f2 = f.getRefUnretained()
  let (f3, f4) = clonePair(f1)
  let (f5, f6) = clonePair(f2)

  expectEqual(f5.pointee, 42.0)
  expectEqual(globalCount, 1, "There should only be one shared object")
#if CHECK_REFCOUNTS
  expectTrue(f4.refCount() >= 8)
#endif

  f2.pointee = -36.5

  expectEqual(f6.pointee, -36.5)
  expectEqual(f5.pointee, -36.5)
  expectEqual(f4.pointee, -36.5)
  expectEqual(f3.pointee, -36.5)
  expectEqual(f2.pointee, -36.5)
  expectEqual(f1.pointee, -36.5)

  expectEqual(ff.getRefUnretained().pointee, -36.5)
  expectEqual(f.getRefUnretained().pointee, -36.5)
}

runAllTests()

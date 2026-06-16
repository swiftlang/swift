// RUN: %target-run-simple-swift(-I %S/Inputs/ -cxx-interoperability-mode=default)
// RUN: %target-run-simple-swift(-I %S/Inputs/ -cxx-interoperability-mode=default -Xcc -std=c++23 -D CXX23)
//
// REQUIRES: executable_test

import StdlibUnittest
import SubscriptOverloads

var Suite = TestSuite("Types with heavily overloaded subscripts")

Suite.test("struct with overloaded subscript") {
  var v = Overloaded()

  // Check initial values
  expectEqual(v[0 as CInt].val, Num.DEFAULT())
  expectEqual(v[1 as CInt].val, Num.DEFAULT())
  expectEqual(v[2 as CInt].val, Num.DEFAULT())
  expectEqual(v[3 as CInt].val, Num.DEFAULT())

  // Perform some writes using the only settable subscript overloads
  v[1 as CUnsignedInt] = Num(111)
  v[Num(2)].val = 222

  // Read values with the various gettable subscripts
  expectEqual(v[0 as CInt].val, Num.DEFAULT())
  expectEqual(v[1 as CInt].val, 111)
  expectEqual(v[2 as CInt].val, 222)
  expectEqual(v[3 as CInt].val, Num.DEFAULT())

  expectEqual(v[0 as CUnsignedInt].val, Num.DEFAULT())
  expectEqual(v[1 as CUnsignedInt].val, 111)
  expectEqual(v[2 as CUnsignedInt].val, 222)
  expectEqual(v[3 as CUnsignedInt].val, Num.DEFAULT())

  expectEqual(v[Num(0)].val, Num.DEFAULT())
  expectEqual(v[Num(1)].val, 111)
  expectEqual(v[Num(2)].val, 222)
  expectEqual(v[Num(3)].val, Num.DEFAULT())

  // Test out the basic custom index types
  typealias Val = Overloaded.GetVal
  typealias Ref = Overloaded.GetRef
  typealias Ptr = Overloaded.GetPtr

  v[Ref(index: 1)].pointee = 1111
  v[Ptr(index: 2)] = 2222

  expectEqual(v[Ref(index: 0)].pointee, Num.DEFAULT())
  expectEqual(v[Ptr(index: 1)], 1111)
  expectEqual(v[2 as CInt].val, 2222)
  expectEqual(v[Val(index: 3)], Num.DEFAULT())

  // Test out the funkier custom index types
  typealias PtrRef = Overloaded.GetPtrRef
  typealias RefVal = Overloaded.GetRefVal
  typealias ValPtr = Overloaded.GetValPtr

  v[PtrRef(index: 1)] = 101
  v[RefVal(index: 2)] = 202

  expectEqual(v[RefVal(index: 0)], Num.DEFAULT())
  expectEqual(v[1 as CInt].val, 101)
  expectEqual(v[PtrRef(index: 2)], 202)
  expectEqual(v[ValPtr(index: 3)]!.pointee, Num.DEFAULT())

  // The operator[](float) overload is just the identity function.
  expectEqual(v[4.2], 4.2)

  // The operator[](Unit) overload is a bit silly. It returns a reference to
  // a static singleton instance of Unit. Play around with this a bit and make
  // sure things don't break, even if the API is wack.
  var unit = Overloaded.Unit()
  expectEqual(v[unit].u, unit.u)

  var unit2 = Overloaded.Unit() // what we we will overwrite with
  unit2.u = 42

  v[unit] = unit2 // overwrite the static singleton by copy assignment
  unit.u = 22     // N.B. this has no effect on this test
  unit2.u = 33    // N.B. this has no effect on this test

  expectEqual(v[unit].u, 42) // overwrite persists across subscript calls

  var u = Overloaded()
  expectEqual(u[unit].u, 42) // overwrite persists across subscript calls to
                             // different instances of Overloaded()
                             // (it is a static instance that is overwritten)

  expectTrue(v[Overloaded.Bogus()])

  let CharPtr = v[Overloaded.GetCharPtr()]
  let ConstCharPtr = v[Overloaded.GetConstCharPtr()]
  expectEqual(v[CharPtr], CharPtr)
  expectEqual(v[ConstCharPtr], ConstCharPtr)

#if CXX23
  expectTrue(v[])

  expectEqual(v[1.0, 2.0].val, Num.DEFAULT())
  expectEqual(v[11, 22], 33)
#endif
}

runAllTests()

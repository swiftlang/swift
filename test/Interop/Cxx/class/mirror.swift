// RUN: %target-run-simple-swift(-cxx-interoperability-mode=default -I %S/Inputs)

// REQUIRES: executable_test
// Metadata for foreign reference types is not supported on Windows.
// UNSUPPORTED: OS=windows-msvc

import StdlibUnittest
import Mirror

var MirrorTestSuite = TestSuite("Mirrors")

MirrorTestSuite.test("EmptyCxxStruct") {
  let s = EmptyStruct()
  let m = Mirror(reflecting: s)
  expectEqual(.`struct`, m.displayStyle)
  expectTrue(m.subjectType == EmptyStruct.self)
  expectEqual(0, m.children.count)

  var output = ""
  dump(s, to: &output)
  expectEqual("- __C.EmptyStruct\n", output)
}

MirrorTestSuite.test("EmptyCxxClass") {
  let s = EmptyClass()
  let m = Mirror(reflecting: s)
  expectEqual(.`struct`, m.displayStyle)
  expectTrue(m.subjectType == EmptyClass.self)
  expectEqual(0, m.children.count)

  var output = ""
  dump(s, to: &output)
  expectEqual("- __C.EmptyClass\n", output)
}

MirrorTestSuite.test("CxxStructWithFields") {
  let s = BaseStruct(1, 2, 3)
  let m = Mirror(reflecting: s)
  expectEqual(.`struct`, m.displayStyle)
  expectTrue(m.subjectType == BaseStruct.self)
  expectEqual(1, m.children.count)

  expectEqual("publ", m.children.first!.label)
  expectEqual(2, m.children.first!.value as? Int32)
  
  var output = ""
  dump(s, to: &output)
  let expected = 
    "▿ __C.BaseStruct\n" +
    "  - publ: 2\n"
  expectEqual(expected, output)
}

MirrorTestSuite.test("CxxStructWithStructsAsFields") {
  let s = OuterStruct()
  let m = Mirror(reflecting: s)
  expectEqual(.`struct`, m.displayStyle)
  expectTrue(m.subjectType == OuterStruct.self)
  expectEqual(1, m.children.count)
  expectEqual("publStruct", m.children.first!.label)
  
  var output = ""
  dump(s, to: &output)
  let expected = 
    "▿ __C.OuterStruct\n" +
    "  ▿ publStruct: __C.BaseStruct\n" +
    "    - publ: 5\n"    
  expectEqual(expected, output)
}

if #available(SwiftStdlib 6.2, *) {
  MirrorTestSuite.test("CxxFRTStruct") {
    let s = FRTStruct()
    let m = Mirror(reflecting: s)
    expectEqual(.foreignReference, m.displayStyle)
    expectTrue(m.subjectType == FRTStruct.self)
    expectEqual(0, m.children.count)

    var output = ""
    dump(s, to: &output)
    expectEqual("- __C.FRTStruct\n", output)
  }

  MirrorTestSuite.test("CxxFRTImmortalClass") {
    let s = FRTImmortalClass()
    let m = Mirror(reflecting: s)
    expectEqual(.foreignReference, m.displayStyle)
    expectTrue(m.subjectType == FRTImmortalClass.self)
    expectEqual(0, m.children.count)

    var output = ""
    dump(s, to: &output)
    expectEqual("- __C.FRTImmortalClass\n", output)
  }
}

runAllTests()

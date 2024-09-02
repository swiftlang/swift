// RUN: %target-run-simple-swift(-I %S/Inputs -Xfrontend -enable-experimental-cxx-interop -Xcc -std=c++20)
// RUN: %target-run-simple-swift(-I %S/Inputs -Xfrontend -enable-experimental-cxx-interop -Xcc -std=c++20 -Xcc -D_LIBCPP_HARDENING_MODE=_LIBCPP_HARDENING_MODE_DEBUG)

// FIXME swift-ci linux tests do not support std::span
// UNSUPPORTED: OS=linux-gnu

// REQUIRES: executable_test

import StdlibUnittest
#if !BRIDGING_HEADER
import StdSpan
#endif
import CxxStdlib

var StdSpanTestSuite = TestSuite("StdSpan")

func checkSpan<T: RandomAccessCollection, E: Equatable>(_ s : T, _ arr: [E]) 
                    where T.Index == Int, T.Element == E {
  expectFalse(s.isEmpty)
  expectEqual(s.count, arr.count)

  for i in 0..<arr.count {
    expectEqual(s[i], arr[i])
  }
}

func takesConstSpanOfInt(_ s: ConstSpan) {
  expectEqual(s.size(), 3)
  expectFalse(s.empty())

  expectEqual(s[0], 1)
  expectEqual(s[1], 2)
  expectEqual(s[2], 3)
}

func takesSpanOfInt(_ s: inout Span) {
  expectEqual(s.size(), 3)
  expectFalse(s.empty())

  expectEqual(s[0], 1)
  expectEqual(s[1], 2)
  s[2] = 4
  expectEqual(s[2], 4)

  s[2] = 3
  expectEqual(s[2], 3)
}

func takesConstSpanOfString(_ s: ConstSpanOfString) {
  expectEqual(s.size(), 3)
  expectFalse(s.empty())

  expectEqual(s[0], "")
  expectEqual(s[1], "ab")
  expectEqual(s[2], "abc")
}


func takesSpanOfString(_ s: inout SpanOfString) {
  expectEqual(s.size(), 3)
  expectFalse(s.empty())

  expectEqual(s[0], "")
  expectEqual(s[1], "ab")
  s[2] = "abcd"
  expectEqual(s[2], "abcd")

  s[2] = "abc"
  expectEqual(s[2], "abc")
}

func returnsConstSpanOfInt() -> ConstSpan {
  let arr: [Int32] = [1, 2, 3]
  return arr.withUnsafeBufferPointer { ubpointer in
    return ConstSpan(ubpointer)
  }
}

func returnsConstSpanOfInt(_ arr: inout [Int32]) -> ConstSpan {
  return arr.withUnsafeBufferPointer { ubpointer in
    return ConstSpan(ubpointer)
  }
}

func returnsSpanOfInt() -> Span {
  var arr: [Int32] = [1, 2, 3]
  return arr.withUnsafeMutableBufferPointer { ubpointer in
    return Span(ubpointer)
  }
}

func returnsSpanOfInt(_ arr: inout [Int32]) -> Span {
  return arr.withUnsafeMutableBufferPointer { ubpointer in
    return Span(ubpointer)
  }
}

func returnsConstSpanOfString() -> ConstSpanOfString {
  let arr: [std.string] = ["", "a", "ab", "abc"]
  return arr.withUnsafeBufferPointer { ubpointer in
    return ConstSpanOfString(ubpointer)
  }
}

func returnsConstSpanOfString(_ arr: inout [std.string]) -> ConstSpanOfString {
  return arr.withUnsafeBufferPointer { ubpointer in
    return ConstSpanOfString(ubpointer)
  }
}

func returnsSpanOfString() -> SpanOfString {
  var arr: [std.string] = ["", "a", "ab", "abc"]
  return arr.withUnsafeMutableBufferPointer { ubpointer in
    return SpanOfString(ubpointer)
  }
}

func returnsSpanOfString(_ arr: inout [std.string]) -> SpanOfString {
  return arr.withUnsafeMutableBufferPointer { ubpointer in
    return SpanOfString(ubpointer)
  }
}

func accessSpanAsGenericParam<T: RandomAccessCollection>(_ col: T) 
                                              where T.Index == Int, T.Element == Int32 {
  expectEqual(col.count, 3)                     
  expectFalse(col.isEmpty)

  expectEqual(col[0], 1)
  expectEqual(col[1], 2)
  expectEqual(col[2], 3)
}

func accessSpanAsGenericParam<T: RandomAccessCollection>(_ col: T) 
                                              where T.Index == Int, T.Element == std.string {
  expectEqual(col.count, 3)                     
  expectFalse(col.isEmpty)

  expectEqual(col[0], "")
  expectEqual(col[1], "ab")
  expectEqual(col[2], "abc")
}

func accessSpanAsSomeGenericParam(_ col: some CxxRandomAccessCollection) {
  expectEqual(col.count, 3)                     
  expectFalse(col.isEmpty)

  if let el = col[0] as? Int32 {
    expectEqual(el, 1)
    expectEqual(col[1] as! Int32, 2)
    expectEqual(col[2] as! Int32, 3)
  } else if let el = col[0] as? std.string {
    expectEqual(el, "")
    expectEqual(col[1] as! std.string, "ab")
    expectEqual(col[2] as! std.string, "abc")
  }
}

StdSpanTestSuite.test("EmptySpan") {
  let s = Span()
  expectEqual(s.size(), 0)
  expectTrue(s.empty())

  let cs = ConstSpan()
  expectEqual(cs.size(), 0)
  expectTrue(cs.empty())
}

StdSpanTestSuite.test("Init SpanOfInt") {
  let s = initSpan()
  expectEqual(s.size(), 3)
  expectFalse(s.empty())

  let cs = initConstSpan()
  expectEqual(cs.size(), 3)
  expectFalse(cs.empty())
}

StdSpanTestSuite.test("Init SpanOfInt from Swift array") {
  var arr: [Int32] = [1, 2, 3]
  arr.withUnsafeMutableBufferPointer{ ubpointer in
    let s = initSpan(ubpointer.baseAddress!, ubpointer.count)
    expectEqual(s.size(), 3)
    expectFalse(s.empty())
    expectEqual(s[0], 1)
    expectEqual(s[1], 2)
    expectEqual(s[2], 3)
  }
}

StdSpanTestSuite.test("Access static SpanOfInt") {
  expectEqual(icspan.size(), 3)
  expectFalse(icspan.empty())

  expectEqual(icspan[0], 1)
  expectEqual(icspan[1], 2)
  expectEqual(icspan[2], 3)

  expectEqual(ispan.size(), 3)
  expectFalse(ispan.empty())

  expectEqual(ispan[0], 1)
  expectEqual(ispan[1], 2)
  ispan[2] = 4
  expectEqual(ispan[2], 4)

  ispan[2] = 3
  expectEqual(ispan[2], 3)
}

StdSpanTestSuite.test("Access static SpanOfString") {
  expectEqual(scspan.size(), 3)
  expectFalse(scspan.empty())

  expectEqual(scspan[0], "")
  expectEqual(scspan[1], "ab")
  expectEqual(scspan[2], "abc")

  expectEqual(sspan.size(), 3)
  expectFalse(sspan.empty())

  expectEqual(sspan[0], "")
  expectEqual(sspan[1], "ab")
  sspan[2] = "abcd"
  expectEqual(sspan[2], "abcd")

  sspan[2] = "abc"
  expectEqual(sspan[2], "abc")
}

StdSpanTestSuite.test("SpanOfInt as Param") {
  takesConstSpanOfInt(icspan)
  takesSpanOfInt(&ispan)
}

StdSpanTestSuite.test("SpanOfString as Param") {
  takesConstSpanOfString(scspan)
  takesSpanOfString(&sspan)
}

StdSpanTestSuite.test("Return SpanOfInt") {
  let cs1 = returnsConstSpanOfInt()
  expectEqual(cs1.size(), 3)
  expectFalse(cs1.empty())

  let s1 = returnsSpanOfInt()
  expectEqual(s1.size(), 3)
  expectFalse(s1.empty())

  var arr: [Int32] = [4, 5, 6, 7]
  let cs2 = returnsConstSpanOfInt(&arr)
  checkSpan(cs2, arr)

  let s2 = returnsSpanOfInt(&arr)
  checkSpan(s2, arr)
}

StdSpanTestSuite.test("Return SpanOfString") {
  let cs1 = returnsConstSpanOfString()
  expectEqual(cs1.size(), 4)
  expectFalse(cs1.empty())
  let s1 = returnsSpanOfString()
  expectEqual(s1.size(), 4)
  expectFalse(s1.empty())

  var arr: [std.string] = ["", "a", "ab"]
  let cs2 = returnsConstSpanOfString(&arr)
  checkSpan(cs2, arr)

  let s2 = returnsSpanOfString(&arr)
  checkSpan(s2, arr)
}

StdSpanTestSuite.test("SpanOfInt.init(addr, count)") {
  var arr: [Int32] = [1, 2, 3]
  arr.withUnsafeBufferPointer { ubpointer in
    let cs = ConstSpan(ubpointer.baseAddress!, ubpointer.count)    
    checkSpan(cs, arr)
  }

  let arrCopy = arr
  arr.withUnsafeMutableBufferPointer { ubpointer in 
    let s = Span(ubpointer.baseAddress!, ubpointer.count)
    checkSpan(s, arrCopy)
    let cs = ConstSpan(ubpointer.baseAddress!, ubpointer.count)
    checkSpan(cs, arrCopy)
  }
}

StdSpanTestSuite.test("SpanOfInt.init(ubpointer)") {
  var arr: [Int32] = [1, 2, 3]
  arr.withUnsafeBufferPointer { ubpointer in 
    let cs = ConstSpan(ubpointer)
    checkSpan(cs, arr)  
  }

  let arrCopy = arr
  arr.withUnsafeMutableBufferPointer { umbpointer in
    let s = Span(umbpointer)
    checkSpan(s, arrCopy)
    let cs = ConstSpan(umbpointer)
    checkSpan(cs, arrCopy)
  }
}

StdSpanTestSuite.test("SpanOfString.init(addr, count)") {
  var arr: [std.string] = ["", "a", "ab", "abc"]
  arr.withUnsafeBufferPointer { ubpointer in 
    let cs = ConstSpanOfString(ubpointer.baseAddress!, ubpointer.count)
    checkSpan(cs, arr)  
  }

  let arrCopy = arr
  arr.withUnsafeMutableBufferPointer { ubpointer in
    let cs = ConstSpanOfString(ubpointer.baseAddress!, ubpointer.count)
    checkSpan(cs, arrCopy)
    let s = SpanOfString(ubpointer.baseAddress!, ubpointer.count)
    checkSpan(s, arrCopy)
  }
}

StdSpanTestSuite.test("SpanOfString.init(ubpointer)") {
  var arr: [std.string] = ["", "a", "ab", "abc"]
  arr.withUnsafeBufferPointer { ubpointer in
    let cs = ConstSpanOfString(ubpointer)
    checkSpan(cs, arr)
  }

  let arrCopy = arr
  arr.withUnsafeMutableBufferPointer { ubpointer in
    let s = SpanOfString(ubpointer)
    checkSpan(s, arrCopy)
    let cs = ConstSpanOfString(ubpointer)
    checkSpan(cs, arrCopy)
  }
}

StdSpanTestSuite.test("SpanOfInt for loop") {
  var arr: [Int32] = [1, 2, 3]
  arr.withUnsafeBufferPointer { ubpointer in
    let cs = ConstSpan(ubpointer)
    var count: Int32 = 1
    for e in cs {
      expectEqual(e, count)
      count += 1
    }
    expectEqual(count, 4)
  }

  arr.withUnsafeMutableBufferPointer { ubpointer in
    let s = Span(ubpointer)
    var count: Int32 = 1
    for e in s {
      expectEqual(e, count)
      count += 1
    }
    expectEqual(count, 4)
  }
}

StdSpanTestSuite.test("SpanOfString for loop") {
  var arr: [std.string] = ["", "a", "ab", "abc"]
  arr.withUnsafeBufferPointer { ubpointer in
    let s = ConstSpanOfString(ubpointer)
    var count = 0
    for e in s {
      count += e.length();
    }
    expectEqual(count, 6)
  }

  arr.withUnsafeMutableBufferPointer { ubpointer in
    let s = SpanOfString(ubpointer)
    var count = 0
    for e in s {
      count += e.length();
    }
    expectEqual(count, 6)
  }
}

StdSpanTestSuite.test("SpanOfInt.map") {
  var arr: [Int32] = [1, 2, 3]
  arr.withUnsafeBufferPointer { ubpointer in
    let s = ConstSpan(ubpointer)
    let result = s.map { $0 + 5 }
    expectEqual(result, [6, 7, 8])
  }

  arr.withUnsafeMutableBufferPointer { ubpointer in
    let s = Span(ubpointer)
    let result = s.map { $0 + 5 }
    expectEqual(result, [6, 7, 8])
  }
}

StdSpanTestSuite.test("SpanOfString.map") {
  var arr: [std.string] = ["", "a", "ab", "abc"]
  arr.withUnsafeBufferPointer { ubpointer in
    let s = ConstSpanOfString(ubpointer)
    let result = s.map { $0.length() }
    expectEqual(result, [0, 1, 2, 3])
  }

  arr.withUnsafeMutableBufferPointer { ubpointer in
    let s = SpanOfString(ubpointer)
    let result = s.map { $0.length() }
    expectEqual(result, [0, 1, 2, 3])
  }
}

StdSpanTestSuite.test("SpanOfInt.filter") {
  var arr: [Int32] = [1, 2, 3, 4, 5]
  arr.withUnsafeBufferPointer { ubpointer in
    let s = ConstSpan(ubpointer)
    let result = s.filter { $0 > 3 }
    expectEqual(result.count, 2)
    expectEqual(result, [4, 5])
  }

  arr.withUnsafeMutableBufferPointer { ubpointer in
    let s = Span(ubpointer)
    let result = s.filter { $0 > 3 }
    expectEqual(result.count, 2)
    expectEqual(result, [4, 5])
  }
}

StdSpanTestSuite.test("SpanOfString.filter") {
  var arr: [std.string] = ["", "a", "ab", "abc"]
  arr.withUnsafeBufferPointer { ubpointer in
    let s = ConstSpanOfString(ubpointer)
    let result = s.filter { $0.length() > 1}
    expectEqual(result.count, 2)
    expectEqual(result, ["ab", "abc"])
  }

  arr.withUnsafeMutableBufferPointer { ubpointer in
    let s = SpanOfString(ubpointer)
    let result = s.filter { $0.length() > 1}
    expectEqual(result.count, 2)
    expectEqual(result, ["ab", "abc"])
  }
}

StdSpanTestSuite.test("Initialize Array from SpanOfInt") {
  var arr: [Int32] = [1, 2, 3]
  let cspan: ConstSpan = returnsConstSpanOfInt(&arr)
  let newArr1 = Array(cspan)
  expectEqual(arr.count, newArr1.count)
  expectEqual(arr, newArr1)

  let span: Span = returnsSpanOfInt(&arr)
  let newArr2 = Array(span)
  expectEqual(arr.count, newArr2.count)
  expectEqual(arr, newArr2)
}

StdSpanTestSuite.test("Initialize Array from SpanOfString") {
  var arr: [std.string] = ["", "a", "ab"]
  let cspan: ConstSpanOfString = returnsConstSpanOfString(&arr)
  let newArr1 = Array(cspan)
  expectEqual(arr.count, newArr1.count)
  expectEqual(arr, newArr1)

  let span: SpanOfString = returnsSpanOfString(&arr)
  let newArr2 = Array(span)
  expectEqual(arr.count, newArr2.count)
  expectEqual(arr, newArr2)
}

StdSpanTestSuite.test("rdar://126570011") {
  var cp = CppApi()
  let span = cp.getSpan()
  expectFalse(span.empty())
  expectEqual(span.size(), 2)

  let constSpan = cp.getConstSpan()
  expectFalse(constSpan.empty())
  expectEqual(constSpan.size(), 2)
}

StdSpanTestSuite.test("Span inside C++ struct") {
  let spb = getStructSpanBox()
  expectEqual(spb.ispan.size(), 3)
  expectFalse(spb.ispan.empty())
  expectEqual(spb.sspan.size(), 3)
  expectFalse(spb.sspan.empty())

  var icount: Int32 = 1
  for e in spb.ispan {
    expectEqual(e, icount)
    icount += 1
  }

  var scount = 0
    for e in spb.sspan {
      scount += e.length();
    }

  let imapResult = spb.ispan.map { $0 + 5 }
  expectEqual(imapResult, [6, 7, 8])

  let smapResult = spb.sspan.map { $0.length() }
  expectEqual(smapResult, [0, 2, 3])

  let ifilterResult = spb.ispan.filter { $0 > 2 }
  expectEqual(ifilterResult.count, 1)
  expectEqual(ifilterResult, [3])

  let sfilterResult = spb.sspan.filter { $0.length() > 1}
  expectEqual(sfilterResult.count, 2)
  expectEqual(sfilterResult, ["ab", "abc"])
}

StdSpanTestSuite.test("Span inside C++ struct") {
  let spb = getStructSpanBox()
  expectEqual(spb.icspan.size(), 3)
  expectFalse(spb.icspan.empty())
  expectEqual(spb.ispan.size(), 3)
  expectFalse(spb.ispan.empty())
  expectEqual(spb.sspan.size(), 3)
  expectFalse(spb.sspan.empty())
  expectEqual(spb.scspan.size(), 3)
  expectFalse(spb.scspan.empty())

  var icount: Int32 = 1
  for e in spb.icspan {
    expectEqual(e, icount)
    icount += 1
  }

  icount = 1
  for e in spb.ispan {
    expectEqual(e, icount)
    icount += 1
  }

  var scount = 0
  for e in spb.scspan {
    scount += e.length();
  }

  scount = 0
  for e in spb.sspan {
    scount += e.length();
  }

  let icmapResult = spb.icspan.map { $0 + 5 }
  expectEqual(icmapResult, [6, 7, 8])
  let imapResult = spb.ispan.map { $0 + 5 }
  expectEqual(imapResult, [6, 7, 8])

  let scmapResult = spb.scspan.map { $0.length() }
  expectEqual(scmapResult, [0, 2, 3])
  let smapResult = spb.sspan.map { $0.length() }
  expectEqual(smapResult, [0, 2, 3])

  let icfilterResult = spb.icspan.filter { $0 > 2 }
  expectEqual(icfilterResult.count, 1)
  expectEqual(icfilterResult, [3])
  let ifilterResult = spb.ispan.filter { $0 > 2 }
  expectEqual(ifilterResult.count, 1)
  expectEqual(ifilterResult, [3])

  let scfilterResult = spb.scspan.filter { $0.length() > 1}
  expectEqual(scfilterResult.count, 2)
  expectEqual(scfilterResult, ["ab", "abc"])
  let sfilterResult = spb.sspan.filter { $0.length() > 1}
  expectEqual(sfilterResult.count, 2)
  expectEqual(sfilterResult, ["ab", "abc"])
}

StdSpanTestSuite.test("Span inside Swift struct") {
  struct SpanBox {
    var icspan: ConstSpan
    var ispan: Span
    var scspan: ConstSpanOfString
    var sspan: SpanOfString
  }

  let spb = SpanBox(
    icspan: icspan,
    ispan: ispan, 
    scspan: scspan,
    sspan: sspan)

  expectEqual(spb.icspan.size(), 3)
  expectFalse(spb.icspan.empty())
  expectEqual(spb.ispan.size(), 3)
  expectFalse(spb.ispan.empty())
  expectEqual(spb.sspan.size(), 3)
  expectFalse(spb.sspan.empty())
  expectEqual(spb.scspan.size(), 3)
  expectFalse(spb.scspan.empty())

  var icount: Int32 = 1
  for e in spb.icspan {
    expectEqual(e, icount)
    icount += 1
  }

  icount = 1
  for e in spb.ispan {
    expectEqual(e, icount)
    icount += 1
  }

  var scount = 0
  for e in spb.scspan {
    scount += e.length();
  }

  scount = 0
  for e in spb.sspan {
    scount += e.length();
  }

  let icmapResult = spb.icspan.map { $0 + 5 }
  expectEqual(icmapResult, [6, 7, 8])
  let imapResult = spb.ispan.map { $0 + 5 }
  expectEqual(imapResult, [6, 7, 8])

  let smapResult = spb.sspan.map { $0.length() }
  expectEqual(smapResult, [0, 2, 3])
  let scmapResult = spb.scspan.map { $0.length() }
  expectEqual(scmapResult, [0, 2, 3])

  let icfilterResult = spb.icspan.filter { $0 > 2 }
  expectEqual(icfilterResult.count, 1)
  expectEqual(icfilterResult, [3])
  let ifilterResult = spb.ispan.filter { $0 > 2 }
  expectEqual(ifilterResult.count, 1)
  expectEqual(ifilterResult, [3])

  let scfilterResult = spb.scspan.filter { $0.length() > 1}
  expectEqual(scfilterResult.count, 2)
  expectEqual(scfilterResult, ["ab", "abc"])
  let sfilterResult = spb.sspan.filter { $0.length() > 1}
  expectEqual(sfilterResult.count, 2)
  expectEqual(sfilterResult, ["ab", "abc"])
}

StdSpanTestSuite.test("Span as arg to generic func") {
  accessSpanAsGenericParam(icspan)
  accessSpanAsGenericParam(ispan)
  accessSpanAsGenericParam(scspan)
  accessSpanAsGenericParam(sspan)
  accessSpanAsSomeGenericParam(icspan)
  accessSpanAsSomeGenericParam(ispan)
  accessSpanAsSomeGenericParam(scspan)
  accessSpanAsSomeGenericParam(sspan)
}

runAllTests()

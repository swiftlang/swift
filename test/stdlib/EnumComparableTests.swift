// RUN: %target-run-simple-swift %t
// REQUIRES: executable_test

import StdlibUnittest

var SynthesizedComparableTests = TestSuite("SynthesizedComparableTests")

SynthesizedComparableTests.test("Simple Enum sorting") {
  enum Album: Comparable {
    case debut, be, fearless, sn, red, roses, reputation, lover
  }
  
  let unsorted: [Album] = [.be, .debut, .lover, .reputation, .sn, .fearless, .roses, .red]

  expectEqual(unsorted.sorted(), [.debut, .be, .fearless, .sn, .red, .roses, .reputation, .lover])
}

SynthesizedComparableTests.test("Simple Enum sorting with duplicates") {
  enum Album: Comparable {
    case debut, be, fearless, sn, red, roses, reputation, lover
  }
  
  let unsorted: [Album] = [.be, .debut, .lover, .lover, .reputation, .sn, .sn, .fearless, .roses, .red]

  expectEqual(Album.fearless == Album.fearless, true)
  expectEqual(Album.fearless <  Album.red, true)
  expectEqual(Album.fearless <  Album.fearless, false)
  expectEqual(unsorted.sorted(), [.debut, .be, .fearless, .sn, .sn, .red, .roses, .reputation, .lover, .lover])
}

SynthesizedComparableTests.test("Associated Values Enum sorting") {
  enum Bar:Comparable 
  {
    case a(Int, Int)
    case b(Int)
    case c 
  }
  
  let unsorted:[Bar] = [.b(89), .a(12, 4), .c, .a(5, 4), .b(9), .a(5, 1)]

  expectEqual(unsorted.sorted(), [.a(5, 1), .a(5, 4), .a(12, 4), .b(9), .b(89), .c])
}

runAllTests()

// RUN: %target-run-simple-swift
// REQUIRES: executable_test

// These test a codepath that was fixed in the Swift 5.9 stdlib, so it will
// fail if run against earlier standard library versions.
// UNSUPPORTED: use_os_stdlib
// UNSUPPORTED: back_deployment_runtime

import StdlibUnittest

var suite = TestSuite("DurationTests")
defer { runAllTests() }

if #available(SwiftStdlib 5.7, *) {
  suite.test("seconds from Double") {
    for _ in 0 ..< 100 {
      let integerValue = Double(Int64.random(in: 0 ... 0x7fff_ffff_ffff_fc00))
      let (sec, attosec) = Duration.seconds(integerValue).components
      expectEqual(sec, Int64(integerValue))
      expectEqual(attosec, 0)
    }
    let quarterSecond = Duration.seconds(0.25)
    expectEqual(quarterSecond.components, (0, 250_000_000_000_000_000))
    // Value that overflows conversion from Double -> Int64, but should be
    // representable as a number of seconds:
    let huge: Double = 1.7e20
    let duration = Duration.seconds(huge)
    // Divide by 1000 to get back to a duration with representable components:
    let smallerDuration = duration / 1000
    expectEqual(smallerDuration.components, (170_000_000_000_000_000, 0))
#if !os(WASI)
    // Now check that the components of the original value trap:
    expectCrashLater()
    let _ = duration.components
#endif
  }
  
  suite.test("milliseconds from Double") {
    for _ in 0 ..< 100 {
      let integerValue = Double(Int64.random(in: 0 ... 0x7fff_ffff_ffff_fc00))
      let (sec, attosec) = Duration.milliseconds(integerValue).components
      expectEqual(sec, Int64(integerValue) / 1000)
      expectEqual(attosec, Int64(integerValue) % 1000 * 1_000_000_000_000_000)
    }
  }
  
  suite.test("microseconds from Double") {
    for _ in 0 ..< 100 {
      let integerValue = Double(Int64.random(in: 0 ... 0x7fff_ffff_ffff_fc00))
      let (sec, attosec) = Duration.microseconds(integerValue).components
      expectEqual(sec, Int64(integerValue) / 1_000_000)
      expectEqual(attosec, Int64(integerValue) % 1_000_000 * 1_000_000_000_000)
    }
  }
  
  suite.test("seconds from Int64") {
    let one = Duration.seconds(1 as Int64)
    expectEqual(one._high, 0)
    expectEqual(one._low, 1_000_000_000_000_000_000)
    let mone = Duration.seconds(-1 as Int64)
    expectEqual(mone._high, -1)
    expectEqual(mone._low, .max - 999_999_999_999_999_999)
    let max64 = Duration.seconds(Int64.max)
    expectEqual(max64._high, 499_999_999_999_999_999)
    expectEqual(max64._low, .max - 999_999_999_999_999_999)
    let min64 = Duration.seconds(Int64.min)
    expectEqual(min64._high,-500_000_000_000_000_000)
    expectEqual(min64._low, 0)
  }
  
  suite.test("seconds from UInt64") {
    let one = Duration.seconds(1 as UInt64)
    expectEqual(one._high, 0)
    expectEqual(one._low, 1_000_000_000_000_000_000)
    let max64 = Duration.seconds(UInt64.max)
    expectEqual(max64._high, 999_999_999_999_999_999)
    expectEqual(max64._low, .max - 999_999_999_999_999_999)
  }
  
  suite.test("milliseconds from Int64") {
    let one = Duration.milliseconds(1 as Int64)
    expectEqual(one._high, 0)
    expectEqual(one._low, 1_000_000_000_000_000)
    let mone = Duration.milliseconds(-1 as Int64)
    expectEqual(mone._high, -1)
    expectEqual(mone._low, .max - 999_999_999_999_999)
    let max64 = Duration.milliseconds(Int64.max)
    expectEqual(max64._high, 499_999_999_999_999)
    expectEqual(max64._low, .max - 999_999_999_999_999)
    let min64 = Duration.milliseconds(Int64.min)
    expectEqual(min64._high,-500_000_000_000_000)
    expectEqual(min64._low, 0)
  }
  
  suite.test("milliseconds from UInt64") {
    let one = Duration.milliseconds(1 as UInt64)
    expectEqual(one._high, 0)
    expectEqual(one._low, 1_000_000_000_000_000)
    let max64 = Duration.milliseconds(UInt64.max)
    expectEqual(max64._high, 999_999_999_999_999)
    expectEqual(max64._low, .max - 999_999_999_999_999)
  }
  
  suite.test("microseconds from Int64") {
    let one = Duration.microseconds(1 as Int64)
    expectEqual(one._high, 0)
    expectEqual(one._low, 1_000_000_000_000)
    let mone = Duration.microseconds(-1 as Int64)
    expectEqual(mone._high, -1)
    expectEqual(mone._low, .max - 999_999_999_999)
    let max64 = Duration.microseconds(Int64.max)
    expectEqual(max64._high, 499_999_999_999)
    expectEqual(max64._low, .max - 999_999_999_999)
    let min64 = Duration.microseconds(Int64.min)
    expectEqual(min64._high,-500_000_000_000)
    expectEqual(min64._low, 0)
  }
  
  suite.test("microseconds from UInt64") {
    let one = Duration.microseconds(1 as UInt64)
    expectEqual(one._high, 0)
    expectEqual(one._low, 1_000_000_000_000)
    let max64 = Duration.microseconds(UInt64.max)
    expectEqual(max64._high, 999_999_999_999)
    expectEqual(max64._low, .max - 999_999_999_999)
  }
  
  suite.test("nanoseconds from Int64") {
    let one = Duration.nanoseconds(1 as Int64)
    expectEqual(one._high, 0)
    expectEqual(one._low, 1_000_000_000)
    let mone = Duration.nanoseconds(-1 as Int64)
    expectEqual(mone._high, -1)
    expectEqual(mone._low, .max - 999_999_999)
    let max64 = Duration.nanoseconds(Int64.max)
    expectEqual(max64._high, 499_999_999)
    expectEqual(max64._low, .max - 999_999_999)
    let min64 = Duration.nanoseconds(Int64.min)
    expectEqual(min64._high,-500_000_000)
    expectEqual(min64._low, 0)
  }
  
  suite.test("nanoseconds from UInt64") {
    let one = Duration.nanoseconds(1 as UInt64)
    expectEqual(one._high, 0)
    expectEqual(one._low, 1_000_000_000)
    let max64 = Duration.nanoseconds(UInt64.max)
    expectEqual(max64._high, 999_999_999)
    expectEqual(max64._low, .max - 999_999_999)
  }
}
 
if #available(SwiftStdlib 6.0, *) {
  suite.test("seconds from Int128") {
    let one = Duration.seconds(1 as Int128)
    expectEqual(one._high, 0)
    expectEqual(one._low, 1_000_000_000_000_000_000)
    let mone = Duration.seconds(-1 as Int128)
    expectEqual(mone._high, -1)
    expectEqual(mone._low, .max - 999_999_999_999_999_999)
    let maxRep = Duration.seconds( 170141183460469231731 as Int128)
    expectEqual(maxRep._high, 9_223_372_036_854_775_807)
    expectEqual(maxRep._low, 17_759_440_357_825_445_888)
    //  negative overflow boundary is _smaller_ than positive for seconds;
    //  this could be avoided by reworking how highScaled is computed, but
    //  it's already so large (5 trillion years) that this probably isn't
    //  necessary.
    let minRep = Duration.seconds(-166020696663385964544 as Int128)
    expectEqual(minRep._high,-9_000_000_000_000_000_000)
    expectEqual(minRep._low, 0)
    #if !os(WASI)
    //  Check just above the overflow boundary
    expectCrashLater()
    let _ = Duration.seconds( 170141183460469231732 as Int128)
    #endif
  }
  
  suite.test("milliseconds from Int128") {
    let one = Duration.milliseconds(1 as Int128)
    expectEqual(one._high, 0)
    expectEqual(one._low, 1_000_000_000_000_000)
    let mone = Duration.milliseconds(-1 as Int128)
    expectEqual(mone._high, -1)
    expectEqual(mone._low, .max - 999_999_999_999_999)
    let maxRep = Duration.milliseconds( 170141183460469231731687 as Int128)
    expectEqual(maxRep._high, 9_223_372_036_854_775_807)
    expectEqual(maxRep._low, 18_446_440_357_825_445_888)
    let minRep = Duration.milliseconds(-170134320591823194554368 as Int128)
    expectEqual(minRep._high,-9_223_000_000_000_000_000)
    expectEqual(minRep._low, 0)
    #if !os(WASI)
    //  Check just above the overflow boundary
    expectCrashLater()
    let _ = Duration.milliseconds( 170141183460469231731689 as Int128)
    #endif
  }
  
  suite.test("microseconds from Int128") {
    let one = Duration.microseconds(1 as Int128)
    expectEqual(one._high, 0)
    expectEqual(one._low, 1_000_000_000_000)
    let mone = Duration.microseconds(-1 as Int128)
    expectEqual(mone._high, -1)
    expectEqual(mone._low, .max - 999_999_999_999)
    let maxRep = Duration.microseconds( 170141183460469231731687303 as Int128)
    expectEqual(maxRep._high, 9_223_372_036_854_775_807)
    expectEqual(maxRep._low, 18_446_743_357_825_445_888)
    let minRep = Duration.microseconds(-170141182780618614507569152 as Int128)
    expectEqual(minRep._high,-9_223_372_000_000_000_000)
    expectEqual(minRep._low, 0)
    #if !os(WASI)
    //  Check just above the overflow boundary
    expectCrashLater()
    let _ = Duration.microseconds( 170141183460469231731687304 as Int128)
    #endif
  }
  
  suite.test("nanoseconds from Int128") {
    let one = Duration.nanoseconds(1 as Int128)
    expectEqual(one._high, 0)
    expectEqual(one._low, 1_000_000_000)
    let mone = Duration.nanoseconds(-1 as Int128)
    expectEqual(mone._high, -1)
    expectEqual(mone._low, .max - 999_999_999)
    let maxRep = Duration.nanoseconds( 170141183460469231731687303715 as Int128)
    expectEqual(maxRep._high, 9_223_372_036_854_775_807)
    expectEqual(maxRep._low, 18_446_744_072_825_445_888)
    let minRep = Duration.nanoseconds(-170141183444701401161113010176 as Int128)
    expectEqual(minRep._high,-9_223_372_036_000_000_000)
    expectEqual(minRep._low, 0)
    #if !os(WASI)
    //  Check just above the overflow boundary
    expectCrashLater()
    let _ = Duration.nanoseconds( 170141183460469231731687303716 as Int128)
    #endif
  }
  
  suite.test("attoseconds init") {
    let zero = Duration(attoseconds: 0)
    expectEqual(zero._high, 0)
    expectEqual(zero._low, 0)
    let one = Duration(attoseconds: 1)
    expectEqual(one._high, 0)
    expectEqual(one._low, 1)
    let mone = Duration(attoseconds: -1)
    expectEqual(mone._high, -1)
    expectEqual(mone._low, .max)
    let max = Duration(attoseconds: .max)
    expectEqual(max._high, 9_223_372_036_854_775_807)
    expectEqual(max._low, 18_446_744_073_709_551_615)
    let min = Duration(attoseconds: .min)
    expectEqual(min._high, -9_223_372_036_854_775_808)
    expectEqual(min._low, 0)
  }
  
  suite.test("attoseconds var") {
    let zero = Duration(_high: 0, low: 0)
    expectEqual(zero.attoseconds, 0)
    let one = Duration(_high: 0, low: 1)
    expectEqual(one.attoseconds, 1)
    let mone = Duration(_high: -1, low: .max)
    expectEqual(mone.attoseconds, -1)
    let max = Duration(_high: 9_223_372_036_854_775_807, low: 18_446_744_073_709_551_615)
    expectEqual(max.attoseconds, .max)
    let min = Duration(_high: -9_223_372_036_854_775_808, low: 0)
    expectEqual(min.attoseconds, .min)
  }
}

if #available(SwiftStdlib 6.2, *) {
  suite.test("nanoseconds from Double") {
    for _ in 0 ..< 100 {
      let integerValue = Double(Int64.random(in: 0 ... 0x7fff_ffff_ffff_fc00))
      let (sec, attosec) = Duration.nanoseconds(integerValue).components
      expectEqual(sec, Int64(integerValue) / 1_000_000_000)
      expectEqual(attosec, Int64(integerValue) % 1_000_000_000 * 1_000_000_000)
    }
  }
}

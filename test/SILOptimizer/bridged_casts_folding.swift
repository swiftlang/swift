// RUN: %target-swift-frontend -O -emit-sil %s | %FileCheck %s

// REQUIRES: objc_interop

// FIXME: https://bugs.swift.org/browse/SR-2808
// XFAIL: resilient_stdlib

// Check that casts between bridged types are replaced by more 
// efficient code sequences.
// 
// In particular, checked_cast_* and unconditional_checked_* instructions,
// which are pretty expensive at run-time (e.g. because they use
// runtime _dynamicCast calls and check conformances at run-time),
// should be replaced by invocations of specialized bridging functions,
// which make use of statically known compile-time conformances and 
// do not perform any conformance checks at run-time.

import Foundation

public func forcedCast<NS, T>(_ ns: NS) -> T {
  return ns as! T
}

public func condCast<NS, T>(_ ns: NS) -> T? {
  return ns as? T
}

// Check optimizations of casts from NSString to String

var nsString: NSString = "string"

// CHECK-LABEL: sil [noinline] @_T021bridged_casts_folding29testForcedCastNStoSwiftStringSSyF
// CHECK-NOT: unconditional_checked
// CHECK: function_ref @_T0SS10FoundationE26_forceBridgeFromObjectiveCySo8NSStringC_SSSgz6resulttFZ : $@convention(method) (@owned NSString, @inout Optional<String>, @thin String.Type) -> ()
// CHECK: return
@inline(never)
public func testForcedCastNStoSwiftString() -> String {
  var o: String = forcedCast(nsString)
  return o
}

// CHECK-LABEL: sil [noinline] @_T021bridged_casts_folding27testCondCastNStoSwiftStringSSSgyF
// CHECK-NOT: checked_cast
// CHECK: function_ref @_T0SS10FoundationE34_conditionallyBridgeFromObjectiveCSbSo8NSStringC_SSSgz6resulttFZ : $@convention(method) (@owned NSString, @inout Optional<String>, @thin String.Type) -> Bool
// CHECK: return
@inline(never)
public func testCondCastNStoSwiftString() -> String? {
  var o: String? = condCast(nsString)
  return o
}


// Check optimizations of casts from NSNumber to Int

var nsIntNumber = NSNumber(value: 1)

// CHECK-LABEL: sil [noinline] @_T021bridged_casts_folding32testForcedCastNSNumberToSwiftIntSiyF
// CHECK-NOT: unconditional_checked
// CHECK: function_ref @_T0Si10FoundationE26_forceBridgeFromObjectiveCySo8NSNumberC_SiSgz6resulttFZ : $@convention(method) (@owned NSNumber, @inout Optional<Int>, @thin Int.Type) -> ()
// CHECK: return
@inline(never)
public func testForcedCastNSNumberToSwiftInt() -> Int {
  var o: Int = forcedCast(nsIntNumber)
  return o
}

// CHECK-LABEL: sil [noinline] @_T021bridged_casts_folding30testCondCastNSNumberToSwiftIntSiSgyF
// CHECK-NOT: checked_cast
// CHECK: function_ref @_T0Si10FoundationE34_conditionallyBridgeFromObjectiveCSbSo8NSNumberC_SiSgz6resulttFZ : $@convention(method) (@owned NSNumber, @inout Optional<Int>, @thin Int.Type) -> Bool
// CHECK: return
@inline(never)
public func testCondCastNSNumberToSwiftInt() -> Int? {
  var o: Int? = condCast(nsIntNumber)
  return o
}

// Check optimizations of casts from NSNumber to Double

var nsDoubleNumber = NSNumber(value: 1.234)

// CHECK-LABEL: sil [noinline] @_T021bridged_casts_folding35testForcedCastNSNumberToSwiftDoubleSdyF
// CHECK-NOT: unconditional_checked
// CHECK: function_ref @_T0Sd10FoundationE26_forceBridgeFromObjectiveCySo8NSNumberC_SdSgz6resulttFZ : $@convention(method) (@owned NSNumber, @inout Optional<Double>, @thin Double.Type) -> ()
// CHECK: return
@inline(never)
public func testForcedCastNSNumberToSwiftDouble() -> Double {
  var o: Double = forcedCast(nsDoubleNumber)
  return o
}

// CHECK-LABEL: sil [noinline] @_T021bridged_casts_folding33testCondCastNSNumberToSwiftDoubleSdSgyF
// CHECK-NOT: checked_cast
// CHECK: function_ref @_T0Sd10FoundationE34_conditionallyBridgeFromObjectiveCSbSo8NSNumberC_SdSgz6resulttFZ : $@convention(method) (@owned NSNumber, @inout Optional<Double>, @thin Double.Type) -> Bool
// CHECK: return
@inline(never)
public func testCondCastNSNumberToSwiftDouble() -> Double? {
  var o: Double? = condCast(nsDoubleNumber)
  return o
}

// CHECK-LABEL: sil [noinline] @_T021bridged_casts_folding38testForcedCastNSIntNumberToSwiftDoubleSdyF
// CHECK-NOT: unconditional_checked
// CHECK: function_ref @_T0Sd10FoundationE26_forceBridgeFromObjectiveCySo8NSNumberC_SdSgz6resulttFZ : $@convention(method) (@owned NSNumber, @inout Optional<Double>, @thin Double.Type) -> ()
// CHECK: return
@inline(never)
public func testForcedCastNSIntNumberToSwiftDouble() -> Double {
  var o: Double = forcedCast(nsIntNumber)
  return o
}

// CHECK-LABEL: sil [noinline] @_T021bridged_casts_folding36testCondCastNSIntNumberToSwiftDoubleSdSgyF
// CHECK-NOT: checked_cast
// CHECK: function_ref @_T0Sd10FoundationE34_conditionallyBridgeFromObjectiveCSbSo8NSNumberC_SdSgz6resulttFZ : $@convention(method) (@owned NSNumber, @inout Optional<Double>, @thin Double.Type) -> Bool
// CHECK: return
@inline(never)
public func testCondCastNSIntNumberToSwiftDouble() -> Double? {
  var o: Double? = condCast(nsIntNumber)
  return o
}



// Check optimization of casts from NSArray to Swift Array

var nsArrInt: NSArray = [1, 2, 3, 4]
var nsArrDouble: NSArray = [1.1, 2.2, 3.3, 4.4]
var nsArrString: NSArray = ["One", "Two", "Three", "Four"]

// CHECK-LABEL: sil [noinline] @_T021bridged_casts_folding31testForcedCastNStoSwiftArrayIntSaySiGyF
// CHECK-NOT: unconditional_checked
// CHECK: function_ref @_T0Sa10FoundationE26_forceBridgeFromObjectiveCySo7NSArrayC_SayxGSgz6resulttFZ : $@convention(method) <τ_0_0> (@owned NSArray, @inout Optional<Array<τ_0_0>>, @thin Array<τ_0_0>.Type) -> ()
// CHECK: return
@inline(never)
public func testForcedCastNStoSwiftArrayInt() -> [Int] {
  var arr: [Int] = forcedCast(nsArrInt)
  return arr
}

// CHECK-LABEL: sil [noinline] @_T021bridged_casts_folding29testCondCastNStoSwiftArrayIntSaySiGSgyF
// CHECK-NOT: checked_cast
// CHECK: function_ref @_T0Sa10FoundationE34_conditionallyBridgeFromObjectiveCSbSo7NSArrayC_SayxGSgz6resulttFZ : $@convention(method) <τ_0_0> (@owned NSArray, @inout Optional<Array<τ_0_0>>, @thin Array<τ_0_0>.Type) -> Bool
// CHECK: return
@inline(never)
public func testCondCastNStoSwiftArrayInt() -> [Int]? {
  var arrOpt: [Int]? = condCast(nsArrInt)
  return arrOpt
}

// CHECK-LABEL: sil [noinline] @_T021bridged_casts_folding34testForcedCastNStoSwiftArrayDoubleSaySdGyF
// CHECK-NOT: unconditional_checked
// CHECK: function_ref @_T0Sa10FoundationE26_forceBridgeFromObjectiveCySo7NSArrayC_SayxGSgz6resulttFZ : $@convention(method) <τ_0_0> (@owned NSArray, @inout Optional<Array<τ_0_0>>, @thin Array<τ_0_0>.Type) -> ()
// CHECK: return
@inline(never)
public func testForcedCastNStoSwiftArrayDouble() -> [Double] {
  var arr: [Double] = forcedCast(nsArrDouble)
  return arr
}

// CHECK-LABEL: sil [noinline] @_T021bridged_casts_folding32testCondCastNStoSwiftArrayDoubleSaySdGSgyF
// CHECK-NOT: checked_cast
// CHECK: function_ref @_T0Sa10FoundationE34_conditionallyBridgeFromObjectiveCSbSo7NSArrayC_SayxGSgz6resulttFZ : $@convention(method) <τ_0_0> (@owned NSArray, @inout Optional<Array<τ_0_0>>, @thin Array<τ_0_0>.Type) -> Bool
// CHECK: return
@inline(never)
public func testCondCastNStoSwiftArrayDouble() -> [Double]? {
  var arrOpt: [Double]? = condCast(nsArrDouble)
  return arrOpt
}


// CHECK-LABEL: sil [noinline] @_T021bridged_casts_folding34testForcedCastNStoSwiftArrayStringSaySSGyF
// CHECK-NOT: unconditional_checked
// CHECK: function_ref @_T0Sa10FoundationE26_forceBridgeFromObjectiveCySo7NSArrayC_SayxGSgz6resulttFZ : $@convention(method) <τ_0_0> (@owned NSArray, @inout Optional<Array<τ_0_0>>, @thin Array<τ_0_0>.Type) -> ()
// CHECK: return
@inline(never)
public func testForcedCastNStoSwiftArrayString() -> [String] {
  var arr: [String] = forcedCast(nsArrString)
  return arr
}

// CHECK-LABEL: sil [noinline] @_T021bridged_casts_folding32testCondCastNStoSwiftArrayStringSaySSGSgyF
// CHECK-NOT: checked_cast
// CHECK: function_ref @_T0Sa10FoundationE34_conditionallyBridgeFromObjectiveCSbSo7NSArrayC_SayxGSgz6resulttFZ : $@convention(method) <τ_0_0> (@owned NSArray, @inout Optional<Array<τ_0_0>>, @thin Array<τ_0_0>.Type) -> Bool
// CHECK: return
@inline(never)
public func testCondCastNStoSwiftArrayString() -> [String]? {
  var arrOpt: [String]? = condCast(nsArrString)
  return arrOpt
}



// Check optimization of casts from NSDictionary to Swift Dictionary

var nsDictInt: NSDictionary = [1:1, 2:2, 3:3, 4:4]
var nsDictDouble: NSDictionary = [1.1 : 1.1, 2.2 : 2.2, 3.3 : 3.3, 4.4 : 4.4]
var nsDictString: NSDictionary = ["One":"One", "Two":"Two", "Three":"Three", "Four":"Four"]

// CHECK-LABEL: sil [noinline] @_T021bridged_casts_folding30testForcedCastNStoSwiftDictInts10DictionaryVyS2iGyF
// CHECK-NOT: unconditional_checked
// CHECK: function_ref @_T0s10DictionaryV10FoundationE26_forceBridgeFromObjectiveCySo12NSDictionaryC_AByxq_GSgz6resulttFZ : $@convention(method) <τ_0_0, τ_0_1 where τ_0_0 : Hashable> (@owned NSDictionary, @inout Optional<Dictionary<τ_0_0, τ_0_1>>, @thin Dictionary<τ_0_0, τ_0_1>.Type) -> ()
// CHECK: return
@inline(never)
public func testForcedCastNStoSwiftDictInt() -> [Int: Int] {
  var dict: [Int: Int] = forcedCast(nsDictInt)
  return dict
}

// CHECK-LABEL: sil [noinline] @_T021bridged_casts_folding28testCondCastNStoSwiftDictInts10DictionaryVyS2iGSgyF
// CHECK-NOT: checked_cast
// CHECK: function_ref @_T0s10DictionaryV10FoundationE34_conditionallyBridgeFromObjectiveCSbSo12NSDictionaryC_AByxq_GSgz6resulttFZ : $@convention(method) <τ_0_0, τ_0_1 where τ_0_0 : Hashable> (@owned NSDictionary, @inout Optional<Dictionary<τ_0_0, τ_0_1>>, @thin Dictionary<τ_0_0, τ_0_1>.Type) -> Bool
// CHECK: return
@inline(never)
public func testCondCastNStoSwiftDictInt() -> [Int: Int]? {
  var dictOpt: [Int: Int]? = condCast(nsDictInt)
  return dictOpt
}

// CHECK-LABEL: sil [noinline] @_T021bridged_casts_folding33testForcedCastNStoSwiftDictDoubles10DictionaryVyS2dGyF
// CHECK-NOT: unconditional_checked
// CHECK: function_ref @_T0s10DictionaryV10FoundationE26_forceBridgeFromObjectiveCySo12NSDictionaryC_AByxq_GSgz6resulttFZ : $@convention(method) <τ_0_0, τ_0_1 where τ_0_0 : Hashable> (@owned NSDictionary, @inout Optional<Dictionary<τ_0_0, τ_0_1>>, @thin Dictionary<τ_0_0, τ_0_1>.Type) -> ()
// CHECK: return
@inline(never)
public func testForcedCastNStoSwiftDictDouble() -> [Double: Double] {
  var dict: [Double: Double] = forcedCast(nsDictDouble)
  return dict
}

// CHECK-LABEL: sil [noinline] @_T021bridged_casts_folding31testCondCastNStoSwiftDictDoubles10DictionaryVyS2dGSgyF
// CHECK-NOT: checked_cast
// CHECK: function_ref @_T0s10DictionaryV10FoundationE34_conditionallyBridgeFromObjectiveCSbSo12NSDictionaryC_AByxq_GSgz6resulttFZ : $@convention(method) <τ_0_0, τ_0_1 where τ_0_0 : Hashable> (@owned NSDictionary, @inout Optional<Dictionary<τ_0_0, τ_0_1>>, @thin Dictionary<τ_0_0, τ_0_1>.Type) -> Bool
// CHECK: return
@inline(never)
public func testCondCastNStoSwiftDictDouble() -> [Double: Double]? {
  var dictOpt: [Double: Double]? = condCast(nsDictDouble)
  return dictOpt
}


// CHECK-LABEL: sil [noinline] @_T021bridged_casts_folding33testForcedCastNStoSwiftDictStrings10DictionaryVyS2SGyF
// CHECK-NOT: unconditional_checked
// CHECK: function_ref @_T0s10DictionaryV10FoundationE26_forceBridgeFromObjectiveCySo12NSDictionaryC_AByxq_GSgz6resulttFZ : $@convention(method) <τ_0_0, τ_0_1 where τ_0_0 : Hashable> (@owned NSDictionary, @inout Optional<Dictionary<τ_0_0, τ_0_1>>, @thin Dictionary<τ_0_0, τ_0_1>.Type) -> ()
// CHECK: return
@inline(never)
public func testForcedCastNStoSwiftDictString() -> [String: String] {
  var dict: [String: String] = forcedCast(nsDictString)
  return dict
}

// CHECK-LABEL: sil [noinline] @_T021bridged_casts_folding31testCondCastNStoSwiftDictStrings10DictionaryVyS2SGSgyF
// CHECK-NOT: checked_cast
// CHECK: function_ref @_T0s10DictionaryV10FoundationE34_conditionallyBridgeFromObjectiveCSbSo12NSDictionaryC_AByxq_GSgz6resulttFZ : $@convention(method) <τ_0_0, τ_0_1 where τ_0_0 : Hashable> (@owned NSDictionary, @inout Optional<Dictionary<τ_0_0, τ_0_1>>, @thin Dictionary<τ_0_0, τ_0_1>.Type) -> Bool
// CHECK: return
@inline(never)
public func testCondCastNStoSwiftDictString() -> [String: String]? {
  var dictOpt: [String: String]? = condCast(nsDictString)
  return dictOpt
}

// CHECK-LABEL: sil [noinline] @_T021bridged_casts_folding40testForcedCastNSDictStringtoSwiftDictInts10DictionaryVyS2iGyF
// CHECK-NOT: unconditional_checked
// CHECK: function_ref @_T0s10DictionaryV10FoundationE26_forceBridgeFromObjectiveCySo12NSDictionaryC_AByxq_GSgz6resulttFZ : $@convention(method) <τ_0_0, τ_0_1 where τ_0_0 : Hashable> (@owned NSDictionary, @inout Optional<Dictionary<τ_0_0, τ_0_1>>, @thin Dictionary<τ_0_0, τ_0_1>.Type) -> ()
// CHECK: return
@inline(never)
public func testForcedCastNSDictStringtoSwiftDictInt() -> [Int: Int] {
  var dictOpt: [Int: Int] = forcedCast(nsDictString)
  return dictOpt
}


// CHECK-LABEL: sil [noinline] @_T021bridged_casts_folding38testCondCastNSDictStringtoSwiftDictInts10DictionaryVyS2iGSgyF
// CHECK-NOT: checked_cast
// CHECK: function_ref @_T0s10DictionaryV10FoundationE34_conditionallyBridgeFromObjectiveCSbSo12NSDictionaryC_AByxq_GSgz6resulttFZ : $@convention(method) <τ_0_0, τ_0_1 where τ_0_0 : Hashable> (@owned NSDictionary, @inout Optional<Dictionary<τ_0_0, τ_0_1>>, @thin Dictionary<τ_0_0, τ_0_1>.Type) -> Bool
// CHECK: return
@inline(never)
public func testCondCastNSDictStringtoSwiftDictInt() -> [Int: Int]? {
  var dictOpt: [Int: Int]? = condCast(nsDictString)
  return dictOpt
}


// Check optimization of casts from NSSet to Swift Set

var nsSetInt: NSSet = [1, 2, 3, 4]
var nsSetDouble: NSSet = [1.1, 2.2, 3.3, 4.4]
var nsSetString: NSSet = ["One", "Two", "Three", "Four"]

// CHECK-LABEL: sil [noinline] @_T021bridged_casts_folding29testForcedCastNStoSwiftSetInts0I0VySiGyF
// CHECK-NOT: unconditional_checked
// CHECK: function_ref @_T0s3SetV10FoundationE26_forceBridgeFromObjectiveCySo5NSSetC_AByxGSgz6resulttFZ : $@convention(method) <τ_0_0 where τ_0_0 : Hashable> (@owned NSSet, @inout Optional<Set<τ_0_0>>, @thin Set<τ_0_0>.Type) -> ()
// CHECK: return
@inline(never)
public func testForcedCastNStoSwiftSetInt() -> Set<Int> {
  var set: Set<Int> = forcedCast(nsSetInt)
  return set
}

// CHECK-LABEL: sil [noinline] @_T021bridged_casts_folding27testCondCastNStoSwiftSetInts0I0VySiGSgyF
// CHECK-NOT: checked_cast
// CHECK: function_ref @_T0s3SetV10FoundationE34_conditionallyBridgeFromObjectiveCSbSo5NSSetC_AByxGSgz6resulttFZ : $@convention(method) <τ_0_0 where τ_0_0 : Hashable> (@owned NSSet, @inout Optional<Set<τ_0_0>>, @thin Set<τ_0_0>.Type) -> Bool
// CHECK: return
@inline(never)
public func testCondCastNStoSwiftSetInt() -> Set<Int>? {
  var setOpt: Set<Int>? = condCast(nsSetInt)
  return setOpt
}

// CHECK-LABEL: sil [noinline] @_T021bridged_casts_folding32testForcedCastNStoSwiftSetDoubles0I0VySdGyF
// CHECK-NOT: unconditional_checked
// CHECK: function_ref @_T0s3SetV10FoundationE26_forceBridgeFromObjectiveCySo5NSSetC_AByxGSgz6resulttFZ : $@convention(method) <τ_0_0 where τ_0_0 : Hashable> (@owned NSSet, @inout Optional<Set<τ_0_0>>, @thin Set<τ_0_0>.Type) -> ()
// CHECK: return
@inline(never)
public func testForcedCastNStoSwiftSetDouble() -> Set<Double> {
  var set: Set<Double> = forcedCast(nsSetDouble)
  return set
}

// CHECK-LABEL: sil [noinline] @_T021bridged_casts_folding30testCondCastNStoSwiftSetDoubles0I0VySdGSgyF
// CHECK-NOT: checked_cast
// CHECK: function_ref @_T0s3SetV10FoundationE34_conditionallyBridgeFromObjectiveCSbSo5NSSetC_AByxGSgz6resulttFZ : $@convention(method) <τ_0_0 where τ_0_0 : Hashable> (@owned NSSet, @inout Optional<Set<τ_0_0>>, @thin Set<τ_0_0>.Type) -> Bool
// CHECK: return
@inline(never)
public func testCondCastNStoSwiftSetDouble() -> Set<Double>? {
  var setOpt: Set<Double>? = condCast(nsSetDouble)
  return setOpt
}


// CHECK-LABEL: sil [noinline] @_T021bridged_casts_folding32testForcedCastNStoSwiftSetStrings0I0VySSGyF
// CHECK-NOT: unconditional_checked
// CHECK: function_ref @_T0s3SetV10FoundationE26_forceBridgeFromObjectiveCySo5NSSetC_AByxGSgz6resulttFZ : $@convention(method) <τ_0_0 where τ_0_0 : Hashable> (@owned NSSet, @inout Optional<Set<τ_0_0>>, @thin Set<τ_0_0>.Type) -> ()
// CHECK: return
@inline(never)
public func testForcedCastNStoSwiftSetString() -> Set<String> {
  var set: Set<String> = forcedCast(nsSetString)
  return set
}

// CHECK-LABEL: sil [noinline] @_T021bridged_casts_folding30testCondCastNStoSwiftSetStrings0I0VySSGSgyF
// CHECK-NOT: checked_cast
// CHECK: function_ref @_T0s3SetV10FoundationE34_conditionallyBridgeFromObjectiveCSbSo5NSSetC_AByxGSgz6resulttFZ : $@convention(method) <τ_0_0 where τ_0_0 : Hashable> (@owned NSSet, @inout Optional<Set<τ_0_0>>, @thin Set<τ_0_0>.Type) -> Bool
// CHECK: return
@inline(never)
public func testCondCastNStoSwiftSetString() -> Set<String>? {
  var setOpt: Set<String>? = condCast(nsSetString)
  return setOpt
}


// Check optimizations of casts from String to NSString

var swiftString: String = "string"

// CHECK-LABEL: sil [noinline] @_T021bridged_casts_folding29testForcedCastSwiftToNSStringSo0I0CyF
// CHECK-NOT: unconditional_checked
// CHECK: function_ref @{{.*}}_bridgeToObjectiveC
// CHECK: return
@inline(never)
public func testForcedCastSwiftToNSString() -> NSString {
  var o: NSString = forcedCast(swiftString)
  return o
}

// CHECK-LABEL: sil [noinline] @_T021bridged_casts_folding27testCondCastSwiftToNSStringSo0I0CSgyF
// CHECK-NOT: unconditional_checked
// CHECK: function_ref @{{.*}}_bridgeToObjectiveC
// CHECK: return
@inline(never)
public func testCondCastSwiftToNSString() -> NSString? {
  var o: NSString? = condCast(swiftString)
  return o
}


// Check optimizations of casts from Int to NSNumber

var swiftIntNumber: Int = 1

// CHECK-LABEL: sil [noinline] @_T021bridged_casts_folding32testForcedCastSwiftIntToNSNumberSo0J0CyF
// CHECK-NOT: unconditional_checked
// CHECK: function_ref @{{.*}}_bridgeToObjectiveC
// CHECK: return
@inline(never)
public func testForcedCastSwiftIntToNSNumber() -> NSNumber {
  var o: NSNumber = forcedCast(swiftIntNumber)
  return o
}

// CHECK-LABEL: sil [noinline] @_T021bridged_casts_folding30testCondCastSwiftIntToNSNumberSo0J0CSgyF
// CHECK-NOT: unconditional_checked
// CHECK: function_ref @{{.*}}_bridgeToObjectiveC
// CHECK: return
@inline(never)
public func testCondCastSwiftIntToNSNumber() -> NSNumber? {
  var o: NSNumber? = condCast(swiftIntNumber)
  return o
}

// Check optimizations of casts from Double to NSNumber

var swiftDoubleNumber: Double = 1.234

// CHECK-LABEL: sil [noinline] @_T021bridged_casts_folding35testForcedCastSwiftDoubleToNSNumberSo0J0CyF
// CHECK-NOT: unconditional_checked
// CHECK: function_ref @{{.*}}_bridgeToObjectiveC
// CHECK: return
@inline(never)
public func testForcedCastSwiftDoubleToNSNumber() -> NSNumber {
  var o: NSNumber = forcedCast(swiftDoubleNumber)
  return o
}

// CHECK-LABEL: sil [noinline] @_T021bridged_casts_folding33testCondCastSwiftDoubleToNSNumberSo0J0CSgyF
// CHECK-NOT: unconditional_checked
// CHECK: function_ref @{{.*}}_bridgeToObjectiveC
// CHECK: return
@inline(never)
public func testCondCastSwiftDoubleToNSNumber() -> NSNumber? {
  var o: NSNumber? = condCast(swiftDoubleNumber)
  return o
}


// Check optimization of casts from Swift Array to NSArray

var arrInt: [Int] = [1, 2, 3, 4]
var arrDouble: [Double] = [1.1, 2.2, 3.3, 4.4]
var arrString: [String] = ["One", "Two", "Three", "Four"]

// CHECK-LABEL: sil [noinline] @_T021bridged_casts_folding31testForcedCastSwiftToNSArrayIntSo0I0CyF
// CHECK-NOT: unconditional_checked
// CHECK: function_ref @{{.*}}_bridgeToObjectiveC
// CHECK: return
@inline(never)
public func testForcedCastSwiftToNSArrayInt() -> NSArray {
  var arr: NSArray = forcedCast(arrInt)
  return arr
}

// CHECK-LABEL: sil [noinline] @_T021bridged_casts_folding29testCondCastSwiftToNSArrayIntSo0I0CSgyF
// CHECK-NOT: unconditional_checked
// CHECK: function_ref @{{.*}}_bridgeToObjectiveC
// CHECK: return
@inline(never)
public func testCondCastSwiftToNSArrayInt() -> NSArray? {
  var arrOpt: NSArray? = condCast(arrInt)
  return arrOpt
}

// CHECK-LABEL: sil [noinline] @_T021bridged_casts_folding34testForcedCastSwiftToNSArrayDoubleSo0I0CyF
// CHECK-NOT: unconditional_checked
// CHECK: function_ref @{{.*}}_bridgeToObjectiveC
// CHECK: return
@inline(never)
public func testForcedCastSwiftToNSArrayDouble() -> NSArray {
  var arr: NSArray = forcedCast(arrDouble)
  return arr
}

// CHECK-LABEL: sil [noinline] @_T021bridged_casts_folding32testCondCastSwiftToNSArrayDoubleSo0I0CSgyF
// CHECK-NOT: unconditional_checked
// CHECK: function_ref @{{.*}}_bridgeToObjectiveC
// CHECK: return
@inline(never)
public func testCondCastSwiftToNSArrayDouble() -> NSArray? {
  var arrOpt: NSArray? = condCast(arrDouble)
  return arrOpt
}


// CHECK-LABEL: sil [noinline] @_T021bridged_casts_folding34testForcedCastSwiftToNSArrayStringSo0I0CyF
// CHECK-NOT: unconditional_checked
// CHECK: function_ref @{{.*}}_bridgeToObjectiveC
// CHECK: return
@inline(never)
public func testForcedCastSwiftToNSArrayString() -> NSArray {
  var arr: NSArray = forcedCast(arrString)
  return arr
}

// CHECK-LABEL: sil [noinline] @_T021bridged_casts_folding32testCondCastSwiftToNSArrayStringSo0I0CSgyF
// CHECK-NOT: unconditional_checked
// CHECK: function_ref @{{.*}}_bridgeToObjectiveC
// CHECK: return
@inline(never)
public func testCondCastSwiftToNSArrayString() -> NSArray? {
  var arrOpt: NSArray? = condCast(arrString)
  return arrOpt
}


// Check optimization of casts from Swift Dict to NSDict

var dictInt: [Int: Int] = [1:1, 2:2, 3:3, 4:4]
var dictDouble: [Double: Double] = [1.1 : 1.1, 2.2 : 2.2, 3.3 : 3.3, 4.4 : 4.4]
var dictString: [String: String] = ["One":"One", "Two":"Two", "Three":"Three", "Four":"Four"]

// CHECK-LABEL: sil [noinline] @_T021bridged_casts_folding30testForcedCastSwiftToNSDictIntSo12NSDictionaryCyF
// CHECK-NOT: unconditional_checked
// CHECK: function_ref @{{.*}}_bridgeToObjectiveC
// CHECK: return
@inline(never)
public func testForcedCastSwiftToNSDictInt() -> NSDictionary {
  var dict: NSDictionary = forcedCast(dictInt)
  return dict
}

// CHECK-LABEL: sil [noinline] @_T021bridged_casts_folding28testCondCastSwiftToNSDictIntSo12NSDictionaryCSgyF
// CHECK-NOT: unconditional_checked
// CHECK: function_ref @{{.*}}_bridgeToObjectiveC
// CHECK: return
@inline(never)
public func testCondCastSwiftToNSDictInt() -> NSDictionary? {
  var dictOpt: NSDictionary? = condCast(dictInt)
  return dictOpt
}

// CHECK-LABEL: sil [noinline] @_T021bridged_casts_folding33testForcedCastSwiftToNSDictDoubleSo12NSDictionaryCyF
// CHECK-NOT: unconditional_checked
// CHECK: function_ref @{{.*}}_bridgeToObjectiveC
// CHECK: return
@inline(never)
public func testForcedCastSwiftToNSDictDouble() -> NSDictionary {
  var dict: NSDictionary = forcedCast(dictDouble)
  return dict
}

// CHECK-LABEL: sil [noinline] @_T021bridged_casts_folding31testCondCastSwiftToNSDictDoubleSo12NSDictionaryCSgyF
// CHECK-NOT: unconditional_checked
// CHECK: function_ref @{{.*}}_bridgeToObjectiveC
// CHECK: return
@inline(never)
public func testCondCastSwiftToNSDictDouble() -> NSDictionary? {
  var dictOpt: NSDictionary? = condCast(dictDouble)
  return dictOpt
}


// CHECK-LABEL: sil [noinline] @_T021bridged_casts_folding33testForcedCastSwiftToNSDictStringSo12NSDictionaryCyF
// CHECK-NOT: unconditional_checked
// CHECK: function_ref @{{.*}}_bridgeToObjectiveC
// CHECK: return
@inline(never)
public func testForcedCastSwiftToNSDictString() -> NSDictionary {
  var dict: NSDictionary = forcedCast(dictString)
  return dict
}

// CHECK-LABEL: sil [noinline] @_T021bridged_casts_folding31testCondCastSwiftToNSDictStringSo12NSDictionaryCSgyF
// CHECK-NOT: unconditional_checked
// CHECK: function_ref @{{.*}}_bridgeToObjectiveC
// CHECK: return
@inline(never)
public func testCondCastSwiftToNSDictString() -> NSDictionary? {
  var dictOpt: NSDictionary? = condCast(dictString)
  return dictOpt
}


// Check optimization of casts from Swift Set to NSSet

var setInt: Set<Int> = [1, 2, 3, 4]
var setDouble: Set<Double> = [1.1, 2.2, 3.3, 4.4]
var setString: Set<String> = ["One", "Two", "Three", "Four"]

// CHECK-LABEL: sil [noinline] @_T021bridged_casts_folding29testForcedCastSwiftToNSSetIntSo0I0CyF
// CHECK-NOT: unconditional_checked
// CHECK: function_ref @{{.*}}_bridgeToObjectiveC
// CHECK: return
@inline(never)
public func testForcedCastSwiftToNSSetInt() -> NSSet {
  var set: NSSet = forcedCast(setInt)
  return set
}

// CHECK-LABEL: sil [noinline] @_T021bridged_casts_folding27testCondCastSwiftToNSSetIntSo0I0CSgyF
// CHECK-NOT: unconditional_checked
// CHECK: function_ref @{{.*}}_bridgeToObjectiveC
// CHECK: return
@inline(never)
public func testCondCastSwiftToNSSetInt() -> NSSet? {
  var setOpt: NSSet? = condCast(setInt)
  return setOpt
}

// CHECK-LABEL: sil [noinline] @_T021bridged_casts_folding32testForcedCastSwiftToNSSetDoubleSo0I0CyF
// CHECK-NOT: unconditional_checked
// CHECK: function_ref @{{.*}}_bridgeToObjectiveC
// CHECK: return
@inline(never)
public func testForcedCastSwiftToNSSetDouble() -> NSSet {
  var set: NSSet = forcedCast(setDouble)
  return set
}

// CHECK-LABEL: sil [noinline] @_T021bridged_casts_folding30testCondCastSwiftToNSSetDoubleSo0I0CSgyF
// CHECK-NOT: unconditional_checked
// CHECK: function_ref @{{.*}}_bridgeToObjectiveC
// CHECK: return
@inline(never)
public func testCondCastSwiftToNSSetDouble() -> NSSet? {
  var setOpt: NSSet? = condCast(setDouble)
  return setOpt
}


// CHECK-LABEL: sil [noinline] @_T021bridged_casts_folding32testForcedCastSwiftToNSSetStringSo0I0CyF
// CHECK-NOT: unconditional_checked
// CHECK: function_ref @{{.*}}_bridgeToObjectiveC
// CHECK: return
@inline(never)
public func testForcedCastSwiftToNSSetString() -> NSSet {
  var set: NSSet = forcedCast(setString)
  return set
}

// CHECK-LABEL: sil [noinline] @_T021bridged_casts_folding30testCondCastSwiftToNSSetStringSo0I0CSgyF
// CHECK-NOT: unconditional_checked
// CHECK: function_ref @{{.*}}_bridgeToObjectiveC
// CHECK: return
@inline(never)
public func testCondCastSwiftToNSSetString() -> NSSet? {
  var setOpt: NSSet? = condCast(setString)
  return setOpt
}

// Casts involving generics cannot be optimized.

// CHECK-LABEL: sil [noinline] @_T021bridged_casts_folding25testForcedCastFromGeneric{{[_0-9a-zA-Z]*}}F
// CHECK: unconditional_checked
// CHECK: return
@inline(never)
public func testForcedCastFromGeneric<T>(_ x: T) -> NSString {
  var set: NSString = x as! NSString
  return set
}

// CHECK-LABEL: sil [noinline] @_T021bridged_casts_folding23testForcedCastToGeneric{{[_0-9a-zA-Z]*}}F
// CHECK: unconditional_checked
// CHECK: return
@inline(never)
public func testForcedCastToGeneric<T>(_ x: T) -> T {
  var set: T = nsString as! T
  return set
}

// CHECK-LABEL: sil [noinline] @_T021bridged_casts_folding23testCondCastFromGeneric{{[_0-9a-zA-Z]*}}F
// CHECK: checked_cast_addr_br
// CHECK: return
@inline(never)
public func testCondCastFromGeneric<T>(_ x: T) -> NSString? {
  var setOpt: NSString? = x as? NSString
  return setOpt
}

// CHECK-LABEL: sil [noinline] @_T021bridged_casts_folding21testCondCastToGeneric{{[_0-9a-zA-Z]*}}F
// CHECK: checked_cast_addr_br
// CHECK: return
@inline(never)
public func testCondCastToGeneric<T>(_ x: T) -> T? {
  var setOpt: T? = nsString as? T
  return setOpt
}


// Run-time tests

//// ObjC -> Swift

// Arrays
print("NS to Swift arrays: Start")
print(testForcedCastNStoSwiftArrayInt())
print(testCondCastNStoSwiftArrayInt())

print(testForcedCastNStoSwiftArrayDouble())
print(testCondCastNStoSwiftArrayDouble())

print(testForcedCastNStoSwiftArrayString())
print(testCondCastNStoSwiftArrayString())
print("NS to Swift arrays: End")

// Dicts
print("NS to Swift dictionaries: Start")
print(testForcedCastNStoSwiftDictInt())
print(testCondCastNStoSwiftDictInt())

print(testForcedCastNStoSwiftDictDouble())
print(testCondCastNStoSwiftDictDouble())

print(testForcedCastNStoSwiftDictString())
print(testCondCastNStoSwiftDictString())
print(testCondCastNSDictStringtoSwiftDictInt())
// This line should crash at run-time
//print(testForcedCastNSDictStringtoSwiftDictInt())
print("NS to Swift dictionaries: End")

// Sets
print("NS to Swift sets: Start")
print(testForcedCastNStoSwiftSetInt())
print(testCondCastNStoSwiftSetInt())

print(testForcedCastNStoSwiftSetDouble())
print(testCondCastNStoSwiftSetDouble())

print(testForcedCastNStoSwiftSetString())
print(testCondCastNStoSwiftSetString())
print("NS to Swift sets: End")


// Basic types

print("NS to Swift basic types: Start")
print(testForcedCastNSNumberToSwiftInt())
print(testCondCastNSNumberToSwiftInt())

print(testForcedCastNSNumberToSwiftDouble())
print(testCondCastNSNumberToSwiftDouble())

print(testForcedCastNSIntNumberToSwiftDouble())
print(testCondCastNSIntNumberToSwiftDouble())

print(testForcedCastNStoSwiftString())
print(testCondCastNStoSwiftString())
print("NS to Swift basic types: End")

//// Swift -> ObjC

// Basic types

print("Swift to NS basic types: Start")
print(testForcedCastSwiftIntToNSNumber())
print(testCondCastSwiftIntToNSNumber())

print(testForcedCastSwiftDoubleToNSNumber())
print(testCondCastSwiftDoubleToNSNumber())

print(testForcedCastSwiftToNSString())
print(testCondCastSwiftToNSString())
print("Swift to NS basic types: End")

// Arrays
print("Swift to NS arrays: Start")

print(testForcedCastSwiftToNSArrayInt())
print(testCondCastSwiftToNSArrayInt())

print(testForcedCastSwiftToNSArrayDouble())
print(testCondCastSwiftToNSArrayDouble())

print(testForcedCastSwiftToNSArrayString())
print(testCondCastSwiftToNSArrayString())

print("Swift to NS arrays: End")


// Dicts
print("Swift to NS dictionaries: Start")

print(testForcedCastSwiftToNSDictInt())
print(testCondCastSwiftToNSDictInt())

print(testForcedCastSwiftToNSDictDouble())
print(testCondCastSwiftToNSDictDouble())

print(testForcedCastSwiftToNSDictString())
print(testCondCastSwiftToNSDictString())

print("Swift to NS dictionaries: End")

// Sets
print("Swift to NS sets: Start")

print(testForcedCastSwiftToNSSetInt())
print(testCondCastSwiftToNSSetInt())

print(testForcedCastSwiftToNSSetDouble())
print(testCondCastSwiftToNSSetDouble())

print(testForcedCastSwiftToNSSetString())
print(testCondCastSwiftToNSSetString())

print("Swift to NS sets: End")

// Check optimizations of casts from String to CFString

// CHECK-LABEL: sil [noinline] @_T021bridged_casts_folding29testForcedCastSwiftToCFStringSo0I0CyF
// CHECK-NOT: unconditional_checked
// CHECK: function_ref @{{.*}}_bridgeToObjectiveC
// CHECK: unchecked_ref_cast{{.*}}: $NSString to $CFString
// CHECK: end{{.*}}_T021bridged_casts_folding29testForcedCastSwiftToCFStringSo0I0CyF
@inline(never)
public func testForcedCastSwiftToCFString() -> CFString {
  let o: CFString = forcedCast(swiftString)
  return o
}

// CHECK-LABEL: sil [noinline] @_T021bridged_casts_folding27testCondCastSwiftToCFStringSo0I0CSgyF
// CHECK-NOT: unconditional_checked
// CHECK: function_ref @{{.*}}_bridgeToObjectiveC
// CHECK: unchecked_ref_cast{{.*}}: $NSString to $CFString
// CHECK: end{{.*}}_T021bridged_casts_folding27testCondCastSwiftToCFStringSo0I0CSgyF
@inline(never)
public func testCondCastSwiftToCFString() -> CFString? {
  let o: CFString? = condCast(swiftString)
  return o
}

// Check optimizations of casts from Int to CFNumber

// CHECK-LABEL: sil [noinline] @_T021bridged_casts_folding32testForcedCastSwiftIntToCFNumberSo0J0CyF
// CHECK-NOT: unconditional_checked
// CHECK: function_ref @{{.*}}_bridgeToObjectiveC
// CHECK: unchecked_ref_cast{{.*}}: $NSNumber to $CFNumber
// CHECK: end{{.*}}_T021bridged_casts_folding32testForcedCastSwiftIntToCFNumberSo0J0CyF
@inline(never)
public func testForcedCastSwiftIntToCFNumber() -> CFNumber {
  let o: CFNumber = forcedCast(swiftIntNumber)
  return o
}

// CHECK-LABEL: sil [noinline] @_T021bridged_casts_folding30testCondCastSwiftIntToCFNumberSo0J0CSgyF
// CHECK-NOT: unconditional_checked
// CHECK: function_ref @{{.*}}_bridgeToObjectiveC
// CHECK: unchecked_ref_cast{{.*}}: $NSNumber to $CFNumber
// CHECK: end{{.*}}_T021bridged_casts_folding30testCondCastSwiftIntToCFNumberSo0J0CSgyF
@inline(never)
public func testCondCastSwiftIntToCFNumber() -> CFNumber? {
  let o: CFNumber? = condCast(swiftIntNumber)
  return o
}

// Check optimization of casts from Swift Array to CFArray

// CHECK-LABEL: sil [noinline] @_T021bridged_casts_folding31testForcedCastSwiftToCFArrayIntSo0I0CyF
// CHECK-NOT: unconditional_checked
// CHECK: function_ref @{{.*}}_bridgeToObjectiveC
// CHECK: unchecked_ref_cast{{.*}}: $NSArray to $CFArray
// CHECK: end{{.*}}_T021bridged_casts_folding31testForcedCastSwiftToCFArrayIntSo0I0CyF
@inline(never)
public func testForcedCastSwiftToCFArrayInt() -> CFArray {
  let arr: CFArray = forcedCast(arrInt)
  return arr
}

// CHECK-LABEL: sil [noinline] @_T021bridged_casts_folding29testCondCastSwiftToCFArrayIntSo0I0CSgyF
// CHECK-NOT: unconditional_checked
// CHECK: function_ref @{{.*}}_bridgeToObjectiveC
// CHECK: unchecked_ref_cast{{.*}}: $NSArray to $CFArray
// CHECK: end{{.*}}_T021bridged_casts_folding29testCondCastSwiftToCFArrayIntSo0I0CSgyF
@inline(never)
public func testCondCastSwiftToCFArrayInt() -> CFArray? {
  let arrOpt: CFArray? = condCast(arrInt)
  return arrOpt
}

// Check optimization of casts from Swift Dict to CFDictionary

// CHECK-LABEL: sil [noinline] @_T021bridged_casts_folding30testForcedCastSwiftToCFDictIntSo12CFDictionaryCyF
// CHECK-NOT: unconditional_checked
// CHECK: function_ref @{{.*}}_bridgeToObjectiveC
// CHECK: unchecked_ref_cast{{.*}}: $NSDictionary to $CFDictionary
// CHECK: end{{.*}}_T021bridged_casts_folding30testForcedCastSwiftToCFDictIntSo12CFDictionaryCyF
@inline(never)
public func testForcedCastSwiftToCFDictInt() -> CFDictionary {
  let dict: CFDictionary = forcedCast(dictInt)
  return dict
}

// CHECK-LABEL: sil [noinline] @_T021bridged_casts_folding28testCondCastSwiftToCFDictIntSo12CFDictionaryCSgyF
// CHECK-NOT: unconditional_checked
// CHECK: function_ref @{{.*}}_bridgeToObjectiveC
// CHECK: unchecked_ref_cast{{.*}}: $NSDictionary to $CFDictionary
// CHECK: end{{.*}}_T021bridged_casts_folding28testCondCastSwiftToCFDictIntSo12CFDictionaryCSgyF
@inline(never)
public func testCondCastSwiftToCFDictInt() -> CFDictionary? {
  let dictOpt: CFDictionary? = condCast(dictInt)
  return dictOpt
}

// Check optimization of casts from Swift Set to CFSet

// CHECK-LABEL: sil [noinline] @_T021bridged_casts_folding29testForcedCastSwiftToCFSetIntSo0I0CyF
// CHECK-NOT: unconditional_checked
// CHECK: function_ref @{{.*}}_bridgeToObjectiveC
// CHECK: unchecked_ref_cast{{.*}}: $NSSet to $CFSet
// CHECK: end{{.*}}_T021bridged_casts_folding29testForcedCastSwiftToCFSetIntSo0I0CyF
@inline(never)
public func testForcedCastSwiftToCFSetInt() -> CFSet {
  let set: CFSet = forcedCast(setInt)
  return set
}

// CHECK-LABEL: sil [noinline] @_T021bridged_casts_folding27testCondCastSwiftToCFSetIntSo0I0CSgyF
// CHECK-NOT: unconditional_checked
// CHECK: function_ref @{{.*}}_bridgeToObjectiveC
// CHECK: unchecked_ref_cast{{.*}}: $NSSet to $CFSet
// CHECK: end{{.*}}_T021bridged_casts_folding27testCondCastSwiftToCFSetIntSo0I0CSgyF
@inline(never)
public func testCondCastSwiftToCFSetInt() -> CFSet? {
  let setOpt: CFSet? = condCast(setInt)
  return setOpt
}



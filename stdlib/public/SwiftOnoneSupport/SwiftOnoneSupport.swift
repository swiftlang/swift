//===----------------------------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2017 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
// Pre-specialization of some popular generic classes and functions.
//===----------------------------------------------------------------------===//
import Swift

// Explicitly force pre-specializations of some symbols of the library.
// This makes the set of pre-specialized symbols more stable and not so
// depending on the optimizer.
@_semantics("prespecialize.$sS2ayxGycfC")
@_semantics("prespecialize.$sSa034_makeUniqueAndReserveCapacityIfNotB0yyF")
@_semantics("prespecialize.$sSa10startIndexSivg")
@_semantics("prespecialize.$sSa11_getElement_20wasNativeTypeChecked22matchingSubscriptCheckxSi_Sbs16_DependenceTokenVtF")
@_semantics("prespecialize.$sSa12_getCapacitySiyF")
@_semantics("prespecialize.$sSa12arrayLiteralSayxGxd_tcfC")
@_semantics("prespecialize.$sSa15_checkSubscript_20wasNativeTypeCheckeds16_DependenceTokenVSi_SbtF")
@_semantics("prespecialize.$sSa15reserveCapacityyySiF")
@_semantics("prespecialize.$sSa16_copyToNewBuffer8oldCountySi_tF")
@_semantics("prespecialize.$sSa19_uninitializedCountSayxGSi_tcfC")
@_semantics("prespecialize.$sSa21_makeMutableAndUniqueyyF")
@_semantics("prespecialize.$sSa22_copyToContiguousArrays0cD0VyxGyF")
@_semantics("prespecialize.$sSa28_unsafeUninitializedCapacity16initializingWithSayxGSi_ySryxGz_SiztKXEtKcfC")
@_semantics("prespecialize.$sSa29_hoistableIsNativeTypeCheckedSbyF")
@_semantics("prespecialize.$sSa36_reserveCapacityAssumingUniqueBuffer8oldCountySi_tF")
@_semantics("prespecialize.$sSa37_appendElementAssumeUniqueAndCapacity_03newB0ySi_xntF")
@_semantics("prespecialize.$sSa5countSivg")
@_semantics("prespecialize.$sSa6appendyyxnF")
@_semantics("prespecialize.$sSa8capacitySivg")
@_semantics("prespecialize.$sSa8endIndexSivg")
@_semantics("prespecialize.$sSa9_getCountSiyF")
@_semantics("prespecialize.$sSa9formIndex5afterySiz_tF")
@_semantics("prespecialize.$sSa9formIndex6beforeySiz_tF")
@_semantics("prespecialize.$sSa9removeAll15keepingCapacityySb_tF")
@_semantics("prespecialize.$sSa9removeAll15keepingCapacityySb_tFfA_")
@_semantics("prespecialize.$sSa9removeAll15keepingCapacityySb_tF")
@_semantics("prespecialize.$sSa9repeating5countSayxGx_SitcfC")
@_semantics("prespecialize.$sSayxSiciM")
@_semantics("prespecialize.$sSayxSicig")
@_semantics("prespecialize.$sSayxSicir")
@_semantics("prespecialize.$ss12_ArrayBufferV013requestNativeB0s011_ContiguousaB0VyxGSgyF")
@_semantics("prespecialize.$ss12_ArrayBufferV027requestUniqueMutableBackingB015minimumCapacitys011_ContiguousaB0VyxGSgSi_tF")
@_semantics("prespecialize.$ss12_ArrayBufferV10_nonNatives06_CocoaA7WrapperVvg")
@_semantics("prespecialize.$ss12_ArrayBufferV10_typeCheckyySnySiGF")
@_semantics("prespecialize.$ss12_ArrayBufferV10startIndexSivg")
@_semantics("prespecialize.$ss12_ArrayBufferV13_copyContents8subRange12initializingSpyxGSnySiG_AFtF")
@_semantics("prespecialize.$ss12_ArrayBufferV19_getElementSlowPathyyXlSiF")
@_semantics("prespecialize.$ss12_ArrayBufferV19firstElementAddressSpyxGvg")
@_semantics("prespecialize.$ss12_ArrayBufferV20isUniquelyReferencedSbyF")
@_semantics("prespecialize.$ss12_ArrayBufferV37_checkInoutAndNativeTypeCheckedBounds_03wasfgH0ySi_SbtF")
@_semantics("prespecialize.$ss12_ArrayBufferV5countSivs")
@_semantics("prespecialize.$ss12_ArrayBufferV7_buffer19shiftedToStartIndexAByxGs011_ContiguousaB0VyxG_SitcfC")
@_semantics("prespecialize.$ss12_ArrayBufferV7_natives011_ContiguousaB0VyxGvg")
@_semantics("prespecialize.$ss12_ArrayBufferV8capacitySivg")
@_semantics("prespecialize.$ss12_ArrayBufferV8endIndexSivg")
@_semantics("prespecialize.$ss12_ArrayBufferV9_isNativeSbvg")
@_semantics("prespecialize.$ss12_ArrayBufferVyxSicig")
@_semantics("prespecialize.$ss12_ArrayBufferVyxSicir")
@_semantics("prespecialize.$ss22_ContiguousArrayBufferV10startIndexSivg")
@_semantics("prespecialize.$ss22_ContiguousArrayBufferV13_copyContents8subRange12initializingSpyxGSnySiG_AFtF")
@_semantics("prespecialize.$ss22_ContiguousArrayBufferV18_initStorageHeader5count8capacityySi_SitF")
@_semantics("prespecialize.$ss22_ContiguousArrayBufferV19firstElementAddressSpyxGvg")
@_semantics("prespecialize.$ss22_ContiguousArrayBufferV5countSivg")
@_semantics("prespecialize.$ss22_ContiguousArrayBufferV7_buffer19shiftedToStartIndexAByxGAE_SitcfC")
@_semantics("prespecialize.$ss22_ContiguousArrayBufferV8endIndexSivg")
@_semantics("prespecialize.$ss22_ContiguousArrayBufferVAByxGycfC")
@_semantics("prespecialize.$ss12_ArrayBufferVyxGSlsSl5index5after5IndexQzAG_tFTW")
@_semantics("prespecialize.$ss12_ArrayBufferVyxGSlsSl20_failEarlyRangeCheck_6boundsy5IndexQz_SnyAGGtFTW")
func prespecializationProxy<T>(_ x: T) {
}


internal enum _Prespecialize {
  // Create specializations for the arrays of most
  // popular builtin integer and floating point types.
  internal static func _specializeArrays() {
    func _createArrayUser<Element : Comparable>(_ sampleValue: Element) {
      // Initializers.
      let _: [Element] = [sampleValue]
      var a = [Element](repeating: sampleValue, count: 1)

      // Read array element
      _ = a[0]

      // Set array elements
      for j in 1..<a.count {
        a[0] = a[j]
        a[j-1] = a[j]
      }

      for i1 in 0..<a.count {
        for i2 in 0..<a.count {
          a[i1] = a[i2]
        }
      }

      a[0] = sampleValue

      // Get count and capacity
      _ = a.count + a.capacity

      // Iterate over array
      for e in a {
        print(e)
        print("Value: \(e)")
      }

      // Iterate in reverse
      for e in a.reversed() {
        print(e)
        print("Value: \(e)")
      }

      print(a)

      // Reserve capacity
      a.removeAll()
      a.reserveCapacity(100)

      // Sort array
      _ = a.sorted { (a: Element, b: Element) in a < b }
      a.sort { (a: Element, b: Element) in a < b }

      // force specialization of append.
      a.append(a[0])

      #if _runtime(_ObjC)
      // Explicitly specialize some private functions.
      // It's very unlikely that those functions are really referenced from
      // a Onone executable. But let's make sure all the swift 5.0 symbols
      // are also in upcoming versions of the OnoneSupport library.
      prespecializationProxy(sampleValue)
      #endif

      // force specialization of print<Element>
      print(sampleValue)
      print("Element:\(sampleValue)")
    }

    func _createArrayUserWithoutSorting<Element>(_ sampleValue: Element) {
      // Initializers.
      let _: [Element] = [sampleValue]
      var a = [Element](repeating: sampleValue, count: 1)

      // Read array element
      _ = a[0]

      // Set array elements
      for j in 0..<a.count {
        a[0] = a[j]
      }

      for i1 in 0..<a.count {
        for i2 in 0..<a.count {
          a[i1] = a[i2]
        }
      }

      a[0] = sampleValue

      // Get length and capacity
      _ = a.count + a.capacity

      // Iterate over array
      for e in a {
        print(e)
        print("Value: \(e)")
      }

      // Iterate in reverse
      for e in a.reversed() {
        print(e)
        print("Value: \(e)")
      }

      print(a)

      // Reserve capacity
      a.removeAll()
      a.reserveCapacity(100)


      // force specialization of append.
      a.append(a[0])

      #if _runtime(_ObjC)
      prespecializationProxy(sampleValue)
      #endif

      // force specialization of print<Element>
      print(sampleValue)
      print("Element:\(sampleValue)")
    }

    // Force pre-specialization of arrays with elements of different
    // integer types.
    _createArrayUser(1 as Int)
    _createArrayUser(1 as Int8)
    _createArrayUser(1 as Int16)
    _createArrayUser(1 as Int32)
    _createArrayUser(1 as Int64)
    _createArrayUser(1 as UInt)
    _createArrayUser(1 as UInt8)
    _createArrayUser(1 as UInt16)
    _createArrayUser(1 as UInt32)
    _createArrayUser(1 as UInt64)

    // Force pre-specialization of arrays with elements of different
    // floating point types.
    _createArrayUser(1.5 as Float)
    _createArrayUser(1.5 as Double)

    // Force pre-specialization of string arrays
    _createArrayUser("a" as String)

    // Force pre-specialization of arrays with elements of different
    // character and unicode scalar types.
    _createArrayUser("a" as Character)
    _createArrayUser("a" as Unicode.Scalar)
    _createArrayUserWithoutSorting("a".utf8)
    _createArrayUserWithoutSorting("a".utf16)
    _createArrayUserWithoutSorting("a".unicodeScalars)
    _createArrayUserWithoutSorting("a")
  }

  // Force pre-specialization of Range<Int>
  @discardableResult
  internal static func _specializeRanges() -> Int {
    let a = [Int](repeating: 1, count: 10)
    var count = 0
    // Specialize Range for integers
    for i in 0..<a.count {
      count += a[i]
    }
    // Specialize Range for integers
    for j in 0...a.count - 1{
      count += a[j]
    }
    return count
  }
}

// Mark with optimize(none) to make sure its not get
// rid of by dead function elimination. 
@_optimize(none)
internal func _swift_forcePrespecializations() {
  _Prespecialize._specializeArrays()
  _Prespecialize._specializeRanges()
}

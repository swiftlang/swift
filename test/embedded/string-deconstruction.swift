// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend %s -parse-as-library -enable-experimental-feature Embedded -c -o %t/main.o
// RUN: %target-clang %target-clang-resource-dir-opt %t/main.o -o %t/a.out -dead_strip -Xlinker %swift_obj_root/lib/swift/embedded/%module-target-triple/libswiftUnicodeDataTables.a
// RUN: %target-run %t/a.out | %FileCheck %s

// REQUIRES: swift_in_compiler
// REQUIRES: executable_test
// REQUIRES: OS=macosx || OS=wasip1
// REQUIRES: optimized_stdlib
// REQUIRES: swift_feature_Embedded

enum ExpectedDeconstruction {
  case scratchIfAvailable
  case interiorPointer
  case extraAllocation
}

func expectDeconstruct(
  _ str: String,
  _ expectDeconstruct: ExpectedDeconstruction,
  showFrame: Bool = true,
  file: String = #file, line: UInt = #line
) {
  let expectBytes = Array(str.utf8)

  _ = Array<UInt8>(unsafeUninitializedCapacity: 16) {
    buffer, initializedCount in
    // Deconstruct with a provided scratch space

    // WS == with scratch, N == nil
    let scratch = UnsafeMutableRawBufferPointer(buffer)
    let (ownerWS, ptrWS, lengthWS, usedScratchWS, allocatedMemoryWS)
      : (_ConvertedObject?, UnsafePointer<UInt8>, Int, Bool, Bool)
      = str._deconstructUTF8(scratch: scratch)
    let (ownerN, ptrN, lengthN, usedScratchN, allocatedMemoryN)
      : (_ConvertedObject?, UnsafePointer<UInt8>, Int, Bool, Bool)
      = str._deconstructUTF8(scratch: nil)

    let rawBytesWS = UnsafeRawBufferPointer(start: ptrWS, count: lengthWS)
    let rawBytesN = UnsafeRawBufferPointer(start: ptrN, count: lengthN)

    precondition(expectBytes.elementsEqual(rawBytesWS))
    precondition(rawBytesWS.elementsEqual(rawBytesN))

    switch expectDeconstruct {
    case .scratchIfAvailable:
      precondition(ownerWS == nil)
      precondition(ownerN != nil)

      precondition(UnsafeRawPointer(scratch.baseAddress) == rawBytesWS.baseAddress)
      precondition(UnsafeRawPointer(scratch.baseAddress) != rawBytesN.baseAddress)

      precondition(lengthWS < scratch.count)
      precondition(lengthN < scratch.count)

      precondition(usedScratchWS)
      precondition(!usedScratchN)

      precondition(!allocatedMemoryWS)
      precondition(allocatedMemoryN)

      case .interiorPointer:
      // TODO: owner == (immortal ? nil : StringObject.largeAddress)
      precondition(str.isContiguousUTF8)
      var copy = str
      copy.withUTF8 {
        precondition($0.baseAddress == ptrWS)
        precondition($0.baseAddress == ptrN)
        precondition($0.count == lengthWS)
        precondition($0.count == lengthN)
      }

      precondition(!usedScratchWS)
      precondition(!usedScratchN)
      precondition(!allocatedMemoryWS)
      precondition(!allocatedMemoryN)
    case .extraAllocation:
      precondition(!str.isContiguousUTF8)
      precondition(ownerWS != nil)
      precondition(ownerN != nil)
      precondition(!usedScratchWS)
      precondition(!usedScratchN)
      precondition(allocatedMemoryWS)
      precondition(allocatedMemoryN)
    }
  }
}

@inline(never)
func id<T>(_ a: T) -> T { a }

func testDeconstruct() {
  let smallASCII = "abcd"

#if _pointerBitWidth(_32) && os(watchOS)
  let smallUTF8 = "ジッパ"
#elseif _pointerBitWidth(_32)
  let smallUTF8 = "ジッ"
#else
  let smallUTF8 = "ジッパー"
#endif

  let large = "the quick fox jumped over the lazy brown dog"

  var largeMortal = large
  largeMortal.append(id("🧟‍♀️"))
  largeMortal.append(id(largeMortal.last!))

  expectDeconstruct(smallASCII, .scratchIfAvailable)
  expectDeconstruct(smallUTF8, .scratchIfAvailable)
  expectDeconstruct(large, .interiorPointer)
  expectDeconstruct(largeMortal, .interiorPointer)
}

@main
struct Main {
  static func main() {
    testDeconstruct()

    // CHECK: Success!
    print("Success!")
  }
}

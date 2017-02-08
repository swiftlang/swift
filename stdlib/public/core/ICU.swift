//===--- ICU.swift --------------------------------------------------------===//
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
import SwiftShims

internal typealias _UText = __swift_stdlib_UText
internal typealias _UChar = __swift_stdlib_UChar
internal typealias _UErrorCode = __swift_stdlib_UErrorCode

extension _UText {
  /// Invokes body, passing this UText's buffer area as a parameter
  mutating func withBuffer<R>(
    _ body: (UnsafeMutableBufferPointer<_UChar>)->R
  ) -> R {
    // Currently we are using the p, q, and r fields to get 12 UWords of
    // contiguous storage on 64-bit machines and 6 on 32-bit.  It's not much.
    return withUnsafeMutablePointer(to: &p) { bufferStart in
      let rawBufferStart = UnsafeRawPointer(bufferStart)
      let capacity = withUnsafeMutablePointer(to: &privP) {
        bufferLimit in
        (
          UnsafeRawPointer(bufferLimit).assumingMemoryBound(to: Int8.self)
          - rawBufferStart.assumingMemoryBound(to: Int8.self)
        ) / MemoryLayout<_UChar>.stride
      }
      let start = rawBufferStart.bindMemory(to: _UChar.self, capacity: capacity)
      let mutableStart = UnsafeMutablePointer(mutating: start)
      let buffer = UnsafeMutableBufferPointer(start: mutableStart, count: capacity)
      return body(buffer)
    }
  }
  
  mutating func validate() {
    let base = self.withBuffer { $0.baseAddress! }
    assert(chunkContents == base, "UText moved!")
  }

  mutating func setup() {
    chunkContents = self.withBuffer { UnsafePointer($0.baseAddress!) }
    pExtra = withUnsafeMutablePointer(to: &self) {
      UnsafeMutableRawPointer($0 + 1)
    }
  }
}

internal protocol _UTextable {
  func _nativeLength(_ uText: inout _UText) -> Int64
  func _access(_ u: inout _UText, _ nativeIndex: Int64, _ forward: Bool) -> Bool
  
  func _clone(
    _ dst: UnsafeMutablePointer<_UText>?, _ u: UnsafePointer<_UText>,
    _ deep: Bool, _ status: UnsafeMutablePointer<_UErrorCode>?
  ) -> UnsafeMutablePointer<_UText>
  
  func _extract(
    _ u: inout _UText,
    _ nativeStart: Int64, _ nativeLimit: Int64,
    _ destination: UnsafeMutableBufferPointer<_UChar>,
    _ error: UnsafeMutablePointer<_UErrorCode>?
  ) -> Int32
  
  func _mapOffsetToNative(_ u: UnsafePointer<_UText>) -> Int64
  func _mapNativeIndexToUTF16(_ u: UnsafePointer<_UText>, _ nativeIndex: Int64) -> Int32
}

extension _UTextable {
  internal func _withUText<R>(_ body: (UnsafeMutablePointer<_UText>)->R) -> R {

    var copy = self, scratch = 0 as _UChar

    return withUnsafePointer(to: &copy) { pSelf in
      withUnsafeMutablePointer(to: &scratch) { pScratch in 
        var vtable = __swift_stdlib_UTextFuncs(
          tableSize: Int32(MemoryLayout<__swift_stdlib_UTextFuncs>.stride),
          reserved1: 0, reserved2: 0, reserved3: 0,
          clone: { dst, u, deep, err in
            // debugLog("clone(\(dst!), \(u!), \(deep), \(String(describing: err)))")
            let _self = u[0].context.assumingMemoryBound(
              to: _UTextable.self)[0]
            return _self._clone(dst, u, deep != 0, err)
          },

          nativeLength: { u in
            // debugLog("nativeLength(\(u!))")
            let _self = u[0].context.assumingMemoryBound(
              to: _UTextable.self)[0]
            let r = _self._nativeLength(&u[0])
            // debugLog("# nativeLength: \(r)")
            return r
          },

          access: { u, nativeIndex, forward in
            // debugLog("access(\(u!), \(nativeIndex), \(forward))")
            let _self = u[0].context.assumingMemoryBound(
              to: _UTextable.self)[0]
            return _self._access(&u[0], nativeIndex, forward != 0) 
              ? 1 : 0
          },

          extract: { u, nativeStart, nativeLimit, dest, destCapacity, status in
            // debugLog("extract(\(u!), \(nativeStart), \(nativeLimit), \(dest!), \(destCapacity), \(String(describing: status)))")
            let _self = u[0].context.assumingMemoryBound(
              to: _UTextable.self)[0]

            let destination = UnsafeMutableBufferPointer(
              start: dest, count: destCapacity^)

            return _self._extract(
              &u[0], nativeStart, nativeLimit, destination, status)
          },

          replace: nil,
          copy: nil,

          mapOffsetToNative: { u in 
            // debugLog("mapOffsetToNative(\(u![0].chunkOffset))")
            let _self = u[0].context.assumingMemoryBound(
              to: _UTextable.self)[0]
            let r = _self._mapOffsetToNative(u)
            // debugLog("# mapOffsetToNative: \(r)")
            return r
          },

          mapNativeIndexToUTF16: { u, nativeIndex in 
            // debugLog("mapNativeIndexToUTF16(nativeIndex: \(nativeIndex), u: \(u![0]))")
            let _self = u[0].context.assumingMemoryBound(
              to: _UTextable.self)[0]
            let r = _self._mapNativeIndexToUTF16(u, nativeIndex)
            // debugLog("# mapNativeIndexToUTF16: \(r)")
            return r
          },
          close: nil,
          spare1: nil, spare2: nil, spare3: nil)

        let rawScratch = UnsafeMutableRawPointer(pScratch)
        
        var u = _UText(
          magic: UInt32(__swift_stdlib_UTEXT_MAGIC),
          flags: 0,
          providerProperties: 0,
          sizeOfStruct: Int32(MemoryLayout<_UText>.size),
          chunkNativeLimit: 0,
          extraSize: 0,
          nativeIndexingLimit: 0,
          chunkNativeStart: 0,
          chunkOffset: 0,
          chunkLength: 0,
          chunkContents: pScratch,
          pFuncs: &vtable,
          pExtra: rawScratch,
          context: UnsafeRawPointer(pSelf),
          p: rawScratch,
          q: rawScratch,
          r: rawScratch,
          privP: rawScratch,
          a: 0, b: 0, c: 0,
          privA: 0, privB: 0, privC: 0)
        u.setup()
        u.validate()
        return body(&u)
      }
    }
  }
}

extension _UErrorCode {
  var isFailure: Bool { return rawValue > __swift_stdlib_U_ZERO_ERROR.rawValue }
  var isWarning: Bool { return rawValue < __swift_stdlib_U_ZERO_ERROR.rawValue }
  var isSuccess: Bool { return rawValue <= __swift_stdlib_U_ZERO_ERROR.rawValue }
}

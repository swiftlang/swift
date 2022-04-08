//===--- AliasAnalysis.swift - the alias analysis -------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2021 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

import OptimizerBridging
import SIL

struct AliasAnalysis {
  let bridged: BridgedAliasAnalysis

  func mayRead(_ inst: Instruction, fromAddress: Value) -> Bool {
    switch AliasAnalysis_getMemBehavior(bridged, inst.bridged, fromAddress.bridged) {
      case MayReadBehavior, MayReadWriteBehavior, MayHaveSideEffectsBehavior:
        return true
      default:
        return false
    }
  }

  func mayWrite(_ inst: Instruction, toAddress: Value) -> Bool {
    switch AliasAnalysis_getMemBehavior(bridged, inst.bridged, toAddress.bridged) {
      case MayWriteBehavior, MayReadWriteBehavior, MayHaveSideEffectsBehavior:
        return true
      default:
        return false
    }
  }

  func mayReadOrWrite(_ inst: Instruction, address: Value) -> Bool {
    switch AliasAnalysis_getMemBehavior(bridged, inst.bridged, address.bridged) {
      case MayReadBehavior, MayWriteBehavior, MayReadWriteBehavior,
           MayHaveSideEffectsBehavior:
        return true
      default:
        return false
    }
  }

  /// Returns the correct path for address-alias functions.
  static func getPtrOrAddressPath(for value: Value) -> EscapeInfo.Path {
    let ty = value.type
    if ty.isAddress {
      // This is the regular case: the path selects any sub-fields of an address.
      return EscapeInfo.Path(.anyValueFields)
    }
    // Some optimizations use the address-alias APIs with non-address SIL values.
    // TODO: this is non-intuitive and we should eliminate those API uses.
    if ty.isClass {
    // If the value is a (non-address) reference it means: all addresses within the class instance.
      return EscapeInfo.Path(.anyValueFields).push(.anyClassField)
    }
    // Any other non-address value means: all addresses of any referenced class instances within the value.
    return EscapeInfo.Path(.anyValueFields).push(.anyClassField).push(.anyValueFields)
  }
  
  static func register() {
    AliasAnalysis_register(
      // initFn
      { (buffer: UnsafeMutableRawPointer, size: Int, ca: BridgedCalleeAnalysis) in
        precondition(MemoryLayout<EscapeInfo>.size <= size, "wrong EscapeInfo size")
        let ei = EscapeInfo(calleeAnalysis: CalleeAnalysis(bridged: ca))
        buffer.initializeMemory(as: EscapeInfo.self, repeating: ei, count: 1)
      },
      // destroyFn
      { (buffer: UnsafeMutableRawPointer, size: Int) in
        precondition(MemoryLayout<EscapeInfo>.size <= size, "wrong EscapeInfo size")
        buffer.assumingMemoryBound(to: EscapeInfo.self).deinitialize(count: 1)
      },
      // isAddrEscaping2InstFn
      { (buffer: UnsafeMutableRawPointer, bridgedVal: BridgedValue, bridgedInst: BridgedInstruction,
         readingOpsMask: UInt64, writingOpsMask: UInt64) -> BridgedMemoryBehavior in
        let eiPointer = buffer.assumingMemoryBound(to: EscapeInfo.self)
        let inst = bridgedInst.instruction
        let val = bridgedVal.value
        let path = AliasAnalysis.getPtrOrAddressPath(for: val)
        var isReading = false
        var isWriting = false
        if eiPointer.pointee.isEscaping(addressesOf: val, path: path,
            visitUse: { op, _, _ in
              let user = op.instruction
              if user == inst {
                let opIdx = op.index
                if opIdx >= 64 || (readingOpsMask & (UInt64(1) << opIdx)) != 0 {
                  isReading = true
                }
                if opIdx >= 64 || (writingOpsMask & (UInt64(1) << opIdx)) != 0 {
                  isWriting = true
                }
              }
              if user is ReturnInst {
                // Anything which is returned cannot escape to an instruction inside the function.
                return .ignore
              }
              return .continueWalking
            }) {
          return MayReadWriteBehavior
        }
        switch (isReading, isWriting) {
          case (false, false): return NoneBehavior
          case (true, false):  return MayReadBehavior
          case (false, true):  return MayWriteBehavior
          case (true, true):   return MayReadWriteBehavior
        }
      },
      // isObjEscaping2InstFn
      { (buffer: UnsafeMutableRawPointer, bridgedObj: BridgedValue, bridgedInst: BridgedInstruction,
         operandMask: UInt64) -> Int in
        let eiPointer = buffer.assumingMemoryBound(to: EscapeInfo.self)
        let inst = bridgedInst.instruction
        let obj = bridgedObj.value
        let path = EscapeInfo.Path(.anyValueFields)
        return eiPointer.pointee.isEscaping(object: obj, path: path,
          visitUse: { op, _, _ in
            let user = op.instruction
            if user == inst {
              let opIdx = op.index
              if opIdx >= 64 || (operandMask & (UInt64(1) << opIdx)) != 0 {
                return .markEscaping
              }
            }
            if user is ReturnInst {
              // Anything which is returned cannot escape to an instruction inside the function.
              return .ignore
            }
            return .continueWalking
          }) ? 1 : 0
      },
      // isAddrVisibleFromObj
      { (buffer: UnsafeMutableRawPointer, bridgedAddr: BridgedValue, bridgedObj: BridgedValue) -> Int in
        let eiPointer = buffer.assumingMemoryBound(to: EscapeInfo.self)
        let addr = bridgedAddr.value
        let obj = bridgedObj.value
      
        // Is the `addr` within all reachable objects/addresses, when start walking from `obj`?
        // Try both directions: 1. from addr -> obj
        return eiPointer.pointee.isEscaping(addressesOf: addr, path: EscapeInfo.Path(.anyValueFields),
            visitUse: { op, _, _ in
              if op.value == obj {
                return .markEscaping
              }
              if op.instruction is ReturnInst {
                // Anything which is returned cannot escape to an instruction inside the function.
                return .ignore
              }
              return .continueWalking
            }) ? 1 : 0
      },
      // canReferenceSameFieldFn
      { (buffer: UnsafeMutableRawPointer, bridgedLhs: BridgedValue, bridgedRhs: BridgedValue) -> Int in
        let eiPointer = buffer.assumingMemoryBound(to: EscapeInfo.self)
        let lhs = bridgedLhs.value
        let rhs = bridgedRhs.value
        return eiPointer.pointee.canReferenceSameField(
          lhs, path: AliasAnalysis.getPtrOrAddressPath(for: lhs),
          rhs, path: AliasAnalysis.getPtrOrAddressPath(for: rhs)) ? 1 : 0
      }
    )
  }
}

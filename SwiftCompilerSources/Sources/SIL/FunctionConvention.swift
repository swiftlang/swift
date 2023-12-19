//===--- FunctionConvention.swift - function conventions ------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2023 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

import SILBridging

/// SIL function convention based on the AST function type and SIL stage.
public struct FunctionConvention {
  let bridgedFunctionType: BridgedASTType
  let hasLoweredAddresses: Bool

  init(for bridgedFunctionType: BridgedASTType, in function: Function) {
    self.bridgedFunctionType = bridgedFunctionType
    self.hasLoweredAddresses = function.hasLoweredAddresses
  }

  struct Results : Collection {
    let bridged: BridgedResultInfoArray
    let hasLoweredAddresses: Bool

    public var startIndex: Int { 0 }

    public var endIndex: Int { bridged.count() }

    public func index(after index: Int) -> Int {
      return index + 1
    }

    public subscript(_ index: Int) -> ResultInfo {
      return ResultInfo(bridged: bridged.at(index),
        hasLoweredAddresses: hasLoweredAddresses)
    }
  }

  var results: Results {
    Results(bridged: bridgedFunctionType.SILFunctionType_getResults(),
      hasLoweredAddresses: hasLoweredAddresses)
  }

  /// Number of SIL arguments for indirect results, not including error results.
  var indirectSILResultCount: UInt {
    // TODO: Return packs directly in lowered-address mode
    return hasLoweredAddresses
    ? bridgedFunctionType.SILFunctionType_getNumIndirectFormalResults()
    : bridgedFunctionType.SILFunctionType_getNumPackResults()
  }

  var errorResult: ResultInfo? {
    guard bridgedFunctionType.SILFunctionType_hasErrorResult() else {
      return nil
    }
    return results[0]
  }

  /// The number of SIL error results passed as address-typed arguments.
  var indirectSILErrorResultCount: UInt {
    guard hasLoweredAddresses else { return 0 }
    return errorResult?.convention == .indirect ? 1 : 0
  }

  var parameterCount: UInt {
    bridgedFunctionType.SILFunctionType_getNumParameters()
  }

  /// The SIL argument index of the function type's first parameter.
  var firstParameterIndex: UInt {
    indirectSILResultCount + indirectSILErrorResultCount
  }

  // The SIL argument index of the 'self' paramter.
  var selfIndex: UInt? {
    guard bridgedFunctionType.SILFunctionType_hasSelfParam() else { return nil }
    return firstParameterIndex + parameterCount - 1
  }
}

/// A function result type and the rules for returning it in SIL.
struct ResultInfo {
  /// The unsubstituted parameter type that describes the abstract calling convention of the parameter.
  ///
  /// TODO: For most purposes, you probably want \c returnValueType.
  let interfaceType: BridgedASTType
  let convention: ResultConvention
  let hasLoweredAddresses: Bool

  /// Is this result returned indirectly in SIL? Most formally indirect results can be returned directly in SIL. This depends on the calling function.
  var isSILIndirect: Bool {
    switch convention {
    case .indirect:
      return hasLoweredAddresses || interfaceType.isOpenedExistentialWithError()
    case .pack:
      return true
    case .owned, .unowned, .unownedInnerPointer, .autoreleased:
      return false
    }
  }
}

public enum ResultConvention {
  /// This result is returned indirectly, i.e. by passing the address of an uninitialized object in memory.  The callee is responsible for leaving an initialized object at this address.  The callee may assume that the address does not alias any valid object.
  case indirect

  /// The caller is responsible for destroying this return value.  Its type is non-trivial.
  case owned

  /// The caller is not responsible for destroying this return value.  Its type may be trivial, or it may simply be offered unsafely.  It is valid at the instant of the return, but further operations may invalidate it.
  case unowned

  /// The caller is not responsible for destroying this return value.  The validity of the return value is dependent on the 'self' parameter, so it may be invalidated if that parameter is released.
  case unownedInnerPointer

  /// This value has been (or may have been) returned autoreleased.  The caller should make an effort to reclaim the autorelease.  The type must be a class or class existential type, and this must be the only return value.
  case autoreleased

  /// This value is a pack that is returned indirectly by passing a pack address (which may or may not be further indirected, depending on the pact type).  The callee is responsible for leaving an initialized object in each element of the pack.
  case pack

  /// Does this result convention require indirect storage? This reflects a FunctionType's conventions, as opposed to the SIL conventions that dictate SILValue types.
  public var isASTIndirect: Bool {
    switch self {
    case .indirect, .pack:
      return true
    default:
      return false
    }
  }
  public var isASTDirect: Bool {
    return !isASTIndirect
  }
}

// Bridging utilities

extension ResultInfo {
  init(bridged: BridgedResultInfo, hasLoweredAddresses: Bool) {
    self.interfaceType = BridgedASTType(type: bridged.type)
    self.convention = ResultConvention(bridged: bridged.convention)
    self.hasLoweredAddresses = hasLoweredAddresses
  }
}

extension ResultConvention {
  init(bridged: BridgedResultConvention) {
    switch bridged {
      case .Indirect:            self = .indirect
      case .Owned:               self = .owned
      case .Unowned:             self = .unowned
      case .UnownedInnerPointer: self = .unownedInnerPointer
      case .Autoreleased:        self = .autoreleased
      case .Pack:                self = .pack
      default:
        fatalError("unsupported result convention")
    }
  }
}

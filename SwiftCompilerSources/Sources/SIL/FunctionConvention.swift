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
///
/// Provides Results and Parameters collections. ArgumentConventions
/// maps these to SIL arguments.
public struct FunctionConvention : CustomStringConvertible {
  let bridgedFunctionType: BridgedASTType
  let hasLoweredAddresses: Bool

  init(for bridgedFunctionType: BridgedASTType, in function: Function) {
    self.bridgedFunctionType = bridgedFunctionType
    self.hasLoweredAddresses = function.hasLoweredAddresses
  }

  public struct Results : Collection {
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

  /// All results including the error.
  public var results: Results {
    Results(bridged: bridgedFunctionType.SILFunctionType_getResultsWithError(),
      hasLoweredAddresses: hasLoweredAddresses)
  }

  public var errorResult: ResultInfo? {
    return ResultInfo(
      bridged: bridgedFunctionType.SILFunctionType_getErrorResult(),
      hasLoweredAddresses: hasLoweredAddresses)
  }

  /// Number of indirect results including the error.
  /// This avoids quadratic lazy iteration on indirectResults.count.
  public var indirectSILResultCount: Int {
    // TODO: Return packs directly in lowered-address mode
    return hasLoweredAddresses
    ? bridgedFunctionType.SILFunctionType_getNumIndirectFormalResultsWithError()
    : bridgedFunctionType.SILFunctionType_getNumPackResults()
  }

  /// Indirect results including the error.
  public var indirectSILResults: LazyFilterSequence<Results> {
    hasLoweredAddresses
    ? results.lazy.filter { $0.isSILIndirect }
    : results.lazy.filter { $0.convention == .pack }
  }

  public struct Parameters : Collection {
    let bridged: BridgedParameterInfoArray
    let hasLoweredAddresses: Bool

    public var startIndex: Int { 0 }

    public var endIndex: Int { bridged.count() }

    public func index(after index: Int) -> Int {
      return index + 1
    }

    public subscript(_ index: Int) -> ParameterInfo {
      return ParameterInfo(bridged: bridged.at(index),
        hasLoweredAddresses: hasLoweredAddresses)
    }
  }

  public var parameters: Parameters {
    Parameters(bridged: bridgedFunctionType.SILFunctionType_getParameters(),
      hasLoweredAddresses: hasLoweredAddresses)
  }

  public var hasSelfParameter: Bool {
    bridgedFunctionType.SILFunctionType_hasSelfParam()
  }

  public var description: String {
    var str = String(taking: bridgedFunctionType.getDebugDescription())
    parameters.forEach { str += "\nparameter: " + $0.description }
    results.forEach { str += "\n   result: " + $0.description }
    str += (hasLoweredAddresses ? "\n[lowered_address]" : "\n[sil_opaque]")
    return str
  }
}

/// A function result type and the rules for returning it in SIL.
public struct ResultInfo : CustomStringConvertible {
  /// The unsubstituted parameter type that describes the abstract
  /// calling convention of the parameter.
  ///
  /// TODO: For most purposes, you probably want \c returnValueType.
  public let interfaceType: BridgedASTType
  public let convention: ResultConvention
  public let hasLoweredAddresses: Bool

  /// Is this result returned indirectly in SIL? Most formally
  /// indirect results can be returned directly in SIL. This depends
  /// on whether the calling function has lowered addresses.
  public var isSILIndirect: Bool {
    switch convention {
    case .indirect:
      return hasLoweredAddresses || interfaceType.isOpenedExistentialWithError()
    case .pack:
      return true
    case .owned, .unowned, .unownedInnerPointer, .autoreleased:
      return false
    }
  }

  public var description: String {
    convention.description + ": "
    + String(taking: interfaceType.getDebugDescription())
  }
}

public struct ParameterInfo : CustomStringConvertible {
  /// The parameter type that describes the abstract calling
  /// convention of the parameter.
  public let interfaceType: BridgedASTType
  public let convention: ArgumentConvention
  public let hasLoweredAddresses: Bool

  /// Is this parameter passed indirectly in SIL? Most formally
  /// indirect results can be passed directly in SIL. This depends on
  /// whether the calling function has lowered addresses.
  public var isSILIndirect: Bool {
    switch convention {
    case .indirectIn, .indirectInGuaranteed:
      return hasLoweredAddresses || interfaceType.isOpenedExistentialWithError()
    case .indirectInout, .indirectInoutAliasable:
      return true
    case .directOwned, .directUnowned, .directGuaranteed:
      return false
    case .packInout, .packOwned, .packGuaranteed:
      return true
    case .indirectOut, .packOut:
      fatalError("invalid parameter convention")
    }
  }

  public var description: String {
    convention.description + ": "
    + String(taking: interfaceType.getDebugDescription())
  }
}

public enum ResultConvention : CustomStringConvertible {
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

  public var description: String {
    switch self {
    case .indirect:
      return "indirect"
    case .owned:
      return "owned"
    case .unowned:
      return "unowned"
    case .unownedInnerPointer:
      return "unownedInnerPointer"
    case .autoreleased:
      return "autoreleased"
    case .pack:
      return "pack"
    }
  }
}

// Bridging utilities

extension ResultInfo {
  init(bridged: BridgedResultInfo, hasLoweredAddresses: Bool) {
    self.interfaceType = BridgedASTType(type: bridged.type)
    self.convention = ResultConvention(bridged: bridged.convention)
    self.hasLoweredAddresses = hasLoweredAddresses
  }
  init(bridged: OptionalBridgedResultInfo, hasLoweredAddresses: Bool) {
    self.interfaceType = BridgedASTType(type: bridged.type!)
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

extension ParameterInfo {
  init(bridged: BridgedParameterInfo, hasLoweredAddresses: Bool) {
    self.interfaceType = BridgedASTType(type: bridged.type)
    self.convention = bridged.convention.convention
    self.hasLoweredAddresses = hasLoweredAddresses
  }
}

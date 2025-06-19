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

import AST
import SILBridging

/// SIL function parameter and result conventions based on the AST
/// function type and SIL stage.
///
/// Provides Results and Parameters collections. This does not know
/// anything about FunctionArguments. Use ArgumentConventions instead to
/// maps FunctionArguments down to these conventions.
///
/// The underlying FunctionType must be contextual and expanded. SIL
/// has no use for interface types or unexpanded types.
public struct FunctionConvention : CustomStringConvertible {
  let functionType: CanonicalType
  let hasLoweredAddresses: Bool

  public init(for functionType: CanonicalType, in function: Function) {
    self.init(for: functionType, hasLoweredAddresses: function.hasLoweredAddresses)
  }

  public init(for functionType: CanonicalType, hasLoweredAddresses: Bool) {
    assert(!functionType.hasTypeParameter, "requires contextual type")
    self.functionType = functionType
    self.hasLoweredAddresses = hasLoweredAddresses
  }

  /// All results including the error.
  public var results: Results {
    Results(bridged: SILFunctionType_getResultsWithError(functionType.bridged),
      hasLoweredAddresses: hasLoweredAddresses)
  }

  public var errorResult: ResultInfo? {
    return ResultInfo(
      bridged: SILFunctionType_getErrorResult(functionType.bridged),
      hasLoweredAddresses: hasLoweredAddresses)
  }

  /// Number of indirect results including the error.
  /// This avoids quadratic lazy iteration on indirectResults.count.
  public var indirectSILResultCount: Int {
    // TODO: Return packs directly in lowered-address mode
    return hasLoweredAddresses
    ? SILFunctionType_getNumIndirectFormalResultsWithError(functionType.bridged)
    : SILFunctionType_getNumPackResults(functionType.bridged)
  }

  /// Returns the indirect result - including the error - at `index`.
  public func indirectSILResult(at index: Int) -> ResultInfo {
    let indirectResults = results.lazy.filter {
      hasLoweredAddresses ? $0.isSILIndirect : $0.convention == .pack
    }
    // Note that subscripting a LazyFilterCollection (with the base index, e.g. `Int`) does not work
    // as expected, because it returns the nth element of the base collection!
    // Therefore we need to implement the subscript "manually".
    return indirectResults.enumerated().first{ $0.offset == index }!.element
  }

  public var parameters: Parameters {
    Parameters(bridged: SILFunctionType_getParameters(functionType.bridged),
      hasLoweredAddresses: hasLoweredAddresses)
  }

  public var hasSelfParameter: Bool {
    SILFunctionType_hasSelfParam(functionType.bridged)
  }

  public var yields: Yields {
    Yields(bridged: SILFunctionType_getYields(functionType.bridged),
      hasLoweredAddresses: hasLoweredAddresses)
  }

  /// If the function result depends on any parameters, return a Collection of LifetimeDependenceConventions for the
  /// dependence source parameters.
  public var resultDependencies: LifetimeDependencies? {
    lifetimeDependencies(for: parameters.count)
  }

  /// If the parameter indexed by 'targetParameterIndex' is the target of any dependencies on other parameters, return a
  /// Collection of LifetimeDependenceConventions for the dependence source parameters.
  public func parameterDependencies(for targetParameterIndex: Int) -> LifetimeDependencies? {
    lifetimeDependencies(for: targetParameterIndex)
  }

  public func hasLifetimeDependencies() -> Bool {
    return SILFunctionType_getLifetimeDependencies(functionType.bridged).count() != 0
  }

  public var description: String {
    var str = functionType.description
    for paramIdx in 0..<parameters.count {
      str += "\nparameter: " + parameters[paramIdx].description
      if let deps = parameterDependencies(for: paramIdx) {
        str += "\n lifetime: \(deps)"
      }
    }
    results.forEach { str += "\n   result: " + $0.description }
    if let deps = resultDependencies {
      str += "\n lifetime: \(deps)"
    }
    return str
  }
}

/// A function result type and the rules for returning it in SIL.
public struct ResultInfo : CustomStringConvertible {
  /// The unsubstituted parameter type that describes the abstract
  /// calling convention of the parameter.
  ///
  /// TODO: For most purposes, you probably want \c returnValueType.
  public let type: BridgedASTType
  public let convention: ResultConvention
  public let hasLoweredAddresses: Bool

  /// Is this result returned indirectly in SIL? Most formally
  /// indirect results can be returned directly in SIL. This depends
  /// on whether the calling function has lowered addresses.
  public var isSILIndirect: Bool {
    switch convention {
    case .indirect:
      return hasLoweredAddresses || type.isExistentialArchetypeWithError()
    case .pack:
      return true
    case .owned, .unowned, .unownedInnerPointer, .autoreleased:
      return false
    }
  }

  public var description: String {
    convention.description + ": "
    + String(taking: type.getDebugDescription())
  }
}

extension FunctionConvention {
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
}

public struct ParameterInfo : CustomStringConvertible {
  /// The parameter type that describes the abstract calling
  /// convention of the parameter.
  public let type: CanonicalType
  public let convention: ArgumentConvention
  public let options: UInt8
  public let hasLoweredAddresses: Bool
  
  // Must be kept consistent with 'SILParameterInfo::Flag'
  public enum Flag : UInt8 {
    case notDifferentiable = 0x1
    case sending = 0x2
    case isolated = 0x4
    case implicitLeading = 0x8
    case const = 0x10
  };

  public init(type: CanonicalType, convention: ArgumentConvention, options: UInt8, hasLoweredAddresses: Bool) {
    self.type = type
    self.convention = convention
    self.options = options
    self.hasLoweredAddresses = hasLoweredAddresses
  }

  /// Is this parameter passed indirectly in SIL? Most formally
  /// indirect results can be passed directly in SIL (opaque values
  /// mode). This depends on whether the calling function has lowered
  /// addresses.
  public var isSILIndirect: Bool {
    switch convention {
    case .indirectIn, .indirectInGuaranteed:
      return hasLoweredAddresses || type.isExistentialArchetypeWithError
    case .indirectInout, .indirectInoutAliasable, .indirectInCXX:
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
    "\(convention): \(type)"
  }
  
  public func hasOption(_ flag: Flag) -> Bool {
    return options & flag.rawValue != 0
  }
}

extension FunctionConvention {
  public struct Parameters : RandomAccessCollection {
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
}

extension FunctionConvention {
  public struct Yields : Collection {
    let bridged: BridgedYieldInfoArray
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
}

public enum LifetimeDependenceConvention : CustomStringConvertible {
  case inherit
  case scope(addressable: Bool, addressableForDeps: Bool)

  public var isScoped: Bool {
    switch self {
    case .inherit:
      return false
    case .scope:
      return true
    }
  }

  public func isAddressable(for value: Value) -> Bool {
    switch self {
    case .inherit:
      return false
    case let .scope(addressable, addressableForDeps):
      return addressable || (addressableForDeps && value.type.isAddressableForDeps(in: value.parentFunction))
    }
  }

  public var description: String {
    switch self {
    case .inherit:
      return "inherit"
    case .scope:
      return "scope"
    }
  }
}

extension FunctionConvention {
  // 'targetIndex' is either the parameter index or parameters.count for the function result.
  private func lifetimeDependencies(for targetIndex: Int) -> LifetimeDependencies? {
    let bridgedDependenceInfoArray = SILFunctionType_getLifetimeDependencies(functionType.bridged)
    for infoIndex in 0..<bridgedDependenceInfoArray.count() {
      let bridgedDependenceInfo = bridgedDependenceInfoArray.at(infoIndex)
      if bridgedDependenceInfo.targetIndex == targetIndex {
        return LifetimeDependencies(bridged: bridgedDependenceInfo,
                                    parameterCount: parameters.count,
                                    hasSelfParameter: hasSelfParameter)
      }
    }
    return nil
  }

  /// Collection of LifetimeDependenceConvention? that parallels parameters.
  public struct LifetimeDependencies : Collection, CustomStringConvertible {
    let bridged: BridgedLifetimeDependenceInfo
    let paramCount: Int
    let hasSelfParam: Bool

    init(bridged: BridgedLifetimeDependenceInfo, parameterCount: Int,
         hasSelfParameter: Bool) {
      assert(!bridged.empty())
      self.bridged = bridged
      self.paramCount = parameterCount
      self.hasSelfParam = hasSelfParameter
    }

    public var startIndex: Int { 0 }

    public var endIndex: Int { paramCount }

    public func index(after index: Int) -> Int {
      return index + 1
    }

    public subscript(_ index: Int) -> LifetimeDependenceConvention? {
      let inherit = bridged.checkInherit(bridgedIndex(parameterIndex: index))
      let scope = bridged.checkScope(bridgedIndex(parameterIndex: index))
      if inherit {
        assert(!scope, "mutualy exclusive lifetime specifiers")
        return .inherit
      }
      if scope {
        let addressable = bridged.checkAddressable(bridgedIndex(parameterIndex: index))
        let addressableForDeps = bridged.checkConditionallyAddressable(bridgedIndex(parameterIndex: index))
        return .scope(addressable: addressable, addressableForDeps: addressableForDeps)
      }
      return nil
    }

    private func bridgedIndex(parameterIndex: Int) -> Int {
      return parameterIndex
    }

    public var description: String {
      String(taking: bridged.getDebugDescription()) +
        "\nparamCount: \(paramCount) self: \(hasSelfParam)"
    }
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
    self.type = BridgedASTType(type: bridged.type)
    self.convention = ResultConvention(bridged: bridged.convention)
    self.hasLoweredAddresses = hasLoweredAddresses
  }
  init(bridged: OptionalBridgedResultInfo, hasLoweredAddresses: Bool) {
    self.type = BridgedASTType(type: bridged.type!)
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
    self.type = CanonicalType(bridged: bridged.type)
    self.convention = bridged.convention.convention
    self.options = bridged.options
    self.hasLoweredAddresses = hasLoweredAddresses
  }

  public var _bridged: BridgedParameterInfo {
    BridgedParameterInfo(type.bridged, convention.bridged, options)
  }
}

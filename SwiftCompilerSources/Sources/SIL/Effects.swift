//===--- Effects.swift - Defines function effects -------------------------===//
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

import SILBridging

public struct SideEffect : CustomStringConvertible {
  public var read: Bool
  public var write: Bool
  public var retain: Bool
  public var release: Bool
  
  public init(read: Bool = false, write: Bool = false, retain: Bool = false, release: Bool = false) {
    self.read = read
    self.write = write
    self.retain = retain
    self.release = release
  }
  
  public mutating func setWorstEffects() {
    self.read = true
    self.write = true
    self.retain = true
    self.release = true
  }
  
  public mutating func merge(with other: SideEffect) {
    read = read || other.read
    write = write || other.write
    retain = retain || other.retain
    release = release || other.release
  }

  public var isPure: Bool { return !(read || write || retain || release)}
  
  public var description: String {
    if isPure { return "_" }
    else { return "\(read ? "r" : "")\(write ? "w" : "")\(retain ? "+" : "")\(release ? "-" : "")" }
  }
}

public struct MemoryEffect : CustomStringConvertible {
  public let read: Bool
  public let write: Bool

  public init(read: Bool, write: Bool) {
    self.read = read
    self.write = write
  }

  public var description: String {
    let desc = "\(read ? "r" : "")\(write ? "w" : "")"
    return "mem:\(desc.isEmpty ? "_" : desc)"
  }

  public func merge(with other: Self) -> Self {
    return Self(read: read || other.read, write: write || other.write)
  }
}

public struct OwnershipEffect : CustomStringConvertible {
  public let copy: Bool
  public let destroy: Bool

  public init(copy: Bool, destroy: Bool) {
    self.copy = copy
    self.destroy = destroy
  }

  public var description: String {
    let desc = "\(copy ? "c" : "")\(destroy ? "d" : "")"
    return "own:\(desc.isEmpty ? "_" : desc)"
  }

  public func merge(with other: Self) -> Self {
    return Self(copy: copy || other.copy, destroy: destroy || other.destroy)
  }
}

public func worstEffects(for convention: ArgumentConvention) -> (MemoryEffect, OwnershipEffect) {
  switch convention {
  case .indirectIn:
    return (MemoryEffect(read: true, write: true), OwnershipEffect(copy: true, destroy: true))
  case .indirectInConstant:
    return (MemoryEffect(read: true, write: false), OwnershipEffect(copy: true, destroy: /*TODO: doublecheck*/false))
  case .indirectInGuaranteed:
    return (MemoryEffect(read: true, write: false), OwnershipEffect(copy: true, destroy: false))
  case .indirectInout, .indirectInoutAliasable:
    return (MemoryEffect(read: true, write: true), OwnershipEffect(copy: true, destroy: false))
  case .indirectOut:
    return (MemoryEffect(read: false, write: true), OwnershipEffect(copy: false, destroy: false))
  case .directOwned:
    return (MemoryEffect(read: true, write: true), OwnershipEffect(copy: true, destroy: true))
  case .directUnowned:
    return (MemoryEffect(read: true, write: true), OwnershipEffect(copy: true, destroy: true))
  case .directGuaranteed:
    return (MemoryEffect(read: true, write: true), OwnershipEffect(copy: true, destroy: false))
  }
}

public struct GlobalEffects : CustomStringConvertible {
  public var sideEffect = SideEffect()

  public var traps: Bool = false
  public var allocates: Bool = false
  public var readsRC: Bool = false

  public init(isValid: Bool = false) {
    self.isValid = isValid
  }

  public mutating func merge(with other: GlobalEffects) {
    guard other.isValid else { return setWorstEffects() }
    sideEffect.merge(with: other.sideEffect)
    traps = traps || other.traps
    allocates = allocates || other.allocates
    readsRC = readsRC || other.readsRC
  }

  public mutating func setWorstEffects() {
    sideEffect.setWorstEffects()
    traps = true
    allocates = true
    readsRC = true
  }

  public var description: String {
    var res: String = "g:\(sideEffect.description)"
    if traps { res += ";trap" }
    if allocates { res += ";allocate" }
    if readsRC { res += ";readrc" }
    return res
  }

  private(set) var isValid: Bool
}

/// An effect on a function argument.
public struct ArgumentEffect : CustomStringConvertible, CustomReflectable {

  public typealias Path = SmallProjectionPath

  /// Selects which argument (or return value) and which projection of the argument
  /// or return value.
  ///
  /// For example, the selection `%0.s1` would select the second struct field of `arg` in
  ///    func foo(_ arg: Mystruct) { ... }
  public struct Selection : CustomStringConvertible {
    public enum ArgumentOrReturn : Equatable {
      case argument(Int)
      case returnValue
    }

    // Which argument or return value.
    public let value: ArgumentOrReturn
    
    /// Which projection(s) of the argument or return value.
    public let pathPattern: Path
 
    public init(_ value: ArgumentOrReturn, pathPattern: Path) {
      self.value = value
      self.pathPattern = pathPattern
    }

    public init(_ arg: Argument, pathPattern: Path) {
      self.init(.argument(arg.index), pathPattern: pathPattern)
    }

    /// Copy the selection by applying a delta on the argument index.
    ///
    /// This is used when copying argument effects for specialized functions where
    /// the indirect result is converted to a direct return value (in this case the
    /// `resultArgDelta` is -1).
    public init(copiedFrom src: Selection, resultArgDelta: Int) {
      switch (src.value, resultArgDelta) {
        case (let a, 0):
          self.value = a
        case (.returnValue, 1):
          self.value = .argument(0)
        case (.argument(0), -1):
          self.value = .returnValue
        case (.argument(let a), let d):
          self.value = .argument(a + d)
        default:
          fatalError()
      }
      self.pathPattern = src.pathPattern
    }
    
    public var argumentIndex: Int {
      switch value {
        case .argument(let argIdx): return argIdx
        default: fatalError("expected argument")
      }
    }
    
    public var description: String {
      let posStr: String
      switch value {
      case .argument(let argIdx):
        posStr = "%\(argIdx)"
      case .returnValue:
        posStr = "%r"
      }
      let pathStr = (pathPattern.isEmpty ? "" : ".\(pathPattern)")
      return "\(posStr)\(pathStr)"
    }
    
    public func matches(_ rhsValue: ArgumentOrReturn, _ rhsPath: Path) -> Bool {
      return value == rhsValue && rhsPath.matches(pattern: pathPattern)
    }
  }

  //----------------------------------------------------------------------//
  //                         Members of ArgumentEffect
  //----------------------------------------------------------------------//

  public enum Kind {
    /// The selected argument value does not escape.
    ///
    /// Syntax examples:
    ///    !%0       // argument 0 does not escape
    ///    !%0.**    // argument 0 and all transitively contained values do not escape
    ///
    case notEscaping
    
    /// The selected argument value escapes to the specified selection (= first payload).
    ///
    /// Syntax examples:
    ///    %0.s1 => %r   // field 2 of argument 0 exclusively escapes via return.
    ///    %0.s1 -> %1   // field 2 of argument 0 - and other values - escape to argument 1.
    ///
    /// The "exclusive" flag (= second payload) is true if only the selected argument escapes
    /// to the specified selection, but nothing else escapes to it.
    /// For example, "exclusive" is true for the following function:
    ///
    ///   @_effect(escaping c => return)
    ///   func exclusiveEscape(_ c: Class) -> Class { return c }
    ///
    /// but not in this case:
    ///
    ///   var global: Class
    ///
    ///   @_effect(escaping c -> return)
    ///   func notExclusiveEscape(_ c: Class) -> Class { return cond ? c : global }
    ///
    case escaping(Selection, Bool)  // to, exclusive
    
    case sideEffect(SideEffect)

    case memory(MemoryEffect)

    case ownership(OwnershipEffect)
  }

  /// To which argument (and projection) does this effect apply to?
  public let selectedArg: Selection
  
  /// The kind of effect.
  public let kind: Kind
  
  /// True, if this effect is derived in an optimization pass.
  /// False, if this effect is defined in the Swift source code.
  public let isDerived: Bool

  public init(_ kind: Kind, selectedArg: Selection, isDerived: Bool = true) {
    self.selectedArg = selectedArg
    self.kind = kind
    self.isDerived = isDerived
  }

    /// Copy the ArgumentEffect by applying a delta on the argument index.
    ///
    /// This is used when copying argument effects for specialized functions where
    /// the indirect result is converted to a direct return value (in this case the
    /// `resultArgDelta` is -1).
  init(copiedFrom srcEffect: ArgumentEffect, resultArgDelta: Int) {
  
    func copy(_ srcSelection: Selection) -> Selection {
      return Selection(copiedFrom: srcSelection, resultArgDelta: resultArgDelta)
    }
  
    self.selectedArg = copy(srcEffect.selectedArg)
    self.isDerived = srcEffect.isDerived
    switch srcEffect.kind {
      case .notEscaping:
        self.kind = .notEscaping
      case .escaping(let toSelectedArg, let exclusive):
        self.kind = .escaping(copy(toSelectedArg), exclusive)
      
      case .sideEffect(let eff):
        self.kind = .sideEffect(eff)
      default:
        fatalError("TODO")
    }
  }

  public var description: String {
    switch kind {
      case .notEscaping:
        return "!\(selectedArg)"
      case .escaping(let toSelectedArg, let exclusive):
        return "\(selectedArg) \(exclusive ? "=>" : "->") \(toSelectedArg)"
      case .sideEffect(let effect):
        let s = effect.description
        return "\(s)(\(selectedArg.description))"
      case .memory(let memoryEffect):
        return "\(memoryEffect)(\(selectedArg))"
      case .ownership(let ownershipEffect):
        return "\(ownershipEffect)(\(selectedArg))"
    }
  }

  public var customMirror: Mirror { Mirror(self, children: []) }
}

/// All argument effects for a function.
///
/// In future we might add non-argument-specific effects, too, like `readnone`, `readonly`.
public struct FunctionEffects : CustomStringConvertible, CustomReflectable {
  public var argumentEffects: [ArgumentEffect] = []
  public var globalEffects: GlobalEffects = GlobalEffects(isValid: false)
  
  public init() {}

  public init(copiedFrom src: FunctionEffects, resultArgDelta: Int) {
    self.argumentEffects = src.argumentEffects.map {
        ArgumentEffect(copiedFrom: $0, resultArgDelta: resultArgDelta)
      }
    self.globalEffects = src.globalEffects
  }

  public func canEscape(argumentIndex: Int, path: ArgumentEffect.Path, analyzeAddresses: Bool) -> Bool {
    return !argumentEffects.contains(where: {
      if case .notEscaping = $0.kind, $0.selectedArg.value == .argument(argumentIndex) {

        // Any address of a class property of an object, which is passed to the function, cannot
        // escape the function. Whereas a value stored in such a property could escape.
        let p = (analyzeAddresses ? path.popLastClassAndValuesFromTail() : path)

        if p.matches(pattern: $0.selectedArg.pathPattern) {
          return true
        }
      }
      return false
    })
  }


  public mutating func removeDerivedEffects() {
    argumentEffects = argumentEffects.filter { !$0.isDerived }
  }

  public var description: String {
    return "[" + argumentEffects.map { $0.description }.joined(separator: ", ") + "]"
  }
  
  public var customMirror: Mirror { Mirror(self, children: []) }
}

//===----------------------------------------------------------------------===//
//                               Parsing
//===----------------------------------------------------------------------===//

extension StringParser {

  mutating func parseEffectFromSource(for function: Function,
                            params: Dictionary<String, Int>) throws -> ArgumentEffect {
    if consume("notEscaping") {
      let selectedArg = try parseSelectionFromSource(for: function, params: params)
      return ArgumentEffect(.notEscaping, selectedArg: selectedArg, isDerived: false)
    }
    if consume("escaping") {
      let from = try parseSelectionFromSource(for: function, params: params)
      let exclusive = try parseEscapingArrow()
      let to = try parseSelectionFromSource(for: function, params: params, acceptReturn: true)
      return ArgumentEffect(.escaping(to, exclusive), selectedArg: from, isDerived: false)
    }
    try throwError("unknown effect")
  }

  mutating func parseGlobalEffect() throws -> GlobalEffects {
    var result = GlobalEffects(isValid: true)
    result.sideEffect = try parseSideEffectFlags()
    if consume(";trap") { result.traps = true }
    if consume(";allocate") { result.allocates = true }
    if consume(";readrc") { result.readsRC = true }
    return result
  }

  mutating func parseSideEffectFlags() throws -> SideEffect {
    // try parsing a pure effect
    if consume("_") { return SideEffect() }
    
    // try parsing an impure effect
    var result = SideEffect()
    if consume("r") { result.read = true }
    if consume("w") { result.write = true }
    if consume("+") { result.retain = true }
    if consume("-") { result.release = true }

    if result.isPure {
      try throwError("Unknown sideEffect flags")
    }

    return result
  }

  mutating func parseEffectFromSIL(for function: Function, effectFlags: Int) throws -> ArgumentEffect {
    if isEscape(effectFlags: effectFlags) {
      if consume("!") {
        let selectedArg = try parseSelectionFromSIL(for: function)
        return ArgumentEffect(.notEscaping, selectedArg: selectedArg, isDerived: isDerived(effectFlags: effectFlags))
      }
      let from = try parseSelectionFromSIL(for: function)
      let exclusive = try parseEscapingArrow()
      let to = try parseSelectionFromSIL(for: function, acceptReturn: true)
      return ArgumentEffect(.escaping(to, exclusive), selectedArg: from, isDerived: isDerived(effectFlags: effectFlags))
    } else if isSideEffect(effectFlags: effectFlags) {
      let sideEffect = try parseSideEffectFlags()
      if !consume("(") { try throwError("Missing selection for argument effect") }
      let selection = try parseSelectionFromSIL(for: function)
      if !consume(")") { try throwError("Expected ')' but unexpected token found. Malformed selection?") }
      return ArgumentEffect(.sideEffect(sideEffect), selectedArg: selection, isDerived: isDerived(effectFlags: effectFlags))
    } else {
      try throwError("Unknown effect flags")
    }
  }

  private mutating func parseEscapingArrow() throws -> Bool {
    if consume("=>") { return true }
    if consume("->") { return false }
    try throwError("expected '=>' or '->'")
  }


  mutating func parseSelectionFromSource(for function: Function,
                  params: Dictionary<String, Int>,
                  acceptReturn: Bool = false) throws -> ArgumentEffect.Selection {
    let value: ArgumentEffect.Selection.ArgumentOrReturn

    if consume("self") {
      if !function.hasSelfArgument {
        try throwError("function does not have a self argument")
      }
      value = .argument(function.selfArgumentIndex)
    } else if consume("return") {
      if !acceptReturn {
        try throwError("return not allowed here")
      }
      if function.numIndirectResultArguments > 0 {
        if function.numIndirectResultArguments != 1 {
          try throwError("multi-value returns not supported yet")
        }
        value = .argument(0)
      } else {
        value = .returnValue
      }
    } else if let name = consumeIdentifier() {
      guard let argIdx = params[name] else {
        try throwError("parameter not found")
      }
      value = .argument(argIdx + function.numIndirectResultArguments)
    } else {
      try throwError("parameter name or return expected")
    }

    let valueType: Type
    switch value {
    case .argument(let argIdx):
      valueType = function.argumentTypes[argIdx]
    case .returnValue:
      valueType = function.resultType
    }

    if consume(".") {
      let path = try parseProjectionPathFromSource(for: function, type: valueType)
      return ArgumentEffect.Selection(value, pathPattern: path)
    }
    if !valueType.isClass {
      switch value {
      case .argument:
        try throwError("the argument is not a class - add 'anyValueFields'")
      case .returnValue:
        try throwError("the return value is not a class - add 'anyValueFields'")
      }
    }
    return ArgumentEffect.Selection(value, pathPattern: ArgumentEffect.Path())
  }

  mutating func parseSelectionFromSIL(for function: Function,
                acceptReturn: Bool = false) throws -> ArgumentEffect.Selection {
    let value: ArgumentEffect.Selection.ArgumentOrReturn

    if consume("%r") {
      if !acceptReturn {
        try throwError("return not allowed here")
      }
      value = .returnValue
    } else if consume("%") {
      guard let argIdx = consumeInt() else {
        try throwError("expected argument index")
      }
      value = .argument(argIdx)
    } else {
      try throwError("expected parameter or return")
    }

    if consume(".") {
      return try ArgumentEffect.Selection(value, pathPattern: parseProjectionPathFromSIL())
    }
    return ArgumentEffect.Selection(value, pathPattern: ArgumentEffect.Path())
  }
}


func isDerived(effectFlags: Int) -> Bool {
  return (effectFlags & Int(EffectsFlag_Derived)) == Int(EffectsFlag_Derived)
}

func isEscape(effectFlags: Int) -> Bool {
  return (effectFlags & Int(EffectsFlag_Escape)) == Int(EffectsFlag_Escape)
}

func isSideEffect(effectFlags: Int) -> Bool {
  return (effectFlags & Int(EffectsFlag_SideEffect)) == Int(EffectsFlag_SideEffect)
}

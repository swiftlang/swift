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
    }
  }

  public var description: String {
    switch kind {
      case .notEscaping:
        return "!\(selectedArg)"
      case .escaping(let toSelectedArg, let exclusive):
        return "\(selectedArg) \(exclusive ? "=>" : "->") \(toSelectedArg)"
    }
  }

  public var customMirror: Mirror { Mirror(self, children: []) }
}

/// All argument effects for a function.
///
/// In future we might add non-argument-specific effects, too, like `readnone`, `readonly`.
public struct FunctionEffects : CustomStringConvertible, CustomReflectable {
  public var argumentEffects: [ArgumentEffect] = []
  
  public init() {}

  public init(copiedFrom src: FunctionEffects, resultArgDelta: Int) {
    self.argumentEffects = src.argumentEffects.map {
        ArgumentEffect(copiedFrom: $0, resultArgDelta: resultArgDelta)
      }
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

  mutating func parseEffectFromSIL(for function: Function, isDerived: Bool) throws -> ArgumentEffect {
    if consume("!") {
      let selectedArg = try parseSelectionFromSIL(for: function)
      return ArgumentEffect(.notEscaping, selectedArg: selectedArg, isDerived: isDerived)
    }
    let from = try parseSelectionFromSIL(for: function)
    let exclusive = try parseEscapingArrow()
    let to = try parseSelectionFromSIL(for: function, acceptReturn: true)
    return ArgumentEffect(.escaping(to, exclusive), selectedArg: from, isDerived: isDerived)
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

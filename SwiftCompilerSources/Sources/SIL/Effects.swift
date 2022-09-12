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

  public enum Kind {
    /// The argument value does not escape.
    ///
    /// Syntax examples:
    ///    [%0: noescape]      // argument 0 does not escape
    ///    [%0: noescape **]   // argument 0 and all transitively contained values do not escape
    ///
    case notEscaping
    
    /// The argument value escapes to the function return value.
    ///
    /// Syntax examples:
    ///    [%0: escape s1 => %r]   // field 2 of argument 0 exclusively escapes via return.
    ///    [%0: escape s1 -> %r]   // field 2 of argument 0 - and other values - escape via return
    ///
    /// The "exclusive" flag (= second payload) is true if only the argument escapes,
    /// but nothing else escapes to the return value.
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
    case escapingToReturn(Path, Bool)         // toPath, exclusive

    /// Like `escapingToReturn`, but the argument escapes to another argument.
    ///
    /// Example: The argument effects of
    ///   func argToArgEscape(_ r: inout Class, _ c: Class) { r = c }
    ///
    /// would be
    ///    [%1: escape => %0]   // Argument 1 escapes to argument 0
    ///
    case escapingToArgument(Int, Path, Bool)  // toArgumentIndex, toPath, exclusive
  }

  /// To which argument does this effect apply to?
  public let argumentIndex: Int
  
  /// To which projection(s) of the argument does this effect apply to?
  public let pathPattern: Path
  
  /// The kind of effect.
  public let kind: Kind
  
  /// True, if this effect is derived in an optimization pass.
  /// False, if this effect is defined in the Swift source code.
  public let isDerived: Bool

  public init(_ kind: Kind, argumentIndex: Int, pathPattern: Path, isDerived: Bool = true) {
    self.argumentIndex = argumentIndex
    self.pathPattern = pathPattern
    self.kind = kind
    self.isDerived = isDerived
  }

  /// Copy the ArgumentEffect by applying a delta on the argument index.
  ///
  /// This is used when copying argument effects for specialized functions where
  /// the indirect result is converted to a direct return value (in this case the
  /// `resultArgDelta` is -1).
  init?(copiedFrom srcEffect: ArgumentEffect, resultArgDelta: Int) {
    if srcEffect.argumentIndex + resultArgDelta < 0 {
      return nil
    }
    self.argumentIndex = srcEffect.argumentIndex + resultArgDelta
    self.pathPattern = srcEffect.pathPattern
    self.isDerived = srcEffect.isDerived

    switch srcEffect.kind {
    case .notEscaping:
      self.kind = .notEscaping
    case .escapingToReturn(let toPath, let exclusive):
      if resultArgDelta > 0 {
        if resultArgDelta != 1 {
          return nil
        }
        self.kind = .escapingToArgument(0, toPath, exclusive)
      } else {
        self.kind = .escapingToReturn(toPath, exclusive)
      }
    case .escapingToArgument(let toArgIdx, let toPath, let exclusive):
      let resultingToArgIdx = toArgIdx + resultArgDelta
      if resultingToArgIdx < 0 {
        if resultingToArgIdx != -1 {
          return nil
        }
        self.kind = .escapingToReturn(toPath, exclusive)
      } else {
        self.kind = .escapingToArgument(resultingToArgIdx, toPath, exclusive)
      }
    }
  }

  public func matches(_ rhsArgIdx: Int, _ rhsPath: Path) -> Bool {
    return argumentIndex == rhsArgIdx && rhsPath.matches(pattern: pathPattern)
  }

  public var headerDescription: String {
    "%\(argumentIndex)\(isDerived ? "" : "!"): "
  }

  public var bodyDescription: String {
    let patternStr = pathPattern.isEmpty ? "" : " \(pathPattern)"
    switch kind {
      case .notEscaping:
        return "noescape\(patternStr)"
      case .escapingToReturn(let toPath, let exclusive):
        let toPathStr = (toPath.isEmpty ? "" : ".\(toPath)")
        return "escape\(patternStr) \(exclusive ? "=>" : "->") %r\(toPathStr)"
      case .escapingToArgument(let toArgIdx, let toPath, let exclusive):
        let toPathStr = (toPath.isEmpty ? "" : ".\(toPath)")
        return "escape\(patternStr) \(exclusive ? "=>" : "->") %\(toArgIdx)\(toPathStr)"
    }
  }

  public var description: String {
    headerDescription + bodyDescription
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
    self.argumentEffects = src.argumentEffects.compactMap {
        ArgumentEffect(copiedFrom: $0, resultArgDelta: resultArgDelta)
      }
  }

  public func canEscape(argumentIndex: Int, path: ArgumentEffect.Path, analyzeAddresses: Bool) -> Bool {
    return !argumentEffects.contains(where: {
      if case .notEscaping = $0.kind, $0.argumentIndex == argumentIndex {

        // Any address of a class property of an object, which is passed to the function, cannot
        // escape the function. Whereas a value stored in such a property could escape.
        let p = (analyzeAddresses ? path.popLastClassAndValuesFromTail() : path)

        if p.matches(pattern: $0.pathPattern) {
          return true
        }
      }
      return false
    })
  }


  public mutating func removeDerivedEffects() {
    argumentEffects = argumentEffects.filter { !$0.isDerived }
  }

  public var argumentEffectsDescription: String {
    var currentArgIdx = -1
    var currentIsDerived = false
    var result = ""
    for effect in argumentEffects {
      if effect.argumentIndex != currentArgIdx ||  effect.isDerived != currentIsDerived {
        if currentArgIdx >= 0 { result += "]\n" }
        result += "[\(effect.headerDescription)"
        currentArgIdx = effect.argumentIndex
        currentIsDerived = effect.isDerived
      } else {
        result += ", "
      }
      result += effect.bodyDescription
    }
    if currentArgIdx >= 0 { result += "]\n" }
    return result
  }

  public var description: String {
    return argumentEffectsDescription
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
      let argIdx = try parseArgumentIndexFromSource(for: function, params: params)
      let path = try parsePathPatternFromSource(for: function, type: function.argumentTypes[argIdx])
      return ArgumentEffect(.notEscaping, argumentIndex: argIdx, pathPattern: path, isDerived: false)
    }
    if consume("escaping") {
      let fromArgIdx = try parseArgumentIndexFromSource(for: function, params: params)
      let fromPath = try parsePathPatternFromSource(for: function, type: function.argumentTypes[fromArgIdx])
      let exclusive = try parseEscapingArrow()
      
      if consume("return") {
        if function.numIndirectResultArguments > 0 {
          if function.numIndirectResultArguments != 1 {
            try throwError("multi-value returns not supported yet")
          }
          let toPath = try parsePathPatternFromSource(for: function, type: function.argumentTypes[0])
          return ArgumentEffect(.escapingToArgument(0, toPath, exclusive),
                                argumentIndex: fromArgIdx, pathPattern: fromPath, isDerived: false)
        }
        let toPath = try parsePathPatternFromSource(for: function, type: function.resultType)
        return ArgumentEffect(.escapingToReturn(toPath, exclusive),
                              argumentIndex: fromArgIdx, pathPattern: fromPath, isDerived: false)
      }
      let toArgIdx = try parseArgumentIndexFromSource(for: function, params: params)
      let toPath = try parsePathPatternFromSource(for: function, type: function.argumentTypes[toArgIdx])
      return ArgumentEffect(.escapingToArgument(toArgIdx, toPath, exclusive),
                            argumentIndex: fromArgIdx, pathPattern: fromPath, isDerived: false)
    }
    try throwError("unknown effect")
  }

  mutating func parseArgumentIndexFromSource(for function: Function,
                                             params: Dictionary<String, Int>) throws -> Int {
    if consume("self") {
      if !function.hasSelfArgument {
        try throwError("function does not have a self argument")
      }
      return function.selfArgumentIndex
    }
    if let name = consumeIdentifier() {
      guard let idx = params[name] else {
        try throwError("parameter not found")
      }
      return idx + function.numIndirectResultArguments
    }
    try throwError("parameter name expected")
  }
  
  mutating func parsePathPatternFromSource(for function: Function, type: Type) throws -> ArgumentEffect.Path {
    if consume(".") {
      return try parseProjectionPathFromSource(for: function, type: type)
    }
    if !type.isClass {
      try throwError("the value is not a class - add 'anyValueFields'")
    }
    return ArgumentEffect.Path()
  }

  mutating func parseEffectsFromSIL(to effects: inout FunctionEffects) throws {
    let argumentIndex = try parseArgumentIndexFromSIL()
    let isDerived = !consume("!")
    if !consume(":") {
      try throwError("expected ':'")
    }
    repeat {
      let effect = try parseEffectFromSIL(argumentIndex: argumentIndex, isDerived: isDerived)
      effects.argumentEffects.append(effect)
    } while consume(",")
  }

  mutating func parseEffectFromSIL(argumentIndex: Int, isDerived: Bool) throws -> ArgumentEffect {
    if consume("noescape") {
      let path = try parseProjectionPathFromSIL()
      return ArgumentEffect(.notEscaping, argumentIndex: argumentIndex, pathPattern: path, isDerived: isDerived)
    }
    if consume("escape") {
      let fromPath = try parseProjectionPathFromSIL()
      let exclusive = try parseEscapingArrow()
      if consume("%r") {
        let toPath = consume(".") ? try parseProjectionPathFromSIL() : ArgumentEffect.Path()
        return ArgumentEffect(.escapingToReturn(toPath, exclusive),
                              argumentIndex: argumentIndex, pathPattern: fromPath, isDerived: isDerived)
      }
      let toArgIdx = try parseArgumentIndexFromSIL()
      let toPath = consume(".") ? try parseProjectionPathFromSIL() : ArgumentEffect.Path()
      return ArgumentEffect(.escapingToArgument(toArgIdx, toPath, exclusive),
                            argumentIndex: argumentIndex, pathPattern: fromPath, isDerived: isDerived)
    }
    try throwError("unknown effect")
  }

  mutating func parseArgumentIndexFromSIL() throws -> Int {
    if consume("%") {
      if let argIdx = consumeInt() {
        return argIdx
      }
      try throwError("expected argument index")
    }
    try throwError("expected parameter")
  }

  private mutating func parseEscapingArrow() throws -> Bool {
    if consume("=>") { return true }
    if consume("->") { return false }
    try throwError("expected '=>' or '->'")
  }

}

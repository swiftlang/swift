//===--- SimplifyThinToThickFunction.swift --------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2026 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
import SIL

extension ThinToThickFunctionInst : Simplifiable, SILCombineSimplifiable {
  /// Keep convert_function after thickening to expose a function_ref callee to
  /// closure specialization. Closure specialization can look through conversions
  /// of a thick closure, but needs thin_to_thick_function to directly reference
  /// the callee.
  ///
  ///   %a = convert_function %f
  ///   %b = thin_to_thick_function %a
  /// ->
  ///   %a = convert_function %f   // deleted later if dead after this transformation (no uses except %b left)
  ///   %c = thin_to_thick_function %f
  ///   %d = convert_function %c
  ///
  /// Uses of %b are replaced with %d. Other uses of %a are unchanged.
  func simplify(_ context: SimplifyContext) {
    guard let conversion = callee as? ConvertFunctionInst else {
      return
    }

    let thickType = conversion.fromFunction.type
      .getFunctionType(withRepresentation: type.functionTypeRepresentation)
      .getFunctionType(withNoEscape: type.isNoEscapeFunction)
      .getFunctionType(withCalleeConvention: type.calleeConvention)
      // A conversion can't form a noncopyable function.
      .getFunctionType(withExecutionSemantics: type.executionSemantics)

    let builder = Builder(before: self, context)
    let newThinToThick = builder.createThinToThickFunction(thinFunction: conversion.fromFunction,
                                                           resultType: thickType)
    // The conversion doesn't change the thickened type, so it isn't needed.
    if thickType == type {
      replace(with: newThinToThick, context)
      return
    }
    let newConversion = builder.createConvertFunction(originalFunction: newThinToThick,
                                                      resultType: type,
                                                      kind: conversion.conversionKind)
    replace(with: newConversion, context)
  }
}

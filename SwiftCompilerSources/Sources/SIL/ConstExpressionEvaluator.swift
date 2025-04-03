//===--- ConstExpressionEvaluator.swift - ---------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2025 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

import AST
import SILBridging

public struct ConstExpressionEvaluator {
  var bridged: BridgedConstExprFunctionState

  public init(bridged: BridgedConstExprFunctionState) { self.bridged = bridged }
  public init() {
    self.bridged = BridgedConstExprFunctionState.create()
  }
  
  mutating public func isConstantValue(_ value: Value) -> Bool {
    return bridged.isConstantValue(value.bridged)
  }
  
  /// TODO: once we have move-only types, make this a real deinit.
  mutating public func deinitialize() {
    bridged.deinitialize()
  }
}

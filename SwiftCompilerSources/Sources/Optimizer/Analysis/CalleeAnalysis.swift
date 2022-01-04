//===--- CalleeAnalysis.swift - the callee analysis -----------------------===//
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

public struct CalleeAnalysis {
  let bridged: BridgedCalleeAnalysis

  public func getCallees(callee: Value) -> FunctionArray {
    return FunctionArray(bridged: CalleeAnalysis_getCallees(bridged, callee.bridged))
  }
}

public struct FunctionArray : RandomAccessCollection, CustomReflectable {
  fileprivate let bridged: BridgedCalleeList
  
  public var startIndex: Int { 0 }
  public var endIndex: Int { BridgedFunctionArray_size(bridged) }

  public subscript(_ index: Int) -> Function {
    return BridgedFunctionArray_get(bridged, index).function
  }

  public var allCalleesKnown: Bool { bridged.incomplete == 0 }

  public var customMirror: Mirror {
    let c: [Mirror.Child] = map { (label: nil, value: $0.name) }
    return Mirror(self, children: c)
  }
}

//===--- AliasInfoDumper.swift --------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2024 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

import SIL

/// Prints the memory behavior of relevant instructions in relation to address values of the function.
let aliasInfoDumper = FunctionPass(name: "dump-alias-info") {
  (function: Function, context: FunctionPassContext) in

  let aliasAnalysis = context.aliasAnalysis

  print("@\(function.name)")

  let values = function.allValues

  var pair = 0
  for (index1, value1) in values.enumerated() {
    for (index2, value2) in values.enumerated() {
      if index2 >= index1 {
        let result = aliasAnalysis.mayAlias(value1, value2)
        precondition(result == aliasAnalysis.mayAlias(value2, value1), "alias analysis not symmetric")

        print("PAIR #\(pair).")
        print("  \(value1)")
        print("  \(value2)")
        if result {
          print("  MayAlias")
        } else if !value1.uses.isEmpty && !value2.uses.isEmpty {
          print("  NoAlias")
        } else {
          print("  noalias?")
        }

        pair += 1
      }
    }
  }
}

private extension Function {
  var allValues: [Value] {
    var values: [Value] = []
    for block in blocks {
      values.append(contentsOf: block.arguments.map { $0 })
      for inst in block.instructions {
        values.append(contentsOf: inst.results)
      }
    }
    return values
  }
}

//===--- SimplifyUncheckedAddrCast.swift ----------------------------------===//
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

import SIL

extension UncheckedAddrCastInst : OnoneSimplifiable, SILCombineSimplifiable {

  func simplify(_ context: SimplifyContext) {
    // ```
    //   %1 = unchecked_addr_cast %0 : $*T to $*T
    // ```
    // ->
    //   replace %1 with %0
    //
    if optimizeSameTypeCast(context) {
      return
    }

    // ```
    //   %1 = unchecked_addr_cast %0 : $*U to $*V
    //   %2 = unchecked_addr_cast %1 : $*V to $*T
    // ```
    // ->
    // ```
    //   %2 = unchecked_addr_cast %0: $*U to $*T
    // ```
    if optimizeDoubleCast(context) {
      return
    }

    // ```
    //   %1 = unchecked_addr_cast %0 : $*Builtin.FixedArray<N, Element> to $*Element
    // ```
    // ->
    // ```
    //   %1 = vector_base_addr %0 : $*Builtin.FixedArray<N, Element>
    // ```
    _ = optimizeVectorBaseCast(context)
  }
}

private extension UncheckedAddrCastInst {
  func optimizeSameTypeCast(_ context: SimplifyContext) -> Bool {
    if fromAddress.type == type {
      self.replace(with: fromAddress, context)
      return true
    }
    return false
  }

  func optimizeDoubleCast(_ context: SimplifyContext) -> Bool {
    if let firstCast = fromAddress as? UncheckedAddrCastInst {
      let builder = Builder(before: self, context)
      let newCast = builder.createUncheckedAddrCast(from: firstCast.fromAddress, to: type)
      self.replace(with: newCast, context)
      return true
    }
    return false
  }

  func optimizeVectorBaseCast(_ context: SimplifyContext) -> Bool {
    if fromAddress.type.isBuiltinFixedArray,
       fromAddress.type.builtinFixedArrayElementType(in: parentFunction, maximallyAbstracted: true).addressType == type
    {
      let builder = Builder(before: self, context)
      let vectorBase = builder.createVectorBaseAddr(vector: fromAddress)
      self.replace(with: vectorBase, context)
      return true
    }
    return false
  }
}

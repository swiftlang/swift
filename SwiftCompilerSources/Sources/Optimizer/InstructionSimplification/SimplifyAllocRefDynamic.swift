//===--- SimplifyAllocRefDynamic.swift ------------------------------------===//
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

import SIL

extension AllocRefDynamicInst : OnoneSimplifiable {
  func simplify(_ context: SimplifyContext) {
    /// Optimize alloc_ref_dynamic of a known type to alloc_ref:
    ///
    ///   %3 = metatype SubClass.Type
    ///   %4 = upcast %3 : SubClass.Type to BaseClass.Type
    ///   %6 = alloc_ref_dynamic [...] %4 : BaseClass.Type, $BaseClass
    ///   %8 = (... some use of ...) %6 : $BaseClass
    /// ->
    ///   %6 = alloc_ref [...] $SubClass
    ///   %7 = upcast %6 : $SubClass to $BaseClass
    ///   %8 = (... some use of ...) %7 : $BaseClass

    let type: Type
    let emitUpcast: Bool
    if let metatypeInst = metatypeOperand.value as? MetatypeInst {
      type = metatypeInst.type.loweredInstanceTypeOfMetatype(in: parentFunction)
      emitUpcast = false
    } else if let upcastInst = metatypeOperand.value as? UpcastInst,
        let metatypeInst = upcastInst.operands[0].value as? MetatypeInst {
      type = metatypeInst.type.loweredInstanceTypeOfMetatype(in: parentFunction)
      emitUpcast = true
    } else {
      return
    }

    let builder = Builder(before: self, context)
    let newAlloc = builder.createAllocRef(type, isObjC: self.isObjC, canAllocOnStack: self.canAllocOnStack, isBare: false,
      tailAllocatedTypes: self.tailAllocatedTypes, tailAllocatedCounts: Array(self.tailAllocatedCounts.values))
    
    let result: Value
    if emitUpcast {
      result = builder.createUpcast(from: newAlloc, to: self.type)
    } else {
      result = newAlloc
    }
    uses.replaceAll(with: result, context)
    context.erase(instruction: self)
  }
}

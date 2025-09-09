//===--- SimplifyMisc.swift -----------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2024 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

import SIL

extension TypeValueInst: OnoneSimplifiable, SILCombineSimplifiable {
  func simplify(_ context: SimplifyContext) {
    // If our parameter is not known statically, then bail.
    guard let value = value else {
      return
    }

    // Note: Right now, value generics only support 'Int'. If we ever expand the
    // scope of types supported, then this should change.
    let fieldType = type.getNominalFields(in: parentFunction)![0]

    let builder = Builder(before: self, context)
    let intLiteral = builder.createIntegerLiteral(value, type: fieldType)
    let structInst = builder.createStruct(type: type, elements: [intLiteral])
    uses.replaceAll(with: structInst, context)
    context.erase(instruction: self)
  }
}


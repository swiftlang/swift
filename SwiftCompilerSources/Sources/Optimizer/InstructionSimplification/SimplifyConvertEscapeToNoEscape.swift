//===--- SimplifyConvertEscapeToNoEscape.swift ----------------------------===//
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

extension ConvertEscapeToNoEscapeInst : OnoneSimplifiable {
  func simplify(_ context: SimplifyContext) {
    tryCombineWithThinToThickOperand(context)
  }
}

private extension ConvertEscapeToNoEscapeInst {

  /// Combine with a thin_to_thick_function operand:
  ///
  ///   %2 = thin_to_thick_function %1 to $() -> ()
  ///   %3 = convert_escape_to_noescape %2 : $() -> () to $@noescape () -> ()
  /// ->
  ///   %3 = thin_to_thick_function %1 to $@noescape () -> ()

  func tryCombineWithThinToThickOperand(_ context: SimplifyContext) {
    if let thinToThick = fromFunction as? ThinToThickFunctionInst {
      let builder = Builder(before: self, context)
      let noEscapeFnType = thinToThick.type.getFunctionType(withNoEscape: true)
      let newThinToThick = builder.createThinToThickFunction(thinFunction: thinToThick.operand.value,
                                                             resultType: noEscapeFnType)
      uses.replaceAll(with: newThinToThick, context)
      context.erase(instruction: self)
    }
  }
}

//===--- PassRegistration.swift - Register optimization passes -------------===//
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
/// TODO: We aren't specialising variadic generics (or are we?), we're optimising the specializations.
/// Rename the pass accordingly
///
/// We can perform this optimisation iff all dynamic_pack_index instructions have a constant argument (probably integer_literal)
///
///
/// We specialise a called function iff:
/// * It hasn't been specialised already
/// * The application
let variadicGenericSpecialization = FunctionPass(name: "variadic-generic-specialization") {
  (function: Function, context: FunctionPassContext) in
}

private func isConcretePack(arg: FunctionArgument) -> Bool {
  switch arg.convention {
  case .packGuaranteed, .packOut, .packInout, .packOwned:
    return true;
  default:
    return false;
  }
}

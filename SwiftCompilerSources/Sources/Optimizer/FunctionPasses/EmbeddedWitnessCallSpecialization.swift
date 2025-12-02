//===--- EmbeddedWitnessCallSpecialization.swift ---------------------------==//
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

/// Changes the function representation of directly called witness methods in Embedded Swift.
///
/// If a function with `witness_method` convention is directly called, the function is specialized
/// by changing the convention to `method` and the call is replaced by a call to the specialized
/// function:
///
/// ```
///   %1 = function_ref @callee : $@convention(witness_method: P) (@guaranteed C) -> ()
///   %2 = apply %1(%0) : $@convention(witness_method: P) (@guaranteed C) -> ()
/// ...
/// sil [ossa] @callee : $@convention(witness_method: P) (@guaranteed C) -> () {
///   ...
/// }
/// ```
/// ->
/// ```
///   %1 = function_ref @$e6calleeTfr9 : $@convention(method) (@guaranteed C) -> ()
///   %2 = apply %1(%0) : $@convention(method) (@guaranteed C) -> ()
/// ...
/// // specialized callee
/// sil shared [ossa] @$e6calleeTfr9 : $@convention(method) (@guaranteed C) -> () {
///   ...
/// }
/// ```
///
/// This is needed in Embedded Swift because the `witness_method` convention requires passing the
/// witness table to the callee. However, the witness table is not necessarily available.
/// A witness table is only generated if an existential value of a protocol is created.
///
/// This is a rare situation because only witness thunks have `witness_method` convention and those
/// thunks are created as "transparent" functions, which means they are always inlined (after de-
/// virtualization of a witness method call). However, inlining - even of transparent functions -
/// can fail for some reasons.
///
let embeddedWitnessCallSpecialization = FunctionPass(name: "embedded-witness-call-specialization") {
  (function: Function, context: FunctionPassContext) in

  guard context.options.enableEmbeddedSwift,
        !function.isGeneric
  else {
    return
  }
  for inst in function.instructions {
    if let apply = inst as? FullApplySite {
      specializeDirectWitnessMethodCall(apply: apply, context)
    }
  }
}

private func specializeDirectWitnessMethodCall(apply: FullApplySite, _ context: FunctionPassContext) {
  guard apply.callee.type.functionTypeRepresentation == .witnessMethod,
        let callee = apply.referencedFunction,
        callee.isDefinition
  else {
    return
  }

  let specializedFunctionName = context.mangle(withChangedRepresentation: callee)

  let specializedFunction: Function

  if let existingSpecializedFunction = context.lookupFunction(name: specializedFunctionName) {
    specializedFunction = existingSpecializedFunction

  } else {
    specializedFunction = context.createSpecializedFunctionDeclaration(
      from: callee, withName: specializedFunctionName,
      withParams: Array(callee.convention.parameters),
      withRepresentation: .method)

    context.buildSpecializedFunction(
      specializedFunction: specializedFunction,
      buildFn: { (specializedFunction, specializedContext) in
        cloneFunction(from: callee, toEmpty: specializedFunction, specializedContext)
      })

    context.notifyNewFunction(function: specializedFunction, derivedFrom: callee)
  }

  apply.replace(withCallTo: specializedFunction, arguments: Array(apply.arguments), context)
}

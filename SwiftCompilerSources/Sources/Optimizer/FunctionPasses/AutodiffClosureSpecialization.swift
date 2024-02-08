//===--- ExperimentalClosureSpecialization.swift ---------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2023 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===-----------------------------------------------------------------------===//

/// AutoDiff Closure Specialization
/// ----------------------
/// This optimization performs closure specialization tailored for the patterns seen
/// in Swift Autodiff. In principle, the optimization does the same thing as the existing
/// closure specialization pass. However, it is tailored to the patterns of Swift Autodiff.
/// 
/// The compiler performs reverse-mode differentiation on functions marked with `@differentiable(reverse)`.
/// In doing so, it generates corresponding VJP and Pullback functions, which perform the 
/// forward and reverse pass respectively. The Pullbacks are essentially a chain of closures, 
/// where the closure-contexts are used as an implicit "tape". 
/// 
/// The code patterns that this optimization targets, look similar to the one below:
/// ``` swift
/// @differentiable(reverse)
/// func foo(_ x: Float) -> Float { 
///     return sin(x)
/// }
/// 
/// //============== Before optimization ==============//
/// sil @pb_foo: $(Float, (Float) -> Float) -> Float { 
/// bb0(%0: $Float, %1: $(Float) -> Float):
///     %2 = apply %1(%0): $Float
///     return %2: $Float
/// }
/// 
/// sil @vjp_foo: $(Float) -> (Float, (Float) -> Float) { 
/// bb0(%0: $Float):
///     %1 = apply @sin(%0): $Float
///     %2 = partial_apply @pb_sin(%0): $(Float) -> Float
///     %pb_foo = partial_apply @pb_foo(%2): $(Float, (Float) -> Float) -> Float
///     return (%y3, %pb_foo)
/// }
/// 
/// //============== After optimization ==============//
/// sil @specialized_pb_foo: $(Float, Float) -> Float { 
/// bb0(%0: $Float, %1: $Float):
///     %2 = partial_apply @pb_sin(%1): $(Float) -> Float
///     %3 = apply %2(%0): $Float
///     return %3: $Float
/// }
/// 
/// sil @vjp_foo: $(Float) -> (Float, (Float) -> Float) { 
/// bb0(%0: $Float):
///     %1 = apply @sin(%0): $Float
///     %pb_foo = partial_apply @specialized_pb_foo(%0): $(Float, Float) -> Float
///     return (%y3, %pb_foo)
/// }
/// ``` 
///
/// Similar complications, as in the existing closure-specialization optimization, exist
/// when populating the specialized Pullback and rewriting the apply instruction in the 
/// VJP. The details of the complications and our work arounds are listed below 
/// (copied over from the existing closure-specialization pass):
///
/// 1. If we support the specialization of closures with multiple user callsites
///    that can be specialized, we need to ensure that any captured values have
///    their reference counts adjusted properly. This implies for every
///    specialized call site, we insert an additional retain for each captured
///    argument with reference semantics. We will pass them in as extra @owned
///    to the specialized function. This @owned will be consumed by the "copy"
///    partial apply that is in the specialized function. Now the partial apply
///    will own those ref counts. This is unapplicable to thin_to_thick_function
///    since they do not have any captured args.
///
/// 2. If the closure was passed in @owned vs if the closure was passed in
///    @guaranteed. If the original closure was passed in @owned, then we know
///    that there is a balancing release for the new "copy" partial apply. But
///    since the original partial apply no longer will have that corresponding
///    -1, we need to insert a release for the old partial apply. We do this
///    right after the old call site where the original partial apply was
///    called. This ensures we do not shrink the lifetime of the old partial
///    apply. In the case where the old partial_apply was passed in at +0, we
///    know that the old partial_apply does not need to have any ref count
///    adjustments. On the other hand, the new "copy" partial apply in the
///    specialized function now needs to be balanced lest we leak. Thus we
///    insert a release right before any exit from the function. This ensures
///    that the release occurs in the epilog after any retains associated with
///    @owned return values.
///
/// 3. In !useLoweredAddresses mode, we do not support specialization of closures
///    with arguments passed using any indirect calling conventions besides
///    @inout and @inout_aliasable.  This is a temporary limitation that goes
///    away with sil-opaque-values.

import SIL

private let verbose = true

private func log(_ message: @autoclosure () -> String) {
  if verbose {
    print("### \(message())")
  }
}

// =========== Entry point into the pass =========== //
let autodiffClosureSpecialization = FunctionPass(name: "autodiff-closure-specialize") {
    (function: Function, context: FunctionPassContext) in

    // 1\ Don't optimize functions that are marked with the opt.never attribute
    // 2\ If F is an external declaration, there is nothing to specialize
    // 3\ Specialize
    // 4\ Eliminate dead closures
    // 5\ Invalidate everything using `invalidateAnalysis` since we delete calls 
    //    as well as add new calls and branches
    
    // var stackAllocDemangler = StackAllocatedDemangler().asDemangler()
    // let demangler = Demangler()
    // demangler.providePreallocatedMemory(parent: &stackAllocDemangler)
    // demangler.demangleSymbol(mangledName: function.name.string)
}

// =========== AutodiffClosureSpecializer ========== //
struct AutodiffClosureSpecializer: ClosureSpecializer {
    typealias PropagatedClosure = SingleValueInstruction

    func gatherCallSites(_ caller: Function) -> Stack<CallSiteDescriptor> {
        fatalError("Not implemented")
    }

    func specialize(_ caller: Function) -> Stack<PropagatedClosure> {
        fatalError("Not implemented")
    }
}

// ===================== Types ===================== //

/// Represents all the information required to
/// represent a closure in isolation, i.e., outside of
/// a callsite context where the closure may be getting
/// passed as an argument.
///
/// Composed with other information inside a `ClosureArgDescriptor`
/// to represent a closure as an argument at a callsite.
class ClosureInfo {
    private let closure: SingleValueInstruction
    private let lifetimeFrontier: InstructionRange

    init(closure: SingleValueInstruction, lifetimeFrontier: InstructionRange) {
        self.closure = closure
        self.lifetimeFrontier = lifetimeFrontier
    }
}

/// Represents a closure as an argument at a callsite.
struct ClosureArgDescriptor {
    private let closureInfo: ClosureInfo
    /// The index of the closure in the callsite's argument list.
    private let closureIndex: UInt
    private let parameterInfo: ParameterInfo
    /// This is only needed if we have guaranteed parameters. In most cases
    /// it will have only one element, a return inst. 
    private let nonFailureExitBBs: Stack<BasicBlock>
}

/// Represents a callsite containing one or more closure arguments.
struct CallSiteDescriptor {
    private let applySite: ApplySite
    private let closureArgDescriptors: [ClosureArgDescriptor]
    private let silArgIndexToClosureArgDescIndex: [UInt: UInt]
}

/// A type capable of performing closure specialization on SIL functions
protocol ClosureSpecializer {
    typealias PropagatedClosure = SingleValueInstruction

    func gatherCallSites(_ caller: Function) -> Stack<CallSiteDescriptor>
    func specialize(_ caller: Function) -> Stack<PropagatedClosure>
}

//===--- DiagnoseInfiniteRecursion.swift -----------------------------------==//
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
import SIL

/// A diagnostic pass that detects infinite recursive function calls.
///
/// It detects simple forms of infinite recursions, like
///
///   func f() {
///     f()
///   }
///
/// and can also deal with invariant conditions, like availability checks
///
///   func f() {
///     if #available(macOS 10.4.4, *) {
///       f()
///     }
///   }
///
/// or invariant conditions due to forwarded arguments:
///
///   func f(_ x: Int) {
///     if x > 0 {
///       f(x)
///     }
///   }
///
let diagnoseInfiniteRecursion = FunctionPass(name: "diagnose-infinite-recursion") {
  (function: Function, context: FunctionPassContext) in

  // Don't rerun diagnostics on deserialized functions.
  if function.wasDeserializedCanonical {
    return
  }

  // Try with different sets of invariants. To catch all cases we would need to try all
  // parameter/memory permutations. But in practice, it's good enough to collect a reasonable set by
  // finding all recursive calls and see what arguments they are forwarding.
  guard let invariantsToTry = collectInvariantsToTry(in: function, context) else {
    // There are no recursive calls in the function at all. This is the case for most functions.
    return
  }

  for invariants in invariantsToTry {
    if analizeAndDiagnose(function, with: invariants, context) {
      return
    }
    // Try again, assuming that memory is invariant.
    if analizeAndDiagnose(function, with: invariants.withInvariantMemory, context) {
      return
    }
  }
}

/// Collect invariants with which we should try the analysis and return true if
/// there is at least one recursive call in the function.
private func collectInvariantsToTry(in function: Function, _ context: FunctionPassContext) -> [Invariants]? {
  var invariants = [Invariants]()

  // Try with no invariants.
  invariants.append(Invariants());

  var recursiveCallsFound = false

  // Scan the function for recursive calls.
  for inst in function.instructions {
    if let applySite = inst as? FullApplySite, applySite.isRecursiveCall(context) {
      recursiveCallsFound = true

      // See what parameters the recursive call is forwarding and use that as invariants.
      let inv = Invariants(fromForwardingArgumentsOf: applySite)
      if !invariants.contains(inv) {
        invariants.append(inv)
      }

      // Limit the size of the set to avoid quadratic complexity in corner
      // cases. Usually 4 invariants are more than enough.
      if invariants.count >= 4 {
        return invariants;
      }
    }
  }
  if !recursiveCallsFound {
    return nil
  }
  return invariants;
}


/// Performs the analysis and issues a warnings for recursive calls.
/// Returns true, if at least one recursive call is found.
private func analizeAndDiagnose(_ function: Function,
                                with invariants: Invariants,
                                _ context: FunctionPassContext) -> Bool
{
  var analysis = Analysis(function: function, with: invariants, context)
  defer { analysis.deinitialize() }

  analysis.compute()

  if analysis.isInfiniteRecursiveFunction {
    analysis.printWarningsForInfiniteRecursiveCalls()
    return true
  }
  return false
}

/// Describes what is expected to be invariant in an infinite recursion loop.
///
/// The dataflow analysis is done with a given set of `Invariants`. The correctness of the result (i.e.
/// no false infinite recursion reported) does _not_ depend on the chosen invariants. But it's a trade-off:
/// The more invariants we include, the more conditions might become invariant (which is good).
/// On the other hand, we have to ignore recursive calls which don't forward all invariant arguments.
///
/// We don't know in advance which invariants will yield the best result, i.e. let us detect an
/// infinite recursion. For example, in `f()` we can only detect the infinite recursion if we expect
/// that the parameter `x` is invariant.
/// ```
///   func f(_ x: Int) {
///     if x > 0 {   // an invariant condition!
///       f(x)       // the call is forwarding the argument
///     }
///   }
/// ```
/// But in `g()` we can only detect the infinite recursion if we _don't_ expect that the parameter
/// is invariant.
/// ```
///   func g(_ x: Int) {
///     if x > 0 {   // no invariant condition
///       g(x - 1)   // argument is not forwarded
///     } else {
///       g(x - 2)   // argument is not forwarded
///     }
///   }
/// ```
private struct Invariants: Equatable {

  // Support up to 32 arguments, which should be enough in real world code.
  // As the definition of invariants does not need to be accurate for correctness, it's fine to only support
  // the common cases.
  typealias ArgumentBits = UInt32

  /// A bit mask of indices of arguments which are expected to be invariant.
  /// An argument is invariant if a recursive call forwards the incoming argument.
  /// For example:
  /// ```
  ///     func f(_ x: Int, _ y: Int) {
  ///       f(x, y - 1) // The first argument is invariant, the second is not
  ///     }
  /// ```
  let arguments: ArgumentBits

  /// True, if all type arguments are invariant.
  /// In contrast to `arguments` we don't distinguish between individual type arguments but have a single
  /// flag for all type arguments.
  /// For example:
  /// ```
  ///     func f<T: P>(_ t: T.Type) {
  ///       f(T.self)   // The type argument is invariant
  ///       f(T.V.self) // The type argument is not invariant
  ///     }
  /// ```
  let typeArguments: Bool

  /// True if memory content is invariant.
  /// Like `typeArguments`, it's all or nothing. Either all memory is expected to be invariant (= never
  /// written) or not. We could use AliasAnalysis to do a more fine-grained analysis, but in mandatory
  /// optimizations we want to keep things simple.
  let memory: Bool

  // Nothing is invariant.
  init() {
    self.memory = false
    self.typeArguments = false
    self.arguments = 0
  }

  init(fromForwardingArgumentsOf recursiveApply: FullApplySite) {
    let function = recursiveApply.parentFunction

    // Check which parameters are exactly passed 1:1 to the recursive call.
    var argMask: ArgumentBits = 0
    for (argIndex, arg) in recursiveApply.arguments.enumerated() {
      if argIndex >= MemoryLayout<ArgumentBits>.size * 8 {
        break
      }
      if arg.rootValue == function.arguments[argIndex] {
        argMask |= 1 << argIndex
      }
    }
    self.arguments = argMask

    // Check if the generic type parameters are exactly passed 1:1 to the recursive call.
    self.typeArguments = recursiveApply.substitutionMap == function.forwardingSubstitutionMap

    // Assume memory is not invariant
    self.memory = false
  }

  private init(arguments: ArgumentBits, genericArguments: Bool, memory: Bool) {
    self.arguments = arguments
    self.typeArguments = genericArguments
    self.memory = memory
  }

  var withInvariantMemory: Invariants {
    Invariants(arguments: arguments, genericArguments: typeArguments, memory: true)
  }

  func isArgumentInvariant(at index: Int) -> Bool {
    if index >= MemoryLayout<ArgumentBits>.size * 8 {
      return false
    }
    return (arguments & (1 << index)) != 0
  }
}

/// Performs the analysis to detect infinite recursion loops.
///
/// The basic idea is to see if there is a path from the entry block to a function return without
/// going through an infinite recursive call.
///
private struct Analysis {

  /// All blocks which contain a recursive call.
  var haveRecursiveCall: BasicBlockSet

  /// All blocks which have a terminator with an invariant condition.
  ///
  /// Note: "invariant" means: invariant with respect to the expected invariants,
  ///       which are passed to the initializer.
  var haveInvariantCondition: BasicBlockSet

  /// All blocks from which there is a path to a function exit, without going through a recursive call.
  ///
  /// Note that if memory is expected to be invariant, all memory-writing instructions are also
  /// considered as a "function exit".
  var reachingFunctionExit: BasicBlockSet

  /// All blocks from which there is a path to a recursive call.
  var reachingRecursiveCall: BasicBlockSet

  private let function: Function
  private let invariants: Invariants
  private let context: FunctionPassContext

  init(function: Function, with invariants: Invariants, _ context: FunctionPassContext) {
    self.haveRecursiveCall = BasicBlockSet(context)
    self.haveInvariantCondition = BasicBlockSet(context)
    self.reachingFunctionExit = BasicBlockSet(context)
    self.reachingRecursiveCall = BasicBlockSet(context)
    self.function = function
    self.context = context
    self.invariants = invariants
  }

  mutating func compute() {
    computeInitialSets()
    propagateReachingRecursiveCall()
    propagateReachingFunctionExit()
  }

  mutating func deinitialize() {
    haveRecursiveCall.deinitialize()
    haveInvariantCondition.deinitialize()
    reachingFunctionExit.deinitialize()
    reachingRecursiveCall.deinitialize()
  }

  var isInfiniteRecursiveFunction: Bool { isInInfiniteRecursionLoop(function.entryBlock) }

  func printWarningsForInfiniteRecursiveCalls() {
    var worklist = BasicBlockWorklist(context)
    defer { worklist.deinitialize() }

    // Print warnings for the first recursive call(s) we reach from the entry block.
    worklist.pushIfNotVisited(function.entryBlock)
    while let block = worklist.pop() {
      if case .recursive(let apply) = block.getKind(for: invariants, context) {
        context.diagnosticEngine.diagnose(.warn_infinite_recursive_call, at: apply.location)
      } else {
        for succ in block.successors where isInInfiniteRecursionLoop(succ) {
          worklist.pushIfNotVisited(succ)
        }
      }
    }
  }

  private mutating func computeInitialSets() {
    for block in function.blocks {
      switch block.getKind(for: invariants, context) {
      case .transparent:
        break
      case .functionExiting:
        reachingFunctionExit.insert(block)
      case .recursive:
        haveRecursiveCall.insert(block)
        reachingRecursiveCall.insert(block)
      case .invariantCondition:
        haveInvariantCondition.insert(block)
      }
    }
  }

  private mutating func propagateReachingRecursiveCall() {
    var worklist = Stack<BasicBlock>(context)
    defer { worklist.deinitialize() }

    worklist.append(contentsOf: function.blocks.filter { reachingRecursiveCall.contains($0) })

    while let block = worklist.pop() {
      for pred in block.predecessors {
        if reachingRecursiveCall.insert(pred) {
          worklist.push(pred)
        }
      }
    }
  }

  private mutating func propagateReachingFunctionExit() {
    var worklist = Stack<BasicBlock>(context)
    defer { worklist.deinitialize() }

    worklist.append(contentsOf: function.blocks.filter { reachingFunctionExit.contains($0) })

    while let block = worklist.pop() {
      for pred in block.predecessors where !reachingFunctionExit.contains(pred) {
        // Recursive calls block the propagation.
        if  haveRecursiveCall.contains(pred) {
          continue
        }

        // This is the trick for handling invariant conditions: usually `reachingFunctionExit` is propagated
        // if _any_ of the successors reach a function exit.
        // For invariant conditions, it's only propagated if _all_ successors reach a function exit.
        // If at least one of the successors is in an infinite recursion loop and this successor is
        // taken once, it will be taken forever (because the condition is invariant).
        if haveInvariantCondition.contains(pred),
           pred.successors.contains(where: isInInfiniteRecursionLoop)
        {
          continue
        }

        reachingFunctionExit.insert(pred)
        worklist.push(pred)
      }
    }
  }

  private func isInInfiniteRecursionLoop(_ block: BasicBlock) -> Bool {
    return reachingRecursiveCall.contains(block) && !reachingFunctionExit.contains(block)
  }
}

private enum BlockKind {
  case functionExiting          // the block is exiting the function (e.g. via a `return`)
  case recursive(FullApplySite) // the block contains a recursive call
  case invariantCondition       // the block's terminator has an invariant condition
  case transparent              // all other blocks
}

private extension BasicBlock {
  func getKind(for invariants: Invariants, _ context: FunctionPassContext) -> BlockKind {
    for inst in instructions {
      if let apply = inst as? FullApplySite {
        // Ignore blocks which call a @_semantics("programtermination_point").
        // This is an assert-like program termination and we explicitly don't
        // want this call to disqualify the warning for infinite recursion,
        // because they're reserved for exceptional circumstances.
        if let callee = apply.referencedFunction, callee.hasSemanticsAttribute("programtermination_point") {
          return .transparent
        }

        if apply.isRecursiveCall(context), apply.hasInvariantArguments(with: invariants) {
          return .recursive(apply)
        }
      }
      if invariants.memory, inst.mayReallyWriteToMemory {
        // If we are assuming that all memory is invariant, a memory-writing
        // instruction potentially breaks the infinite recursion loop. For the
        // sake of the analysis, it's like a function exit.
        return .functionExiting
      }
    }
    if terminator.isFunctionExiting ||
       // Also treat non-assert-like unreachables as returns, like "exit()".
        terminator is UnreachableInst
    {
      return .functionExiting
    }
    if terminator.isInvariant(accordingTo: invariants, context) {
      return .invariantCondition
    }
    return .transparent
  }
}

private extension FullApplySite {
  /// True if this apply calls its parent function.
  func isRecursiveCall(_ context: FunctionPassContext) -> Bool {
    if let calledFn = referencedFunction {
      return calledFn == parentFunction
    }

    switch callee {
    case let cmi as ClassMethodInst:
      let classType = cmi.operand.value.type.canonicalType.lookThroughMetatype
      guard let nominal = classType.nominal,
            let classDecl = nominal as? ClassDecl,
            // It's sufficient to handle class methods in the module where they are defined.
            // This aovids the need to de-serialized vtables from other modules.
           classDecl.parentModule == context.currentModuleContext,
           let vtable = context.lookupVTable(for: classDecl),
           let entry = vtable.lookup(method: cmi.member),
           entry.implementation == parentFunction
      else {
        return false
      }

      if cmi.member.calleesAreStaticallyKnowable(context),
          // The "statically knowable" check just means that we have all the
          // callee candidates available for analysis. We still need to check
          // if the current function has a known override point.
          !(cmi.member.decl as! AbstractFunctionDecl).isOverridden
      {
        return true
      }

      // Even if the method is (or could be) overridden, it's a recursive call if
      // it's called on the self argument:
      // ```
      // class X {
      //   // Even if foo() is overridden in a derived class, it'll end up in an
      //   // infinite recursion if initially called on an instance of `X`.
      //   func foo() { foo() }
      // }
      // ```
      if let selfArgument = parentFunction.selfArgument, cmi.operand.value == selfArgument {
        return true
      }
      return false

    case let wmi as WitnessMethodInst:
      if wmi.conformance.isConcrete,
         let wTable = context.lookupWitnessTable(for: wmi.conformance.rootConformance),
         let method = wTable.lookup(method: wmi.member),
         method == parentFunction
      {
        return true
      }
      return false

    default:
      return false
    }
  }

  func hasInvariantArguments(with invariants: Invariants) -> Bool {
    return arguments.enumerated().allSatisfy { (argIndex, arg) in
      !invariants.isArgumentInvariant(at: argIndex) ||
      arg.rootValue == parentFunction.arguments[argIndex]
    }
  }
}

private extension CanonicalType {
  var lookThroughMetatype: CanonicalType {
    if self.isMetatype {
      return self.instanceTypeOfMetatype
    }
    return self
  }
}

private extension Value {
  /// Recursively walks the use-def chain starting at this value and returns
  /// true if all visited values are invariant.
  func isInvariant(accordingTo invariants: Invariants, visited: inout InstructionSet) -> Bool {
    if let inst = definingInstruction {
      // Avoid exponential complexity in case a value is used by multiple
      // operands.
      if !visited.insert(inst) {
        return true
      }

      if !invariants.memory, inst.mayReadFromMemory {
        return false
      }

      if !invariants.typeArguments, inst.mayDependOnTypeParameters {
        return false
      }

      for op in inst.operands {
        if !op.value.isInvariant(accordingTo: invariants, visited: &visited) {
          return false
        }
      }
      return true
    }

    if let funcArg = self as? FunctionArgument {
      return invariants.isArgumentInvariant(at: funcArg.index)
    }
    return false
  }

  var rootValue: Value {
    switch self {
    case let ba as BeginAccessInst:
      return ba.operand.value.rootValue
    default:
      return self
    }
  }
}

private extension Instruction {
  var mayReallyWriteToMemory: Bool {
    switch self {
    case is BeginAccessInst, is EndAccessInst,
         // A `load` is defined to write memory or have side effects in two cases:
         // * We don't care about retain instructions of a `load [copy]`.
         // * We don't care about a `load [take]` because it cannot occur in an
         //   infinite recursion loop without another write (which re-initializes
         //   the memory).
         is LoadInst:
      return false
    default:
      return mayWriteToMemory
    }
  }

  var mayDependOnTypeParameters: Bool {
    switch self {
    case let bi as BuiltinInst:
      return bi.substitutionMap.replacementTypes.contains { $0.hasArchetype }
    case let mt as MetatypeInst:
      return mt.type.hasArchetype
    default:
      return false
    }
  }
}

private extension TermInst {
  func isInvariant(accordingTo invariants: Invariants, _ context: FunctionPassContext) -> Bool {
    switch self {
    case is SwitchEnumAddrInst,
         is CheckedCastAddrBranchInst:
      if !invariants.memory {
        return false
      }
      fallthrough
    case is CondBranchInst,
         is SwitchValueInst,
         is SwitchEnumInst,
         is CheckedCastBranchInst:
      var visited = InstructionSet(context)
      defer { visited.deinitialize() }
      return operands[0].value.isInvariant(accordingTo: invariants, visited: &visited)
    default:
      return false
    }
  }
}

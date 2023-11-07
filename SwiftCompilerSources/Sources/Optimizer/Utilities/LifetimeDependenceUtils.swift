//===--- LifetimeDependenceUtils.swift - Utils for lifetime dependence ----===//
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

/// Walk up the value dependence chain to find the best-effort variable declaration. Typically called while diagnosing an error.
///
/// The walk stops at:
/// - an address
/// - a variable declaration (begin_borrow [var_decl], move_value [var_decl])
/// - the root of the dependence chain
///
/// If the introducer is an address, then the client can call Value.enclosingAccess iteratively to find to AccessBase. This walker is useful for finding the innermost access, which may also be relevant for diagnostics.
func gatherVariableIntroducers(for value: Value, _ context: Context) -> [Value]
{
  var introducers: [Value] = []
  var useDefVisitor = VariableIntroducerUseDefWalker(context) {
    introducers.append($0)
    return .continueWalk
  }
  defer { useDefVisitor.deinitialize() }
  _ = useDefVisitor.walkUp(value: value)
  return introducers
}

/// A lifetime dependence represents a scope in which some parent value is alive and accessible along with a dependent value. All values derived from the dependent value must be used within this scope. This supports diagnostics on non-escapable types.
///
/// A lifetime dependence is produced by either 'mark_dependence [nonescaping]':
///
///   %dependent = mark_dependence [nonescaping] %value on %parent
///
/// or a non-escapable function argument:
///
///   bb0(%dependent : NonEscapableThing):
///
/// A  lifetime dependence identifies its parent value, the kind of scope that the parent value represents, and a dependent value. A self-dependence has the same parent and dependent value:
///
///   %dependent = mark_dependence [nonescaping] %value on %value
///
/// Self-dependence is useful to ensure that derived values, including copies, do not escape the lifetime of the original value. Non-escapable function arguments are implicitly self-dependent, meaning that the argument's value does not escape the function body. Note that we do not insert a 'mark_dependence [nonescaping]' for function arguments because the caller must already represent the argument's dependence on some parent value. That parent value may not be the value directly passed to the argument. After inlining, an additional self-dependence on argument value would be overly strict.
struct LifetimeDependence {
  enum Scope {
    /// A non-escapable or guaranteed argument whose scope is provided
    /// by the caller and covers the entire function.
    case caller(Argument)
    /// An access scope.
    case access(BeginAccessInst)
    /// An owned value whose OSSA lifetime encloses nonescapable values
    case owned(Value)
    /// An singly-initialized address-only value, in which the entire
    /// initialized region is a lifetime (as opposed to an individual
    /// access). e.g. A value produced by @out.
    case initialized(Value)

    var parentValue: Value {
      switch self {
      case let .caller(argument): return argument
      case let .access(beginAccess): return beginAccess
      case let .owned(value): return value
      case let .initialized(value): return value
      }
    }

    func checkPrecondition() {
      switch self {
      case let .caller(argument):
        precondition(!argument.type.isEscapable
          || argument.ownership == .guaranteed,
          "only non-escapable or guaranteed arguments have a caller scope")
      case .access:
        break
      case let .owned(value):
        precondition(value.ownership == .owned)
      case let .initialized(value):
        precondition(value.type.isAddress, "expected an address")
      }
    }
    var description: String {
      {
        switch self {
        case .caller: "Caller: "
        case .access: "Access: "
        case .owned: "Owned: "
        case .initialized: "Initialized: "
        }
      }() + "\(parentValue)"
    }
  }
  let scope: Scope
  let dependentValue: Value
  
  var parentValue: Value { scope.parentValue }

  var function: Function {
    dependentValue.parentFunction // dependentValue can't be undef
  }

  var description: String {
    return scope.description +
      """
      dependent: \(dependentValue)
      """
  }
}

extension LifetimeDependence {
  /// Construct LifetimeDependence from mark_dependence [nonescaping]
  ///
  /// TODO: Add SIL verification that all mark_depedence [nonescaping]
  /// have a valid LifetimeDependence.
  init?(markDependenceInst: MarkDependenceInst, _ context: some Context) {
    guard markDependenceInst.isNonEscaping else { return nil }
    guard let scope = Scope(base: markDependenceInst.base, context) else {
      return nil
    }
    self.scope = scope
    self.dependentValue = markDependenceInst.value
  }
}

extension LifetimeDependence.Scope {
  /// Construct a lifetime dependence scope from the base value that
  /// other values depend on. This derives the kind of dependence
  /// scope and its parentValue from `base`.
  init?(base: Value, _ context: some Context) {
    if base.type.isAddress {
      let accessScope = base.enclosingAccessScope
      guard case let .scope(access) = accessScope else {
        // TODO: Handle singly initialize address-only values and
        // create a LifetimeDependence.initialized
        fatalError("a non-escaping scope's address must be in an access scope")
      }
      self = .access(access)
      return
    }
    switch base.ownership {
    case .owned:
      self = .owned(base)
      return
    case .guaranteed:
      var introducers = Stack<Value>(context)
      gather(borrowIntroducers: &introducers, for: base, context)
      // TODO: Add a SIL verifier check that a mark_dependence [nonescaping]
      // base is never a guaranteed phi.
      guard let introducer = introducers.pop() else { return nil }
      assert(introducers.isEmpty,

        "guaranteed phis not allowed when diagnosing lifetime dependence")
      guard let beginBorrow = BeginBorrowValue(introducer) else {
        fatalError("unknown borrow introducer")
      }
      if let borrowOperand = beginBorrow.baseOperand {
        if let scope = LifetimeDependence.Scope(base: borrowOperand.value,
          context) {
          self = scope
          return
        }
        return nil
      }
      self = .caller(beginBorrow.value as! Argument)
      return
    case .none:
      // lifetime dependence requires a nontrivial value"
      return nil
    case .unowned:
      fatalError("Cannot create a dependence on an unowned value")
    }
  }
}

extension LifetimeDependence.Scope {
  /// Compute the range of the dependence scope. 
  ///
  /// Returns nil if the dependence scope covers the entire function.
  ///
  /// Note: The caller must deinitialize the returned range.
  func computeRange(_ context: Context) -> InstructionRange? {
    switch self {
    case .caller: return nil
    case let .access(beginAccessInst):
      var range = InstructionRange(begin: beginAccessInst, context)
      for endInst in beginAccessInst.endInstructions {
        range.insert(endInst)
      }
      return range
    case let .owned(value):
      return computeLinearLiveness(for: value, context)
    case let .initialized(value):
      return LifetimeDependence.Scope.computeIndirectResultRange(value: value,
        context)
    }
  }
  
  private static func computeIndirectResultRange(
    value: Value, _ context: Context
  ) -> InstructionRange {
    // TODO: add a utility for addressible lifetimes. This only
    // happens for singly initialized address-only values, like
    // indirect function results. It will be sufficient to visit all
    // the destroy_addr, copy_addr [take], and load[take] address
    // uses. This is unreachable with opaque SIL values.
    fatalError("Unimplemented: addressible lifetime parent")
  }
}

extension LifetimeDependence {
  /// TODO: By making UseDefVisitor a NonEscapable type, we should be
  /// able to make \p visitor a non-escaping closure.
  static func visitDependenceRoots(enclosing value: Value,
    _ context: Context,
    _ visitor: @escaping (LifetimeDependence.Scope) -> WalkResult)
  -> WalkResult {
    var useDefVisitor = UseDefVisitor(context, visitor)
    defer { useDefVisitor.deinitialize() }
    return useDefVisitor.walkUp(value: value)
  }
  
  private struct UseDefVisitor : LifetimeDependenceUseDefWalker {
    let context: Context
    // This visited set is only really needed for instructions with
    // multiple results, including phis.
    private var visitedValues: ValueSet
    // Call \p visit rather than calling this directly.
    private let visitorClosure: (LifetimeDependence.Scope) -> WalkResult
    
    init(_ context: Context,
         _ visitor: @escaping (LifetimeDependence.Scope) -> WalkResult) {
      self.context = context
      self.visitedValues = ValueSet(context)
      self.visitorClosure = visitor
    }
    
    mutating func deinitialize() {
      visitedValues.deinitialize()
    }
    
    mutating func needWalk(for value: Value) -> Bool {
      visitedValues.insert(value)
    }

    // Visit the base value of a lifetime dependence. If the base is
    // an address, the dependence scope is the enclosing access. The
    // walker does not walk past an `mark_dependence [nonescaping]`
    // that produces an address, because that will never occur inside
    // of an access scope. An address type mark_dependence
    // [nonescaping]` can only result from an indirect function result
    // when opaque values are not enabled.
    mutating func introducer(_ value: Value) -> WalkResult {
      guard let scope = LifetimeDependence.Scope(base: value, context)
      else {
        return .continueWalk
      }
      scope.checkPrecondition()
      return visitorClosure(scope)
    }
  }
}

struct VariableIntroducerUseDefWalker : LifetimeDependenceUseDefWalker {
  let context: Context
  // This visited set is only really needed for instructions with
  // multiple results, including phis.
  private var visitedValues: ValueSet

  // Call \p visit rather than calling this directly.
  private let visitorClosure: (Value) -> WalkResult

  init(_ context: Context, _ visitor: @escaping (Value) -> WalkResult) {
    self.context = context
    self.visitedValues = ValueSet(context)
    self.visitorClosure = visitor
  }

  mutating func deinitialize() {
    visitedValues.deinitialize()
  }
 
  mutating func needWalk(for value: Value) -> Bool {
    visitedValues.insert(value)
  }

  mutating func introducer(_ value: Value) -> WalkResult {
    return visitorClosure(value)
  }

  mutating func walkUp(value: Value) -> WalkResult {
    switch value.definingInstruction {
    case let moveInst as MoveValueInst:
      if moveInst.isFromVarDecl {
        return introducer(moveInst)
      }
    case let borrow as BeginBorrowInst:
      if borrow.isFromVarDecl {
        return introducer(borrow)
      }
    default:
      break
    }
    return walkUpDefault(dependent: value)
  }
}

/// Walk up dominating dependent values.
///
/// Find the roots of a dependence chain stopping at phis. These root
/// LifeDependence instances are the value's "inherited"
/// dependencies. In this example, the dependence root is copied,
/// borrowed, and forwarded before being used as the base operand of
/// `mark_dependence`. The dependence "root" is the parent of the
/// outer-most dependence scope.
///
///   %root = apply                  // lifetime dependence root
///   %copy = copy_value %root
///   %parent = begin_borrow %copy   // lifetime dependence parent value
///   %base = struct_extract %parent // lifetime dependence base value
///   %dependent = mark_dependence [nonescaping] %value on %base
///
/// This extends the ForwardingDefUseWalker, which finds the
/// forward-extended lifetime introducers. Certain forward-extended
/// lifetime introducers produce dependent values: namely copies,
/// moves, and borrows. These introducers are considered part of their
/// operand's dependence scope because non-escapable values can be
/// copied, moved, and borrowed. Nonetheless, all of their uses must
/// remain within original dependence scope.
///
///   # owned lifetime dependence
///   %parent = apply               // begin dependence scope -+
///   ...                                                      |
///   %1 = mark_dependence [nonescaping] %value on %parent     |
///   ...                                                      |
///   %2 = copy_value %1        -+                             |
///   # forwarding instruction   |                             |
///   %3 = struct $S (%2)        | forward-extended lifetime   |
///                              |                             | OSSA Lifetime
///   %4 = move_value %3        -+                             |
///   ...                        | forward-extended lifetime   |
///   %5 = begin_borrow %4       | -+                          |
///   # dependent use of %1      |  | forward-extended lifetime|
///   end_borrow %5              | -+                          |
///   destroy_value %4          -+                             |
///   ...                                                      |
///   destroy_value %parent        // end dependence scope    -+
///
/// All of the dependent uses including `end_borrow %5` and
/// `destroy_value %4` must be before the end of the dependence scope:
/// `destroy_value %parent`. In this case, the dependence parent is an
/// owned value, so the scope is simply the value's OSSA lifetime.
///
/// Minimal requirements:
///   var context: Context
///   introducer(_ value: Value) -> WalkResult
///   needWalk(for value: Value) -> Bool
///
/// Start walking:
///   walkUp(value: Value) -> WalkResult
protocol LifetimeDependenceUseDefWalker : ForwardingUseDefWalker {
  var context: Context { get }

  mutating func introducer(_ value: Value) -> WalkResult

  // Minimally, check a ValueSet. This walker may traverse chains of aggregation and destructuring along with phis.
  mutating func needWalk(for value: Value) -> Bool

  mutating func walkUp(value: Value) -> WalkResult
}

extension LifetimeDependenceUseDefWalker {
  mutating func walkUp(value: Value) -> WalkResult {
    walkUpDefault(dependent: value)
  }

  // Extend ForwardingUseDefWalker to handle copies, moves, and
  // borrows. Also transitively walk up other lifetime dependencies to
  // find the roots.
  //
  // If `value` is an address, this immediately invokes
  // `introducer()`. We only expect to see an address-type
  // `mark_dependence [nonescaping]` when opaque values are disabled.
  //
  // Handles loads as a convenience so the client receives the load's
  // address as an introducer.
  mutating func walkUpDefault(dependent value: Value) -> WalkResult {
    switch value.definingInstruction {
    case let copyInst as CopyValueInst:
      return walkUp(value: copyInst.fromValue)
    case let moveInst as MoveValueInst:
      return walkUp(value: moveInst.fromValue)
    case let borrow as BeginBorrowInst:
      return walkUp(value: borrow.borrowedValue)
    case let load as LoadInstruction:
      return walkUp(value: load.address)
    case let markDep as MarkDependenceInst:
      if let dependence = LifetimeDependence(markDependenceInst: markDep,
        context) {
        return walkUp(value: dependence.parentValue)
      }
    default:
      break
    }
    // If the dependence chain has a phi, consider it a root. Dependence roots
    // are currently expected to dominate all dependent values.
    if Phi(value) != nil {
      return introducer(value)
    }
    return walkUpDefault(forwarded: value)
  }
}

/// Walk down dependent values.
///
/// Delegates value def-use walking to the ForwardingUseDefWalker and
/// overrides copy, move, borrow, and mark_dependence.
///
/// Stops at an address value. Finding the leaf uses of an address
/// requires an AddressDefUseWalker.
///
/// Ignores trivial values (~Escapable types are never
/// trivial. Escapable types may only be lifetime-depenent values if
/// they are non-trivial).
///
/// Skips uses within nested borrow scopes.
///
/// TODO: override BeginAccessInst to handle EndAccessInst.
///
/// Minimal requirements:
///   needWalk(for value: Value) -> Bool
///   leafUse(_ operand: Operand) -> WalkResult
///   deadValue(_ value: Value, using operand: Operand?) -> WalkResult
///
/// Start walking:
///   walkDown(root: Value)
protocol LifetimeDependenceDefUseWalker : ForwardingDefUseWalker {
  var function: Function { get }
  var context: Context { get }

  // Minimally, check a ValueSet. This walker may traverse chains of
  // aggregation and destructuring by default. Implementations may
  // handle phis.
  mutating func needWalk(for value: Value) -> Bool

  mutating func leafUse(_ operand: Operand) -> WalkResult

  // Report any initial or dependent value with no uses. Only relevant for
  // guaranteed values or incomplete OSSA. This could be a dead
  // instruction, a terminator in which the result is dead on one
  // path, or a dead phi.
  //
  // \p operand is nil if \p value is the root.
  mutating func deadValue(_ value: Value, using operand: Operand?) -> WalkResult

  mutating func walkDownUses(of: Value, using: Operand?) -> WalkResult
    
  mutating func walkDown(operand: Operand) -> WalkResult
}

extension LifetimeDependenceDefUseWalker {
  // Callback from ForwardingDefUseWalker.
  mutating func walkDown(operand: Operand) -> WalkResult {
    switch operand.instruction {
    case let copy as CopyValueInst:
      return walkDownUses(of: copy, using: operand)

    case let move as MoveValueInst:
      return walkDownUses(of: move, using: operand)

    case let borrow as BeginBorrowInst:
      // Skip the uses within nested borrow scopes.
      for endOperand in BeginBorrowValue(borrow)!.scopeEndingOperands {
        switch endOperand.ownership {
        case .endBorrow:
          if leafUse(operand) == .abortWalk {
            return .abortWalk
          }
        case .reborrow:
          let phi = Phi(using: operand)!
          if walkDownUses(of: phi.value, using: operand) == .abortWalk {
            return .abortWalk
          }
        default:
          fatalError("begin_borrow scope cannot be consumed")
        }
      }
      return .continueWalk

    case let markDep as MarkDependenceInst:
      // Follow the uses of nonescaping dependents.
      if markDep.baseOperand == operand
      && LifetimeDependence(markDependenceInst: markDep, context) != nil {
        return walkDownUses(of: markDep, using: operand)
      }
      // Let the forwarding walker handle the mark_dependence value operand.
    default:
      break
    }
    return walkDownDefault(forwarding: operand)
  }
}

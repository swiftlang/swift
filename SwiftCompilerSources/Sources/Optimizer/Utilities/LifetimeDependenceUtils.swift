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
//
// Utilities that specify lifetime dependence:
//
// gatherVariableIntroducers(for:) is a use-def walk that returns the
// values that most closely associated with the variable declarations
// that the given value holds an instance of.
//
// LifetimeDependence.init(base:) finds the OSSA lifetime for `base`
// and categorizes the kind of dependence scope that the lifetime
// represents.
//
// LifetimeDependence.Scope.computeRange() computes the instruction
// range covered by the dependence scope.
//
// LifetimeDependence.visitDependenceRoots(enclosing:) is a use-def
// walk that walks up the chain of dependent values and visits the
// earliest LifetimeDependencies that whose lifetime can be inherited
// by the given value.
//
// LifetimeDependenceDefUseWalker walks the def-use chain to find all
// values that depend on the given OSSA lifetime.
//
//===----------------------------------------------------------------------===//

import SIL

/// Walk up the value dependence chain to find the best-effort
/// variable declaration. Typically called while diagnosing an error.
///
/// The walk stops at:
/// - an address
/// - a variable declaration (begin_borrow [var_decl], move_value [var_decl])
/// - the root of the dependence chain
///
/// If the introducer is an address, then the client can call
/// Value.enclosingAccess iteratively to find to AccessBase. This
/// walker is useful for finding the innermost access, which may also
/// be relevant for diagnostics.
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

/// A lifetime dependence represents a scope in which some parent
/// value is alive and accessible along with a dependent value. All
/// values derived from the dependent value must be used within this
/// scope. This supports diagnostics on non-escapable types.
///
/// A lifetime dependence is produced by either 'mark_dependence [nonescaping]':
///
///   %dependent = mark_dependence [nonescaping] %value on %parent
///
/// or a non-escapable function argument:
///
///   bb0(%dependent : NonEscapableThing):
///
/// A lifetime dependence identifies its parent value, the kind of
/// scope that the parent value represents, and a dependent value. A
/// self-dependence has the same parent and dependent value:
///
///   %dependent = mark_dependence [nonescaping] %value on %value
///
/// Self-dependence is useful to ensure that derived values, including
/// copies, do not escape the lifetime of the original
/// value. Non-escapable function arguments are implicitly
/// self-dependent, meaning that the argument's value does not escape
/// the function body. Note that we do not insert a 'mark_dependence
/// [nonescaping]' for function arguments because the caller must
/// already represent the argument's dependence on some parent
/// value. That parent value may not be the value directly passed to
/// the argument. After inlining, an additional self-dependence on
/// argument value would be overly strict.
struct LifetimeDependence : CustomStringConvertible {
  enum Scope : CustomStringConvertible {
    /// A non-escapable, guaranteed, or inout argument whose scope is provided
    /// by the caller and covers the entire function.
    case caller(Argument)
    /// An access scope.
    case access(BeginAccessInst)
    /// An coroutine.
    case yield(Value)
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
      case let .yield(value): return value
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
      case let .yield(value):
        precondition(value.definingInstruction is BeginApplyInst)
      case let .owned(value):
        precondition(value.ownership == .owned)
      case let .initialized(value):
        precondition(value.type.isAddress, "expected an address")
      }
    }
    var description: String {
      {
        switch self {
        case .caller: return "Caller: "
        case .access: return "Access: "
        case .yield: return "Yield: "
        case .owned: return "Owned: "
        case .initialized: return "Initialized: "
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
    return scope.description + "\nDependent: \(dependentValue)"
  }
}

extension LifetimeDependence {
  /// Construct LifetimeDependence from mark_dependence [nonescaping]
  ///
  /// TODO: Add SIL verification that all mark_depedence [nonescaping]
  /// have a valid LifetimeDependence.
  init?(markDependence: MarkDependenceInst, _ context: some Context) {
    guard markDependence.isNonEscaping else { return nil }
    guard let scope = Scope(base: markDependence.base, context) else {
      return nil
    }
    self.scope = scope
    self.dependentValue = markDependence.value
  }
}

extension LifetimeDependence.Scope {
  /// Construct a lifetime dependence scope from the base value that
  /// other values depend on. This derives the kind of dependence
  /// scope and its parentValue from `base`.
  ///
  /// `base` represents the OSSA lifetime that the dependent value
  /// must be used within. If `base` is owned, then it directly
  /// defines the parent lifetime. If `base` is guaranteed, then it
  /// must have a single borrow introducer, which defines the parent
  /// lifetime. `base` must not be derived from a guaranteed phi or
  /// forwarded (via struct/tuple) from multiple guaranteed values.
  init?(base: Value, _ context: some Context) {
    if base.type.isAddress {
      switch base.enclosingAccessScope {
      case let .scope(access):
        self = .access(access)
        return
      case let .base(accessBase):
        if case let .argument(arg) = accessBase {
          self = .caller(arg)
          return
        }
        // TODO: Handle singly initialize address-only values:
        // if case let .stack(stackAddr) = accessBase:
        // and create a LifetimeDependence.initialized
        fatalError("a non-escaping scope's address must be in an access scope")
      }
    }
    switch base.ownership {
    case .owned:
      self = .owned(base)
      return
    case .guaranteed:
      if let scope = Self(guaranteed: base, context) {
        self = scope
        return
      }
      return nil
    case .none:
      // lifetime dependence requires a nontrivial value"
      return nil
    case .unowned:
      fatalError("Cannot create a dependence on an unowned value")
    }
  }

  private init?(guaranteed base: Value, _ context: some Context) {
    var introducers = Stack<Value>(context)
    gatherBorrowIntroducers(for: base, in: &introducers, context)
    // If introducers is empty, then the dependence is on a trivial value, so
    // there is no dependence scope.
    //
    // TODO: Add a SIL verifier check that a mark_dependence [nonescaping]
      // base is never a guaranteed phi.
    guard let introducer = introducers.pop() else { return nil }
    assert(introducers.isEmpty,
           "guaranteed phis not allowed when diagnosing lifetime dependence")
    guard let beginBorrow = BeginBorrowValue(introducer) else {
      fatalError("unknown borrow introducer")
    }
    switch beginBorrow {
    case .beginBorrow, .loadBorrow:
      let borrowOperand = beginBorrow.baseOperand!
      if let scope = LifetimeDependence.Scope(base: borrowOperand.value,
                                              context) {
        self = scope
        return
      }
      return nil
    case let .beginApply(value):
      self = .yield(value)
    case .functionArgument:
      self = .caller(beginBorrow.value as! Argument)
      return
    case .reborrow:
      fatalError("reborrows are not supported in diagnostics")
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
    case .caller:
      return nil
    case let .access(beginAccess):
      var range = InstructionRange(begin: beginAccess, context)
      range.insert(contentsOf: beginAccess.endInstructions)
      return range
    case let .yield(value):
      let def = value.definingInstruction!
      var range = InstructionRange(begin: def, context)
      _ = BorrowingInstruction(def)!.visitScopeEndingOperands(context) {
        range.insert($0.instruction)
        return .continueWalk
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
      if let dependence = LifetimeDependence(markDependence: markDep,
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
///   leafUse(of: Operand) -> WalkResult
///   deadValue(_ value: Value, using operand: Operand?) -> WalkResult
///   pointerEscapingUse(of operand: Operand) -> WalkResult
///
/// Start walking:
///   walkDown(root: Value)
protocol LifetimeDependenceDefUseWalker : ForwardingDefUseWalker,
                                          OwnershipUseVisitor,
                                          AddressUseVisitor {
  var function: Function { get }

  mutating func leafUse(of operand: Operand) -> WalkResult
}

// ForwardingDefUseWalker
extension LifetimeDependenceDefUseWalker {
  // Callback from ForwardingDefUseWalker.
  //
  // This is not a ForwardingInstruction use, so consider it a leaf use.
  mutating func nonForwardingUse(_ operand: Operand) -> WalkResult {
    // OwnershipVisitor classified this as a forwarding use, but it is
    // not a forwarding instruction.
    leafUse(of: operand)
  }

  // Callback from ForwardingDefUseWalker.
  mutating func walkDown(operand: Operand) -> WalkResult {
    classify(operand: operand)
  }
}

// OwnershipUseVisitor
extension LifetimeDependenceDefUseWalker {
  mutating func ownershipLeafUse(of operand: Operand, isInnerLifetime: Bool)
    -> WalkResult {
    switch operand.instruction {
    case let copy as CopyValueInst:
      return walkDownUses(of: copy, using: operand)

    case let move as MoveValueInst:
      return walkDownUses(of: move, using: operand)

    default:
      break
    }
    return walkDownDefault(forwarding: operand)
  }

  mutating func forwardingUse(of operand: Operand, isInnerLifetime: Bool)
    -> WalkResult {
    return walkDownDefault(forwarding: operand)
  }

  mutating func interiorPointerUse(of: Operand, into address: Value)
    -> WalkResult {
    return walkDownAddressUses(of: address)
  }

  mutating func dependentUse(of operand: Operand, into value: Value)
    -> WalkResult {
    return walkDownUses(of: value, using: operand)
  }

  mutating func borrowingUse(of operand: Operand,
                             by borrowInst: BorrowingInstruction)
    -> WalkResult {
    return visitInnerBorrowUses(of: borrowInst)   
  }

  // TODO: Consider supporting lifetime dependence analysis of
  // guaranteed phis. See InteriorUseWalker.walkDown(guaranteedPhi: Phi)
  mutating func reborrowingUse(of operand: Operand, isInnerLifetime: Bool)
    -> WalkResult {
    return pointerEscapingUse(of: operand)
  }
}

extension LifetimeDependenceDefUseWalker {
  /// An address projection produces a single address result and does not
  /// escape its address operand in any other way.
  mutating func projectedAddressUse(of operand: Operand, into value: Value)
    -> WalkResult {
    return walkDownAddressUses(of: value)
  }

  mutating func scopedAddressUse(of operand: Operand) -> WalkResult {
    switch operand.instruction {
    case let ba as BeginAccessInst:
      return ba.endOperands.walk { leafUse(of: $0) }
    case let ba as BeginApplyInst:
      return ba.token.uses.walk { leafUse(of: $0) }
    case let sb as StoreBorrowInst:
      return sb.uses.filterUsers(ofType: EndBorrowInst.self).walk {
        leafUse(of: $0)
      }
    case let load as LoadBorrowInst:
      return load.uses.endingLifetime.walk { leafUse(of: $0) }
    default:
      fatalError("Unrecognized scoped address use: \(operand.instruction)")
    }
  }

  mutating func leafAddressUse(of operand: Operand) -> WalkResult {
    return leafUse(of: operand)
  }

  mutating func loadedAddressUse(of operand: Operand, into value: Value)
    -> WalkResult {
    return walkDownUses(of: value, using: operand)
  }    

  mutating func loadedAddressUse(of operand: Operand, into address: Operand)
    -> WalkResult {
    // TODO: Call reaching-def analysis on the local variable that
    // defines `address`. Find all the uses of this store into
    // `address`.
    return pointerEscapingUse(of: operand)
  }

  mutating func dependentAddressUse(of operand: Operand, into value: Value)
    -> WalkResult {
    walkDownUses(of: value, using: operand)
  }    

  mutating func escapingAddressUse(of operand: Operand) -> WalkResult {
    return pointerEscapingUse(of: operand)
  }

  mutating func unknownAddressUse(of operand: Operand) -> WalkResult {
    return .abortWalk
  }

  private mutating func walkDownAddressUses(of address: Value) -> WalkResult {
    address.uses.ignoreTypeDependence.walk {
      return classifyAddress(operand: $0)
    }
  }
}

let variableIntroducerTest = FunctionTest("variable_introducer") {
    function, arguments, context in
  let value = arguments.takeValue()
  print("Variable introducers of: \(value)")
  print(gatherVariableIntroducers(for: value, context))
}

let lifetimeDependenceScopeTest = FunctionTest("lifetime_dependence_scope") {
    function, arguments, context in
  let markDep = arguments.takeValue() as! MarkDependenceInst
  guard let dependence = LifetimeDependence(markDependence: markDep, context)
  else {
    print("Trivial Dependence")
    return
  }
  print(dependence)
  guard var range = dependence.scope.computeRange(context) else {
    print("Caller range")
    return
  }
  defer { range.deinitialize() }
  print(range)
}

let lifetimeDependenceRootTest = FunctionTest("lifetime_dependence_root") {
    function, arguments, context in
  let value = arguments.takeValue()
  _ = LifetimeDependence.visitDependenceRoots(enclosing: value, context) {
    scope in
    print("Scope: \(scope)")
    return .continueWalk
  }
}

private struct LifetimeDependenceUsePrinter : LifetimeDependenceDefUseWalker {
  let context: Context
  var visitedValues: ValueSet

  var function: Function
  
  init(function: Function, _ context: Context) {
    self.context = context
    self.function = function
    self.visitedValues = ValueSet(context)
  }
  
  mutating func deinitialize() {
    visitedValues.deinitialize()
  }

  mutating func needWalk(for value: Value) -> Bool {
    visitedValues.insert(value)
  }

  mutating func deadValue(_ value: Value, using operand: Operand?)
    -> WalkResult {
    print("Dead value: \(value)")
    return .continueWalk
  }

  mutating func leafUse(of operand: Operand) -> WalkResult {
    print("Leaf use: \(operand)")
    return .continueWalk
  }

  mutating func pointerEscapingUse(of operand: Operand) -> WalkResult {
    print("Pointer escaping use: \(operand)")
    return .continueWalk
  }
}

let lifetimeDependenceUseTest = FunctionTest("lifetime_dependence_use") {
    function, arguments, context in
  let value = arguments.takeValue()
  print("LifetimeDependence uses of: \(value)")
  var printer = LifetimeDependenceUsePrinter(function: function, context)
  defer { printer.deinitialize() }
  _ = printer.walkDown(root: value)
}

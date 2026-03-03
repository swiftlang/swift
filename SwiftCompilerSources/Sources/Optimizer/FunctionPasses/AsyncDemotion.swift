//===--- AsyncDemotion.swift -----------------------------------------------==//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2023 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

import AST
import SIL

private struct Stats: CustomStringConvertible {
  enum Kind {
    case asyncFns               // Functions analyzed.
    case demotionCandidateFn    // Functions that qualified for demotion.
    case demotionPerformedFn    // Functions that were demoted.

    case asyncCallsCount        // Calls analyzed.
    case asyncKnownCallsCount   // Calls to known async functions.
    case asyncCallsReplaced     // Calls replaced with call to sync demotion.
    case asyncCallExecutorMismatch  // Calls that could not be replaced due to mismatch.

    case hopCount               // Hop-to-executors analyzed.
    case hopsRemoved            // Hop-to-executors removed.
  }

  private var data: [Kind: Int] = [:]

  mutating func tick(_ k: Kind, _ increment: Int = 1) {
    data[k, default: 0] += increment
  }

  /// Produces a JSON description of all statistics
  var description: String {
    var s = "{\n"
    var entries = data.map { "  \"\($0.0)\":  \($0.1)" }
    entries.sort() // for consistency of output
    s += entries.joined(separator: ",\n")
    s += "\n}"
    return s
  }
}

fileprivate var stats: Stats = Stats()

enum Executor: CustomStringConvertible, Hashable {
  case selfActor                    // self argument of this function
  case nonisolated                  // the global, concurrent executor
  case globalActor(NominalTypeDecl) // a global actor of the given type
  case unknown(HashableValue)

  var description: String {
    switch self {
      case .selfActor: return "self"
      case .nonisolated: return "nonisolated"
      case let .globalActor(nom): return "globalActor(\(nom.name))"
      case let .unknown(value): return "unknown(\(value))"
    }
  }

  init(_ op: Operand) {
    self = .init(op.value)
  }

  init(_ val: Value) {
    var finder = FindExecutor()
    self = finder.search(val)
  }

  /// Same-function executor equality only.
  static func == (_ a: Executor, _ b: Executor) -> Bool {
    return same(duringCall: nil, from: a, to: b)
  }

  /// Returns true if the executors are the same.
  ///
  /// If `duringCall` is `nil`, then the executor comparison
  /// is assumed to be within the same function. Otherwise, the
  /// callsite for the transfer of control is used to do matching
  /// across functions.
  static func same(duringCall: ApplySite?,
                   from: Executor,
                   to: Executor) -> Bool {
    switch (from, to) {
      case (.nonisolated, .nonisolated): return true
      case let (.unknown(f), .unknown(t)): return f == t
      case (.selfActor, .selfActor):
        guard duringCall != nil else { return true }

        fatalError("implement selfActor comparison logic")

      case let (.globalActor(ty1), .globalActor(ty2)): return ty1 == ty2
      default: return false
    }
  }

  // Finds the executor corresponding to a hop instruction's operand.
  struct FindExecutor : ValueUseDefWalker {
    private var def: Executor?
    internal var walkUpCache = WalkerCache<SmallProjectionPath>()

    mutating func search(_ val: Value) -> Executor {
      // assert(val.type == $Optional<Builtin.Executor>)
      _ = walkUp(value: val, path: SmallProjectionPath(.anyValueFields))
      guard let result = def else {
        fatalError("could not find def for value:\n \(val)")
      }
      return result
    }

    internal mutating func rootDef(value: Value,
                                   path: SmallProjectionPath) -> WalkResult {
//      dump("FindExecutor.rootDef -> \(value)")

      if let arg = value as? FunctionArgument {
        def = arg.isSelf ? .selfActor : .unknown(value.hashable)

      } else if let enm = value as? EnumInst {
        assert(enm.payload == nil, "expected empty payload case for a def")
        def = .nonisolated

      } else if let nom = value.type.nominal {
        def = nom.isGlobalActor ? .globalActor(nom) : .unknown(value.hashable)

      } else {
        def = .unknown(value.hashable)
      }

      return .abortWalk
    }
  }
}

// A node represents a function in the demotion graph. It encapsulates all of
// the state of the function as it is analyzed and transformed.
private struct Node: CustomStringConvertible {
  enum DemotionStatus: Equatable, CustomStringConvertible {
    case prohibited(ProhibitionReason)         // demotion is never permitted
    case awaiting                              // demotion might be possible
    case demoted(Function)

    enum ProhibitionReason {
      case ambiguousExecutor
      case suspends
      case external
    }

    var isDemoted: Bool {
      switch self {
        case .demoted(_): return true
        default: return false
      }
    }

    var description: String {
      switch self {
        case let .prohibited(reason): return "prohibited(\(reason))"
        case .awaiting: return "awaiting"
        case let .demoted(fn): return "demoted(\(fn.name))"
      }
    }
  }

  let function: Function
  let executors: Set<Executor>
  private(set) var demotionState: DemotionStatus
  private(set) var asyncCalls: [ApplySite]
  private(set) var hops: [Instruction]

  init(_ analysis: AnalysisResult) {
    let data = analysis.data
    function = data.function
    executors = data.executors
    demotionState = analysis.demotionStatus
    asyncCalls = data.knownAsyncCalls
    hops = data.hops

    assert(executors.count <= 1 || demotionState != .awaiting,
           "could only be awaiting demotion if <1 executor")
  }

  var description: String {
    return  """
            DemotionStatus {
              function: \(function.name),
              demotionState: \(demotionState),
              executors: \(executors),
              asyncCalls: \(asyncCalls.map { $0.referencedFunction!.name.description })
            }
            """
  }

  var isDemoted: Bool { demotionState.isDemoted }

  // Effectively the out-edges of this node.
  // FIXME: incrementally update this as the async calls are rewritten
  func getUniqueCallees() -> Set<Function> {
    return .init(asyncCalls.map { $0.referencedFunction! })
  }

  /// Rewrites async calls into synchronous calls, if possible.
  ///
  /// Returns true iff this function had a call rewritten.
  mutating
  func rewriteCalls(to callee: Node) -> Bool {
    let oldEntry = callee.function
    guard case let .demoted(newEntry) = callee.demotionState else {
      fatalError("rewriting calls when callee is not demoted?")
    }

    // It is only safe to rewrite calls in this function of the target
    // if the executors are compatible.
    assert(callee.executors.count <= 1)
    guard executors.count <= 1 else {
      // TODO: once we have precise information about which call-sites will be
      // encountered while on a specific executor, we can be less conservative.
      return false
    }

    let before = asyncCalls.count
    assert(before > 0, "no async calls to rewrite?")

    asyncCalls.removeAll { site in
      let target = site.referencedFunction!
      guard target == oldEntry else { return false }

      // When converting a call from async-to-sync, we must ensure that prior to
      // entering the sync entry-point, that we're on an executor compatible with
      // the original async callee.
      switch (executors.first, callee.executors.first) {
        // If both require an executor, they must be the same
        case let (.some(from), .some(to)):
          if Executor.same(duringCall: site, from: from, to: to) {
            break
          } else {
            dump("executor mismatch:\n\tfrom:\(from)\n\tto:\(to)", .med)
            stats.tick(.asyncCallExecutorMismatch)
            return false
          }

        // ❌ The callee requires an executor, but as the caller we don't know
        // which executor we run on!
        case (.none, .some(_)):
          stats.tick(.asyncCallExecutorMismatch)
          return false

        // ✅ Callee has no preference.
        case (_, .none): break
      }

      dump("📝 \(function.name) | rewriting \(oldEntry.name) --> \(newEntry.name) @ \(site)")
      // FIXME: actually implement the rewriting


      return true
    }

    let callsReplaced = before - asyncCalls.count

    stats.tick(.asyncCallsReplaced, callsReplaced)
    return callsReplaced > 0
  }

  /// Attempts a simple, local demotion of the this function.
  ///
  /// Returns true if demotion was performed.
  mutating func tryDemotion(_ moduleContext: ModulePassContext) -> Bool {
    assert(!demotionState.isDemoted, "already demoted!")

    // Is it valid to demote this function?
    guard case .awaiting = demotionState else { return false }

    let uniqueCallees = getUniqueCallees()

    // We can demote if there are only self-recursive async calls.
    guard uniqueCallees.count <= 1 else { return false }

    // If there is a callee, then it must be ourself.
    // Otherwise we need non-local analysis.
    if let callee = uniqueCallees.first,
           callee != function {
      return false
    }

    /// Demotion involves a few steps:
    ///  1. Remove all `hop_to_executor`'s from the async body.
    ///
    ///  TODO: the below
    ///
    ///  - Create an empty non-async `Function` with an otherwise identical
    ///     signature.
    ///  - Transfer the body of the async function to the non-async function.
    ///
    ///  - Fill in the body of the async function with an executor hop followed
    ///     by a tail-call to the non-async function.
    ///  - Replace self-recursive async calls with non-async calls.

//    dump("------->  \(moduleContext.mangleAsyncRemoved(from: function))\n")

    moduleContext.transform(function: function) { context in
      while let hop = hops.popLast() {
        context.erase(instruction: hop)
        stats.tick(.hopsRemoved)
      }
    }

    // FIXME: temporary
    demotionState = .demoted(function)
    stats.tick(.demotionPerformedFn)
    dump("✨ successful demotion: \(self)")

    return true
  }
}

/// The DemotionGraph is a subgraph of the entire program's call-graph that
/// only deals with functions that are candidates of async demotion. Two
/// connected nodes of the graph represent a possible demotion chain.
private struct DemotionGraph: CustomStringConvertible {
  typealias StatusMap = [Function : Node]

  private let context: ModulePassContext

  // A map from function F to the demotion status of F.
  private var status: StatusMap = [:]

  // A map from function F to all functions that are known to call F.
  private var knownCallers: [Function: Set<Function>] = [:]

  // The set of demotable functions.
  private var demotable: Set<Function> = .init()

  init(_ context: ModulePassContext) { self.context = context}

  mutating func addNode(_ ds: Node) {
    let empty = Set<Function>()

    assert(status.index(forKey: ds.function) == nil,
           "adding function twice!")

    status[ds.function] = ds

    if case .awaiting = ds.demotionState {
      demotable.insert(ds.function)
    }

    // cache which functions 'fn' calls.
    for apply in ds.asyncCalls {
      let callee = apply.referencedFunction!
      knownCallers[callee, default: empty].insert(ds.function)
    }
  }

  /// Generates a GraphViz-compatible representation of the graph.
  func asGraphViz(_ title: String = "DemotionGraph") -> String {
    var nameMap: [String: String] = [:]
    var freshID: Int = 0
    func nameOf(_ fn: Function) -> String {
      let full = fn.name.string

      if let shortName = nameMap[full] {
        return shortName
      }

      let newShortName = "\(freshID)"
      freshID += 1

      nameMap[full] = newShortName
      return newShortName
    }

    func colorOf(_ fn: Function) -> String {
      switch status[fn]!.demotionState {
        case .prohibited(.external): return "black"
        case .prohibited(.suspends): return "red"
        case .prohibited(.ambiguousExecutor): return "hotpink"
        case .awaiting: return "darkviolet"
        case .demoted: return "green"
      }
    }

    var output = "\ndigraph \(title) {\n"
    for fn in status.keys {
      output += "  \(nameOf(fn)) [color=\"\(colorOf(fn))\"] // \(fn.name)\n"

      for callee in status[fn]!.getUniqueCallees() {
        output += "  \(nameOf(fn)) -> \(nameOf(callee))\n"
      }
    }
    output += "}\n"

    return output
  }

  var description : String {
    return "\(status.values)\n\n" + asGraphViz()
  }

  /// Entry-point into the main optimization process.
  mutating func optimize() {

    /** Do an initial chain-demotion pass over all demotable functions while
        building the SCC graph.

        This will clean-up the call-graph so that chains of non-SCC function
        calls are removed. The benefit is that a chain of demotable functions
        that leave an SCC will be removed. For example, if we have a graph like:
        ```
        digraph G {
          // SCC 1
          a -> b
          b -> c
          c -> a

          // SCC 2
          d -> d

          x -> a
          c -> d

          // edges that will be removed by the initial chain-demotion
          c -> y
          y -> z
        }
        ```
        Then this initial pass over the nodes will demote `z` and `y`, removing
        the edge between them. But more importantly, it will also remove the
        edge `c -> y`, which is an edge that leaves SCC 1!
    */
    for fn in demotable {
      guard !status[fn]!.isDemoted else {
        demotable.remove(fn)
        continue
      }

      if status[fn]!.tryDemotion(context) {
        demotable.remove(fn)
        rewriteCallers(of: fn)
        continue
      }
    }

    /// Demote SCC's until we no longer can make any demotions.
    ///
    /// Demotion an SCC is quite similar to demoting a single function. An SCC
    /// can be demoted iff:
    ///   1. There does not exist any edges leaving the SCC.
    ///   2. The executors of all functions in the SCC are mutually compatible.
    ///

    // TODO: implement

  }

  mutating func rewriteCallers(of demotedFn: Function) {
    /// Rewriting a function's async calls can cause a chain of
    /// demotions through the call graph. We can track our progress
    /// through the chain using a simple stack, relying on the removal
    /// of edges in `knownCallers` to ensure we don't loop forever.
    var worklist = [demotedFn]

    while let fn = worklist.popLast() {
      let state = status[fn]!

      assert(state.demotionState.isDemoted)

      guard let callers = knownCallers.removeValue(forKey: fn) else {
        continue  // no callers to rewrite
      }

      for caller in callers {
        let changed = status[caller]!.rewriteCalls(to: state)

        // Skip if we re-wrote the self-recursive calls.
        guard caller != demotedFn else { continue }

        // If rewriting changed the caller, chain a demotion attempt.
        if changed && status[caller]!.tryDemotion(context) {
          worklist.append(caller)
        }
      }
    }
  }


} // end struct

/// Embodies the result of analyzing a Function to determine whether
/// it is a valid candidate for async demotion. If it is not a valid
/// candidate, then the reason for rejection is indicated.
private enum AnalysisResult: CustomStringConvertible {
  typealias Data = (function: Function,
                    knownAsyncCalls: [ApplySite],
                    executors: Set<Executor>,
                    hops: [Instruction])

  case demotionCandidate(Data)
  case externalFunction(Data)
  case multipleExecutors(Data)
  case unknownAsyncOp(Data, Instruction) // an unanalyzed suspending op

  var data: Data {
    switch self {
      case let .demotionCandidate(data): return data
      case let .externalFunction(data):  return data
      case let .multipleExecutors(data): return data
      case let .unknownAsyncOp(data, _): return data
    }
  }

  var demotionStatus: Node.DemotionStatus {
    switch self {
      case .demotionCandidate: return .awaiting
      case .unknownAsyncOp: return .prohibited(.suspends)
      case .multipleExecutors: return .prohibited(.ambiguousExecutor)
      case .externalFunction: return .prohibited(.external)
    }
  }



  init(_ data: Data,
       _ unknownAsyncOp: Instruction?) {

      assert(data.knownAsyncCalls.allSatisfy {
              $0.callee.type.isAsyncFunction
              && $0.parentFunction == data.function })

      assert(data.hops.allSatisfy { $0.parentFunction == data.function })

      // Can't demote or analyze external async functions.
      if data.function.blocks.isEmpty {
        assert(data.function.isAsync)
        self = .externalFunction(data)

      // Demotion to non-async is only ever possible if the function hops to at
      // most one executor.
      } else if data.executors.count > 1 {
        self = .multipleExecutors(data)

      // Next, if there are any unknown async operations we also cannot demote.
      } else if let op = unknownAsyncOp {
        self = .unknownAsyncOp(data, op)

      } else {
        self = .demotionCandidate(data)
        stats.tick(.demotionCandidateFn)
      }
   }

  var description: String {
    switch self {
      case let .multipleExecutors(base):
        return "\(base.0.name) -- ❌ multiple executors \(base.2)"
      case let .unknownAsyncOp(base, inst):
        return "\(base.0.name) -- ❌ unknown async operation: \(inst) | \(base.2)"
      case let .externalFunction(base):
        return "\(base.0.name) -- ❌ external function"
      case let .demotionCandidate(base):
        return "\(base.0.name) -- ✅ demotion candidate \(base.2)"
    }
  }
}

extension Optional {
  mutating func setIfUnset(_ value: @autoclosure () -> Wrapped) {
    if case .none = self {
      self = .some(value())
    }
  }
}


/// Analyzes the kinds of possible suspension points and determines whether
/// the function is locally a candidate for async demotion.
///
/// Returns the initial demotion status for this function if it is async.
private func analyzeForDemotion(_ function: Function) -> Node? {
  // must be async
  guard function.isAsync else { return nil }

  stats.tick(.asyncFns)

  // We want to collect all of the ApplySites in this analysis,
  // even if the async function isn't a candidate for demotion itself,
  // because it may be a caller of a function that can be demoted.

  var executors = Set<Executor>()
  var knownAsyncApplySites: [ApplySite] = []
  var hops: [Instruction] = []
  var unknownAsyncOp: Instruction? = nil

  for inst in function.instructions {
    // Track hops in a low-resolution way.
    //
    // FIXME: would be better to associate each call-site with the executor
    // it would happen under, so demoted entry-points can be called from
    // multi-executor async functions.
    if let hop = inst as? HopToExecutorInst {
      executors.insert(Executor(hop.operand))
      hops.append(hop)
      stats.tick(.hopCount)

    } else if let apply = inst as? ApplySite {
      // Only record async call-sites.
      guard apply.isAsync else { continue }
      stats.tick(.asyncCallsCount)

      // Treat an unknown async callee as outside of our analysis
      if apply.referencedFunction == nil {
        unknownAsyncOp.setIfUnset(inst)
      } else {
        knownAsyncApplySites.append(apply)
        stats.tick(.asyncKnownCallsCount)
      }

    } else if inst.maySuspend {

//    assert(inst is GetAsyncContinuationInst ||
//           inst is GetAsyncContinuationAddrInst ||
//           inst is AwaitAsyncContinuationInst, "unexpected instruction")

      unknownAsyncOp.setIfUnset(inst)
    }
  }

  let data: AnalysisResult.Data =
    (function: function,
     knownAsyncCalls: knownAsyncApplySites,
     executors: executors,
     hops: hops)

  let result = AnalysisResult(data, unknownAsyncOp)
  dump(result)
  return Node(result)
}

/// An async demotion is a conversion of an async function like this:
///
///   ```
///   func f(_ args: Args) async -> Result { Body }
///   ```
///
/// into this:
///
///   ```
///   func f(_ args: Args) async -> Result {
///     hop_to_executor %E
///     return f_specialized(args)
///   }
///
///   func f_specialized(_ args: Args) async -> Result {
///     Body, but with all hop_to_executor instructions removed and
///     async calls replaced with calls to their demoted specializations.
///   }
///   ```
///
/// The demotion is valid only if an async function satisfies all of the
/// following requirements:
///   1. It hops to at most one kind of executor.
///   2. It does not perform a continuation capture.
///   3. It only makes async function calls to callees who themselves can be
///      async demoted, with respect to either the same executor or no executor.
///
/// Then the function can be async demoted.
///
/// Requirement 3 is the tricky part of this analysis and makes it
/// interprocedural. In particular, this pass handles cycles and other kinds of
/// mutually recursive function groups.
let asyncDemotion = ModulePass(name: "async-demotion") {
  (moduleContext: ModulePassContext) in

  dump("\n\n\n-------------------------------\n\n\n")
  defer { dump("\n\n\n-------------------------------\n\n\n") }

  var graph = DemotionGraph(moduleContext)

  for function in moduleContext.functions {
    // Perform local analysis of each function to generate initial conditions
    // of async demotion candidacy.
    guard let initialState = analyzeForDemotion(function) else { continue }

    // Using the local analysis, build the subgraph of the full call-graph
    // that we actually want to analyze. This subgraph only deals with
    // async functions that are possible candidates of demotion.
    graph.addNode(initialState)
  }

  dump("\n\n" + graph.asGraphViz("BeforeAsyncDemotion"),
       .high)

  graph.optimize()

  dump(stats, .high)

  dump("\n\n" + graph.asGraphViz("AfterAsyncDemotion"),
       .high)
}


enum LogPriority: UInt8 {
  case low  = 0
  case med  = 1
  case high = 2
}

let ENABLE_LOGGING: LogPriority? = nil

/// Debug logging
private func dump<T: CustomStringConvertible>(
  _ t: @autoclosure () -> T,
  _ level: LogPriority = .low) {

  guard let minLevel = ENABLE_LOGGING else { return }

  if level.rawValue >= minLevel.rawValue {
    print(t())
  }
}

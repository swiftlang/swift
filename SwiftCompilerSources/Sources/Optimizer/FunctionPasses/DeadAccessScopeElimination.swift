//===--- DeadAccessScopeElimination.swift ----------------------------------==//
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

/// Eliminates dead access scopes if they are not conflicting with other scopes.
///
/// Removes:
/// ```
///   %2 = begin_access [modify] [dynamic] %1
///   ...                                       // no uses of %2
///   end_access %2
/// ```
///
/// However, dead _conflicting_ access scopes are not removed.
/// If a conflicting scope becomes dead because an optimization e.g. removed a load, it is still
/// important to get an access violation at runtime.
/// Even a propagated value of a redundant load from a conflicting scope is undefined.
///
/// ```
///   %2 = begin_access [modify] [dynamic] %1
///   store %x to %2
///   %3 = begin_access [read] [dynamic] %1    // conflicting with %2!
///   %y = load %3
///   end_access %3
///   end_access %2
///   use(%y)
/// ```
/// After redundant-load-elimination:
/// ```
///   %2 = begin_access [modify] [dynamic] %1
///   store %x to %2
///   %3 = begin_access [read] [dynamic] %1    // now dead, but still conflicting with %2
///   end_access %3
///   end_access %2
///   use(%x)                                  // propagated from the store, but undefined here!
/// ```
/// In this case the scope `%3` is not removed because it's important to get an access violation
/// error at runtime before the undefined value `%x` is used.
///
/// This pass considers potential conflicting access scopes in called functions.
/// But it does not consider potential conflicting access in callers (because it can't!).
/// However, optimizations, like redundant-load-elimination, can only do such transformations if
/// the outer access scope is within the function, e.g.
///
/// ```
/// bb0(%0 : $*T):     // an inout from a conflicting scope in the caller
///   store %x to %0
///   %3 = begin_access [read] [dynamic] %1
///   %y = load %3     // cannot be propagated because it cannot be proved that %1 is the same address as %0
///   end_access %3
/// ```
///
/// All those checks are only done for dynamic access scopes, because they matter for runtime
/// exclusivity checking. Dead static scopes are removed unconditionally.
///
let deadAccessScopeElimination = FunctionPass(name: "dead-access-scope-elimination") {
  (function: Function, context: FunctionPassContext) in

  // Add all dead scopes here and then remove the ones which turn out to be conflicting.
  var removableScopes = SpecificIterableInstructionSet<BeginAccessInst>(context)
  defer { removableScopes.deinitialize() }

  var scopeTree = ScopeTree(context)

  // The payload is the recent access instruction at the block begin, e.g.
  // ```
  //   %1 = begin_acces %0
  //   br bb2
  // bb2:                  // recent access instruction at begin of bb2 is: `%1 = begin_acces %0`
  // ```
  // It's nil if the block is not within an access scope.
  //
  var blockWorklist = BasicBlockWorklistWithPayload<Instruction?>(context)
  defer { blockWorklist.deinitialize() }

  blockWorklist.pushIfNotVisited(function.entryBlock, with: nil)

  // Walk through the control flow in depth-first order. Note that we don't need to do any kind
  // of state merging at merge-points, because access scopes must be consistent on all paths.
  while let (block, recentAccessInstAtBlockBegin) = blockWorklist.pop() {

    // The last seen `begin_access` (or `end_access` in case of not perfectly nested scopes; see ScopeTree.backlinks)
    var recentAccessInst = recentAccessInstAtBlockBegin

    for inst in block.instructions {
      process(instruction: inst, updating: &recentAccessInst, &scopeTree, &removableScopes)
    }

    blockWorklist.pushIfNotVisited(contentsOf: block.successors, with: recentAccessInst)
  }

  for deadBeginAccess in removableScopes {
    context.erase(instructionIncludingAllUsers: deadBeginAccess)
  }
}

private func process(instruction: Instruction,
                     updating recentAccessInst: inout Instruction?,
                     _ scopeTree: inout ScopeTree,
                     _ removableScopes: inout SpecificIterableInstructionSet<BeginAccessInst>)
{
  switch instruction {

  case let beginAccess as BeginAccessInst:
    if beginAccess.isDead {
      // Might be removed again later if it turns out to be in a conflicting scope.
      removableScopes.insert(beginAccess)
    }
    if beginAccess.enforcement != .dynamic {
      // We unconditionally remove dead _static_ scopes, because they don't have any impact at runtime.
      // Usually static scopes are already removed in the optimization pipeline. However optimizations
      // might turn dynamic into static scopes. So let's handle them.
      break
    }
    scopeTree.visitEnclosingScopes(of: recentAccessInst) { enclosingBeginAccess in
      if beginAccess.accessKind.conflicts(with: enclosingBeginAccess.accessKind),
         // Avoid computing alias info if both scopes are not removable anyway.
         removableScopes.contains(beginAccess) || removableScopes.contains(enclosingBeginAccess),
         scopeTree.context.aliasAnalysis.mayAlias(beginAccess.address, enclosingBeginAccess.address)
      {
        // Conflicting enclosing scopes are not removable.
        removableScopes.erase(enclosingBeginAccess)
        // ... as well as the inner scope (which conflicts with the enclosing scope).
        removableScopes.erase(beginAccess)
      }
    }
    scopeTree.update(recent: &recentAccessInst, with: beginAccess)

  case let endAccess as EndAccessInst where endAccess.beginAccess.enforcement == .dynamic:
    scopeTree.update(recent: &recentAccessInst, with: endAccess)

  default:
    if instruction.mayCallFunction {
      // Check for potential conflicting scopes in called functions.
      scopeTree.visitEnclosingScopes(of: recentAccessInst) { enclosingBeginAccess in
        if removableScopes.contains(enclosingBeginAccess),
           instruction.mayHaveAccessScopeWhichConflicts(with: enclosingBeginAccess, scopeTree.context)
        {
          removableScopes.erase(enclosingBeginAccess)
        }
      }
    }
  }
}

/// Represents the tree of access scopes in a function.
/// Note that if the scopes are not nested perfectly, it's strictly speaking not a tree.
private struct ScopeTree {

  // Links `begin_access` and `end_access` instructions in backward control flow direction.
  // This is used to visit all enclosing scopes of a `begin_access`.
  // As an optimization, `end_access`es are ignored for scopes which are perfectly nested - which is
  // by far the most common case. In this case the backlinks simply are the parent links in the scope tree.
  //
  // Example of not perfectly nested scopes:
  // ```
  //   %1 = begin_access    <------------------+
  //   ...                                     |
  //   %2 = begin_access    <--------------+  -+
  //   ...                                 |
  //   end_access %1        <---------+   -+
  //   ...                            |
  //   %3 = begin_access    <-----+  -+
  //   ...                        |
  //   end_access %2        <-+  -+
  //   ...                    |
  //   end_access %3         -+
  // ```
  //
  // Perfectly nested scopes:
  // ```
  //   %1 = begin_access    <-+   <-+
  //   ...                    |     |
  //   %2 = begin_access     -+     |
  //   end_access %2                |    <- ignored
  //   ...                          |
  //   %3 = begin_access     -------+
  //   end_access %3                     <- ignored
  //   ...
  //   end_access %1                     <- ignored
  // ```
  private var backlinks = Dictionary<Instruction, Instruction>()

  let context: FunctionPassContext

  init(_ context: FunctionPassContext) { self.context = context }

  mutating func update(recent: inout Instruction?, with beginAccess: BeginAccessInst) {
    backlinks[beginAccess] = recent
    recent = beginAccess
  }

  mutating func update(recent: inout Instruction?, with endAccess: EndAccessInst) {
    if endAccess.beginAccess == recent {
      // The scope is perfectly nested. Ignore it and directly backlink to the parent of the `begin_access`
      recent = backlinks[endAccess.beginAccess]
    } else {
      backlinks[endAccess] = recent
      recent = endAccess
    }
  }

  func visitEnclosingScopes(of accessInstruction: Instruction?, closure: (BeginAccessInst) -> ()) {
    // Ignore scopes which are already closed
    var ignore = SpecificInstructionSet<BeginAccessInst>(context)
    defer { ignore.deinitialize() }

    var enclosingScope = accessInstruction

    while let parent = enclosingScope {
      switch parent {
      case let parentBeginAccess as BeginAccessInst where !ignore.contains(parentBeginAccess):
        closure(parentBeginAccess)
      case let parentEndAccess as EndAccessInst:
        // Seeing an `end_access` in the backlink chain means that this scope is already closed.
        ignore.insert(parentEndAccess.beginAccess)
      default:
        break
      }
      enclosingScope = backlinks[parent]
    }
  }
}

private extension Instruction {
  func mayHaveAccessScopeWhichConflicts(with beginAccess: BeginAccessInst, _ context: FunctionPassContext) -> Bool {
    if beginAccess.accessKind == .read {
      return mayWrite(toAddress: beginAccess.address, context.aliasAnalysis)
    } else {
      return mayReadOrWrite(address: beginAccess.address, context.aliasAnalysis)
    }
  }
}

private extension BeginAccessInst {
  var isDead: Bool { users.allSatisfy({ $0.isIncidentalUse }) }
}

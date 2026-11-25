//===--- ClosureGroupSchedule.h - Closure groups ----------------*- C++ -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2026 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
///
/// ClosureGroupSchedule partitions a module's function definitions into
/// closure groups, the units a SILClosureGroupTransform runs on,
/// and orders the groups callees first.
///
/// - Closure link: a function_ref from a function (the scope) to a closure
///   definition, a function SILGen marks when it creates it (isClosure). A link
///   doesn't depend on what the closure captures or on how the reference is
///   used. Thunks are excluded, so a shared thunk doesn't link unrelated
///   functions.
///
/// - Closure group: a connected component of (undirected) closure links: a
///   function, the closures it references, and every other function that
///   references them. So scopes that share a closure form one group. Members
///   of groups keep their bottom-up order.
///
/// A capture must never leave its group. Only a closure receives one (e.g. a
/// box or an @inout_aliasable address), and the function that passes it links
/// to that closure. So a pass that follows a capture into the callee must stay
/// inside the group. SILGen output satisfies this, and the passes assert it.
/// Hand-written SIL must mark such a callee [closure].
///
/// Groups run callees first, along the call graph that BottomUpFunctionOrder
/// follows: when a pass runs on a group, every function the group calls in
/// another group has already finished the block, as in the function pass
/// pipeline.
///
/// Groups on a call cycle have no callees-first order, so they run
/// in bottom-up order of their first members, and a pass or analysis that
/// reads a callee in another group must be conservative about recursion, also
/// as in the function pass pipeline.
///
/// In the following example, helper's group runs first. The groups {f, cf} and
/// {g, cg} call each other, so they run in bottom-up order, and {f, cf} runs
/// before its callee g:
///
///     func helper(_ body: () -> ()) { body() }
///     func f() { var x = 0; helper { x += 1; g() } }   // closure cf
///     func g() { var y = 0; helper { y += 1; f() } }   // closure cg
///
///     functions          groups, in schedule order
///
///     f ~~> cf           [helper]   [f cf] --> [g cg]
///     ^      |                        ^          |
///     |      v                        +----------+
///     cg <~~ g
///
///     ~~> closure link, from scope to closure; the other arrows are calls,
///     except the calls to helper, which aren't drawn.
///
//===----------------------------------------------------------------------===//

#ifndef SWIFT_SILOPTIMIZER_UTILS_CLOSUREGROUPSCHEDULE_H
#define SWIFT_SILOPTIMIZER_UTILS_CLOSUREGROUPSCHEDULE_H

#include "swift/Basic/Debug.h"
#include "llvm/ADT/ArrayRef.h"
#include "llvm/ADT/SmallVector.h"
#include "llvm/Support/Debug.h"
#include "llvm/Support/raw_ostream.h"

namespace swift {

class BasicCalleeAnalysis;
class SILFunction;
class SILModule;

class ClosureGroupSchedule {
public:
  /// One group's members, in bottom-up order.
  using Group = llvm::SmallVector<SILFunction *, 4>;

private:
  /// Groups in schedule order: a group's callees in other groups come earlier.
  llvm::SmallVector<Group, 16> groups;

public:
  ClosureGroupSchedule(SILModule &module, BasicCalleeAnalysis *bca);

  llvm::ArrayRef<Group> getGroups() const { return groups; }

  void print(llvm::raw_ostream &os) const;
  SWIFT_DEBUG_DUMP { print(llvm::dbgs()); }
};

} // end namespace swift

#endif

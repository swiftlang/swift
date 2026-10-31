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
/// closure groups, the units a SILClosureGroupTransform (Transforms.h) runs on,
/// and orders the groups callees first.
///
/// - Closure link: a function_ref or dynamic_function_ref from a function (the
///   scope) to a closure or local function definition, which SILGen marks by
///   its captures or which is declared in a local context. A link doesn't
///   depend on what the closure captures or on how the reference is used: an
///   escaping closure, a closure that captures a box or a value, and one that
///   captures nothing all get links. Thunks are excluded, so a shared thunk
///   doesn't link unrelated functions.
///
/// - Capture edge (ClosureScope.h): an apply site that passes its callee a box
///   or an @inout_aliasable address. Group passes follow a capture into another
///   function only along a capture edge.
///
/// - Closure component: a connected component of (undirected) closure links
///   and capture edges: a function, the closures and local functions it
///   references, the functions it passes captures to, and every other
///   function that references or passes captures to them. So scopes that
///   share a closure form one component, and so do the functions that pass
///   captures to a shared thunk.
///
/// - Closure group: an SCC of the call graph that BottomUpFunctionOrder
///   follows, with each closure component contracted to one node. Group members
///   keep their bottom-up order. In this example, no function is on a call
///   cycle, but the components {f, cf} and {g, cg} call each other, so all
///   four functions form one group:
///
///     func f() { var x = 0; helper { x += 1; g() } }   // closure cf
///     func g() { var y = 0; helper { y += 1; f() } }   // closure cg
///
///     functions          components            closure group
///
///     f ~~> cf           [f cf] --> [g cg]     [f cf g cg]
///     ^      |             ^          |
///     |      v             +----------+
///     cg <~~ g
///
///     ~~> closure link, from scope to closure; the other arrows are calls.
///
//===----------------------------------------------------------------------===//

#ifndef SWIFT_SILOPTIMIZER_UTILS_CLOSUREGROUPSCHEDULE_H
#define SWIFT_SILOPTIMIZER_UTILS_CLOSUREGROUPSCHEDULE_H

#include "llvm/ADT/ArrayRef.h"
#include "llvm/ADT/SmallVector.h"
#include "llvm/Support/raw_ostream.h"

namespace swift {

class BasicCalleeAnalysis;
class SILFunction;
class SILModule;

class ClosureGroupSchedule {
public:
  /// Members of one group, bottom-up order.
  using Group = llvm::SmallVector<SILFunction *, 4>;

private:
  /// For `print()`: the leader of each closure component, in the
  /// order members appear in getGroups().
  llvm::SmallVector<SILFunction *, 32> memberLeaders;

  /// Groups in schedule order: a group's callees in other groups come earlier.
  llvm::SmallVector<Group, 16> groups;

public:
  ClosureGroupSchedule(SILModule &module, BasicCalleeAnalysis *bca);

  llvm::ArrayRef<Group> getGroups() const { return groups; }

  void print(llvm::raw_ostream &os) const;
};

} // end namespace swift

#endif

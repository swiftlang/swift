//===--- ArrayOpt.h ---------------------------------------------*- C++ -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2019 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
///
/// Array optimization utilities.
///
//===----------------------------------------------------------------------===//

#include "swift/SIL/InstructionUtils.h"
#include "swift/SIL/Projection.h"
#include "swift/SIL/SILInstruction.h"
#include "llvm/ADT/SmallPtrSet.h"

namespace swift {

/// Collect all uses of a struct given an aggregate value that contains the
/// struct and access path describing the projection of the aggregate
/// that accesses the struct.
///
/// AggregateAddressUsers records uses of the aggregate value's address. These
/// may indirectly access the struct's elements.
///
/// Projections over the aggregate that do not access the struct are ignored.
///
/// StructLoads records loads of the struct value.
/// StructAddressUsers records other uses of the struct address.
/// StructValueUsers records direct uses of the loaded struct.
///
/// Projections of the struct over its elements are all similarly recorded in
/// ElementAddressUsers, ElementLoads, and ElementValueUsers.
///
/// bb0(%arg : $*S)
/// apply %f(%arg)           // <--- Aggregate Address User
/// %struct_addr = struct_element_addr %arg : $*S, #S.element
/// apply %g(%struct_addr)   // <--- Struct Address User
/// %val = load %struct_addr // <--- Struct Load
/// apply %h(%val)           // <--- Struct Value User
/// %elt_addr = struct_element_addr %struct_addr : $*A, #A.element
/// apply %i(%elt_addr)      // <--- Element Address User
/// %elt = load %elt_addr    // <--- Element Load
/// apply %j(%elt)           // <--- Element Value User
class StructUseCollector {
public:
  typedef SmallPtrSet<Operand*, 16> VisitedSet;
  typedef SmallVector<SILInstruction*, 16> UserList;

  /// Record the users of a value or an element within that value along with the
  /// operand that directly uses the value. Multiple levels of struct_extract
  /// may exist between the operand and the user instruction.
  typedef SmallVector<std::pair<SILInstruction*, Operand*>, 16> UserOperList;

  /// \return a sequence of integers representing the access path of this
  /// element within a Struct/Ref/Tuple.
  ///
  /// Do not form a path with an IndexAddrInst because we have no way to
  /// distinguish between indexing and subelement access. The same index could
  /// either refer to the next element (indexed) or a subelement.
  static SILValue getAccessPath(SILValue V, SmallVectorImpl<unsigned>& Path) {
    V = stripCasts(V);
    if (auto *IA = dyn_cast<IndexAddrInst>(V)) {
      // Don't include index_addr projections in the access path. We could if
      // the index is constant. For simplicity we just ignore them.
      V = stripCasts(IA->getBase());
    }
    ProjectionIndex PI(V);
    if (!PI.isValid())
      return V;
    
    SILValue UnderlyingObject = getAccessPath(PI.Aggregate, Path);
    Path.push_back(PI.Index);
    return UnderlyingObject;
  }

  UserList AggregateAddressUsers;
  UserList StructAddressUsers;
  SmallVector<LoadInst*, 16> StructLoads;
  UserList StructValueUsers;
  UserOperList ElementAddressUsers;
  SmallVector<std::pair<LoadInst*, Operand*>, 16> ElementLoads;
  UserOperList ElementValueUsers;
  VisitedSet Visited;

  /// Collect all uses of the value at the given address.
  void collectUses(ValueBase *V, ArrayRef<unsigned> AccessPath) {
    // Save our old indent and increment.
    // Collect all users of the address and loads.
    collectAddressUses(V, AccessPath, nullptr);

    // Collect all uses of the Struct value.
    for (auto *DefInst : StructLoads) {
      for (auto *DefUI : DefInst->getUses()) {
        if (!Visited.insert(&*DefUI).second) {
          continue;
        }

        StructValueUsers.push_back(DefUI->getUser());
      }
    }

    // Collect all users of element values.
    for (auto &Pair : ElementLoads) {
      for (auto *DefUI : Pair.first->getUses()) {
        if (!Visited.insert(&*DefUI).second) {
          continue;
        }

        ElementValueUsers.push_back(
            std::make_pair(DefUI->getUser(), Pair.second));
      }
    }
  }

  /// Returns true if there is a single address user of the value.
  bool hasSingleAddressUse(SILInstruction *SingleAddressUser) {
    if (!AggregateAddressUsers.empty())
      return false;
    if (!ElementAddressUsers.empty())
      return false;
    if (StructAddressUsers.size() != 1)
      return false;
    return StructAddressUsers[0] == SingleAddressUser;
  }

protected:

  static bool definesSingleObjectType(ValueBase *V) {
    return V->getType().isObject();
  }

  /// If AccessPathSuffix is non-empty, then the value is the address of an
  /// aggregate containing the Struct. If AccessPathSuffix is empty and
  /// StructVal is invalid, then the value is the address of the Struct. If
  /// StructVal is valid, the value is the address of an element within the
  /// Struct.
  void collectAddressUses(ValueBase *V, ArrayRef<unsigned> AccessPathSuffix,
                          Operand *StructVal) {
    for (auto *UI : V->getUses()) {
      // Keep the operand, not the instruction in the visited set. The same
      // instruction may theoretically have different types of uses.
      if (!Visited.insert(&*UI).second) {
        continue;
      }

      SILInstruction *UseInst = UI->getUser();

      if (UseInst->isDebugInstruction())
        continue;

      if (StructVal) {
        // Found a use of an element.
        assert(AccessPathSuffix.empty() && "should have accessed struct");
        if (auto *LoadI = dyn_cast<LoadInst>(UseInst)) {
          ElementLoads.push_back(std::make_pair(LoadI, StructVal));
          continue;
        }

        if (auto proj = dyn_cast<StructElementAddrInst>(UseInst)) {
          collectAddressUses(proj, AccessPathSuffix, StructVal);
          continue;
        }

        ElementAddressUsers.push_back(std::make_pair(UseInst,StructVal));
        continue;
      }

      if (isa<UncheckedRefCastInst>(UseInst) || isa<IndexAddrInst>(UseInst)) {
        // Skip over unchecked_ref_cast and index_addr.
        collectAddressUses(cast<SingleValueInstruction>(UseInst),
                           AccessPathSuffix, nullptr);
        continue;
      }

      if (AccessPathSuffix.empty()) {
        // Found a use of the struct at the given access path.
        if (auto *LoadI = dyn_cast<LoadInst>(UseInst)) {
          StructLoads.push_back(LoadI);
          continue;
        }

        if (auto proj = dyn_cast<StructElementAddrInst>(UseInst)) {
          collectAddressUses(proj, AccessPathSuffix, &*UI);
          continue;
        }

        // Value users - this happens if we start with a value object in V.
        if (definesSingleObjectType(V)) {
          StructValueUsers.push_back(UseInst);
          continue;
        }

        StructAddressUsers.push_back(UseInst);
        continue;
      }

      // Check for uses of projections.

      // These are all single-value instructions.
      auto *ProjInst = dyn_cast<SingleValueInstruction>(UseInst);
      if (!ProjInst) {
        AggregateAddressUsers.push_back(UseInst);
        continue;
      }
      ProjectionIndex PI(ProjInst);
      // Do not form a path from an IndexAddrInst without otherwise
      // distinguishing it from subelement addressing.
      if (!PI.isValid()) {
        // Found a use of an aggregate containing the given element.
        AggregateAddressUsers.push_back(UseInst);
        continue;
      }

      if (PI.Index != AccessPathSuffix[0]) {
        // Ignore uses of disjoint elements.
        continue;
      }

      // An alloc_box returns its address as the second value.
      assert(PI.Aggregate && "Expected unary element addr inst.");

      // Recursively check for users after stripping this component from the
      // access path.
      collectAddressUses(ProjInst, AccessPathSuffix.slice(1), nullptr);
    }
  }
};
} // namespace swift

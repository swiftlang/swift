//===--- Projection.cpp ---------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2015 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See http://swift.org/LICENSE.txt for license information
// See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#define DEBUG_TYPE "sil-projection"
#include "swift/SIL/Projection.h"
#include "llvm/ADT/None.h"
#include "llvm/Support/Debug.h"

using namespace swift;

//===----------------------------------------------------------------------===//
//                                 Projection
//===----------------------------------------------------------------------===//

/// Returns true if we are accessing different fields.
static bool areProjectionsToDifferentFields(const Projection &P1,
                                            const Projection &P2) {
  // If operands have the same type and we are accessing different fields,
  // returns true. Operand's type is not saved in Projection. Instead we check
  // Decl's context.
  return P1.getDecl() && P2.getDecl() &&
         P1.getDecl()->getDeclContext() == P2.getDecl()->getDeclContext() &&
         P1 != P2;
}

//===----------------------------------------------------------------------===//
//                              Projection Path
//===----------------------------------------------------------------------===//

Optional<ProjectionPath>
ProjectionPath::getAddrProjectionPath(SILValue Start, SILValue End,
                                      bool IgnoreCasts) {
  // Do not inspect the body of structs with unreferenced types such as
  // bitfields and unions.
  if (Start.getType().aggregateHasUnreferenceableStorage() ||
      End.getType().aggregateHasUnreferenceableStorage()) {
    return llvm::NoneType::None;
  }

  ProjectionPath P;

  // If Start == End, there is a "trivial" address projection in between the
  // two. This is represented by returning an empty ProjectionPath.
  if (Start == End)
    return std::move(P);

  // Otherwise see if End can be projection extracted from Start. First see if
  // End is a projection at all.
  auto Iter = End;
  if (IgnoreCasts)
    Iter = Iter.stripCasts();
  while (Projection::isAddressProjection(Iter) && Start != Iter) {
    if (auto *SEA = dyn_cast<StructElementAddrInst>(Iter))
      P.Path.push_back(Projection(SEA));
    else if (auto *TEA = dyn_cast<TupleElementAddrInst>(Iter))
      P.Path.push_back(Projection(TEA));
    else if (auto *REA = dyn_cast<RefElementAddrInst>(Iter))
      P.Path.push_back(Projection(REA));
    else
      P.Path.push_back(Projection(cast<UncheckedTakeEnumDataAddrInst>(Iter)));
    Iter = cast<SILInstruction>(*Iter).getOperand(0);
    if (IgnoreCasts)
      Iter = Iter.stripCasts();
  }

  // Return None if we have an empty projection list or if Start == Iter.
  if (P.empty() || Start != Iter)
    return llvm::NoneType::None;

  // Otherwise, return P.
  return std::move(P);
}

/// Returns true if the two paths have a non-empty symmetric difference.
///
/// This means that the two objects have the same base but access different
/// fields of the base object.
bool
ProjectionPath::
hasNonEmptySymmetricDifference(const ProjectionPath &RHS) const {
  // If either the LHS or RHS is empty, there is no common base class. Return
  // false.
  if (empty() || RHS.empty())
    return false;

  // We reverse the projection path to scan from the common object.
  auto LHSReverseIter = Path.rbegin();
  auto RHSReverseIter = RHS.Path.rbegin();

  // For each index i until min path size...
  for (unsigned i = 0, e = std::min(size(), RHS.size()); i != e; ++i) {
    // Grab the current projections.
    const Projection &LHSProj = *LHSReverseIter;
    const Projection &RHSProj = *RHSReverseIter;

    // If we are accessing different fields of a common object, return
    // false. The two projection paths must have a non-empty symmetric
    // difference.
    if (areProjectionsToDifferentFields(LHSProj, RHSProj)) {
      DEBUG(llvm::dbgs() << "        Path different at index: " << i << '\n');
      return true;
    }

    // Otherwise, if the two projections equal exactly, they have no symmetric
    // difference.
    if (LHSProj == RHSProj)
      return false;

    // Continue if we are accessing the same field.
    LHSReverseIter++;
    RHSReverseIter++;
  }

  // We checked
  return false;
}

/// TODO: Integrate has empty non-symmetric difference into here.
SubSeqRelation_t
ProjectionPath::
computeSubSeqRelation(const ProjectionPath &RHS) const {
  // If either path is empty, we can not prove anything, return Unrelated.
  if (empty() || RHS.empty())
    return SubSeqRelation_t::Unrelated;

  // We reverse the projection path to scan from the common object.
  auto LHSReverseIter = rbegin();
  auto RHSReverseIter = RHS.rbegin();

  unsigned MinPathSize = std::min(size(), RHS.size());

  // For each index i until min path size...
  for (unsigned i = 0; i != MinPathSize; ++i) {
    // Grab the current projections.
    const Projection &LHSProj = *LHSReverseIter;
    const Projection &RHSProj = *RHSReverseIter;

    // If the two projections do not equal exactly, return Unrelated.
    //
    // TODO: If Index equals zero, then we know that the two lists have nothing
    // in common and should return unrelated. If Index is greater than zero,
    // then we know that the two projection paths have a common base but a
    // non-empty symmetric difference. For now we just return Unrelated since I
    // can not remember why I had the special check in the
    // hasNonEmptySymmetricDifference code.
    if (LHSProj != RHSProj)
      return SubSeqRelation_t::Unrelated;

    // Otherwise increment reverse iterators.
    LHSReverseIter++;
    RHSReverseIter++;
  }

  // Ok, we now know that one of the paths is a subsequence of the other. If
  // both size() and RHS.size() equal then we know that the entire sequences
  // equal.
  if (size() == RHS.size())
    return SubSeqRelation_t::Equal;

  // If MinPathSize == size(), then we know that LHS is a strict subsequence of
  // RHS.
  if (MinPathSize == size())
    return SubSeqRelation_t::LHSStrictSubSeqOfRHS;

  // Otherwise, we know that MinPathSize must be RHS.size() and RHS must be a
  // strict subsequence of LHS. Assert to check this and return.
  assert(MinPathSize == RHS.size() &&
        "Since LHS and RHS don't equal and size() != MinPathSize, RHS.size() "
         "must equal MinPathSize");
  return SubSeqRelation_t::RHSStrictSubSeqOfLHS;
}

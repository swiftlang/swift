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
#include "swift/SIL/SILBuilder.h"
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
  if (!P1.isNominalKind() || !P2.isNominalKind())
    return false;

  return P1.getDecl()->getDeclContext() == P2.getDecl()->getDeclContext() &&
         P1 != P2;
}

bool Projection::matchesValueProjection(SILInstruction *I) const {
  llvm::Optional<Projection> P = Projection::valueProjectionForInstruction(I);
  if (!P)
    return false;
  return *this == P.getValue();
}

llvm::Optional<Projection>
Projection::valueProjectionForInstruction(SILInstruction *I) {
  switch (I->getKind()) {
  case ValueKind::StructExtractInst:
    assert(isValueProjection(I) && "isValueProjection out of sync");
    return Projection(cast<StructExtractInst>(I));
  case ValueKind::TupleExtractInst:
    assert(isValueProjection(I) && "isValueProjection out of sync");
    return Projection(cast<TupleExtractInst>(I));
  case ValueKind::UncheckedEnumDataInst:
    assert(isValueProjection(I) && "isValueProjection out of sync");
    return Projection(cast<UncheckedEnumDataInst>(I));
  default:
    assert(!isValueProjection(I) && "isValueProjection out of sync");
    return llvm::NoneType::None;
  }
}

llvm::Optional<Projection>
Projection::addressProjectionForInstruction(SILInstruction *I) {
  switch (I->getKind()) {
  case ValueKind::StructElementAddrInst:
    assert(isAddrProjection(I) && "isAddrProjection out of sync");
    return Projection(cast<StructElementAddrInst>(I));
  case ValueKind::TupleElementAddrInst:
    assert(isAddrProjection(I) && "isAddrProjection out of sync");
    return Projection(cast<TupleElementAddrInst>(I));
  case ValueKind::RefElementAddrInst:
    assert(isAddrProjection(I) && "isAddrProjection out of sync");
    return Projection(cast<RefElementAddrInst>(I));
  case ValueKind::UncheckedTakeEnumDataAddrInst:
    assert(isAddrProjection(I) && "isAddrProjection out of sync");
    return Projection(cast<UncheckedTakeEnumDataAddrInst>(I));
  default:
    assert(!isAddrProjection(I) && "isAddrProjection out of sync");
    return llvm::NoneType::None;
  }
}

llvm::Optional<Projection>
Projection::projectionForInstruction(SILInstruction *I) {
  if (auto P = addressProjectionForInstruction(I))
    return P;
  return valueProjectionForInstruction(I);
}

bool
Projection::operator==(const Projection &Other) const {
  if (isNominalKind() && Other.isNominalKind()) {
    return Other.getDecl() == Decl;
  } else {
    return !Other.isNominalKind() && Index == Other.getIndex();
  }
}

bool
Projection::operator<(Projection Other) const {
  // If we have a nominal kind...
  if (isNominalKind()) {
    // And Other is also nominal...
    if (Other.isNominalKind()) {
      // Just compare the value decl pointers.
      return getDeclIndex() < Other.getDeclIndex();
    }

    // Otherwise if Other is not nominal, return true since we always sort
    // decls before indices.
    return true;
  } else {
    // If this is not a nominal kind and Other is nominal, return
    // false. Nominal kinds are always sorted before non-nominal kinds.
    if (Other.isNominalKind())
      return false;

    // Otherwise, we are both index projections. Compare the indices.
    return getIndex() < Other.getIndex();
  }
}

static unsigned getIndexForValueDecl(ValueDecl *Decl) {
  NominalTypeDecl *D = cast<NominalTypeDecl>(Decl->getDeclContext());

  unsigned i = 0;
  for (auto *V : D->getStoredProperties()) {
    if (V == Decl)
      return i;
    ++i;
  }

  llvm_unreachable("Failed to find Decl in its decl context?!");
}

Projection::Projection(StructElementAddrInst *SEA)
  : Kind(ProjectionKind::Struct), Type(SEA->getType()),
    Decl(SEA->getField()), Index(getIndexForValueDecl(Decl)) {}

Projection::Projection(TupleElementAddrInst *TEA)
  : Kind(ProjectionKind::Tuple), Type(TEA->getType()),
    Decl(nullptr), Index(TEA->getFieldNo()) {}

Projection::Projection(RefElementAddrInst *REA)
  : Kind(ProjectionKind::Class), Type(REA->getType()),
    Decl(REA->getField()), Index(getIndexForValueDecl(Decl)) {}

/// UncheckedTakeEnumDataAddrInst always have an index of 0 since enums only
/// have one payload.
Projection::Projection(UncheckedTakeEnumDataAddrInst *UTEDAI)
  : Kind(ProjectionKind::Enum), Type(UTEDAI->getType()),
    Decl(UTEDAI->getElement()), Index(0) {}

Projection::Projection(StructExtractInst *SEI)
  : Kind(ProjectionKind::Struct), Type(SEI->getType()),
    Decl(SEI->getField()), Index(getIndexForValueDecl(Decl)) {}

Projection::Projection(TupleExtractInst *TEI)
  : Kind(ProjectionKind::Tuple), Type(TEI->getType()),
    Decl(nullptr), Index(TEI->getFieldNo()) {}

/// UncheckedEnumData always have an index of 0 since enums only have one
/// payload.
Projection::Projection(UncheckedEnumDataInst *UEDAI)
  : Kind(ProjectionKind::Enum), Type(UEDAI->getType()),
    Decl(UEDAI->getElement()), Index(0) {}

NullablePtr<SILInstruction>
Projection::
createValueProjection(SILBuilder &B, SILLocation Loc, SILValue Base) const {
  // Grab Base's type.
  SILType BaseTy = Base.getType();

  // If BaseTy is not an object type, bail.
  if (!BaseTy.isObject())
    return nullptr;

  // If this projection is associated with an address type, convert its type to
  // an object type.
  //
  // We explicitly do not convert Type to be an object if it is a local storage
  // type since we want it to fail.
  SILType Ty = Type.isAddress()? Type.getObjectType() : Type;

  if (!Ty.isObject())
    return nullptr;

  // Ok, we now know that the type of Base and the type represented by the base
  // of this projection match and that this projection can be represented as
  // value. Create the instruction if we can. Otherwise, return nullptr.
  switch (getKind()) {
  case ProjectionKind::Struct:
    return B.createStructExtract(Loc, Base, cast<VarDecl>(getDecl()));
  case ProjectionKind::Tuple:
    return B.createTupleExtract(Loc, Base, getIndex());
  case ProjectionKind::Enum:
    return B.createUncheckedEnumData(Loc, Base,
                                     cast<EnumElementDecl>(getDecl()));
  case ProjectionKind::Class:
    return nullptr;
  }
}

NullablePtr<SILInstruction>
Projection::
createAddrProjection(SILBuilder &B, SILLocation Loc, SILValue Base) const {
  // Grab Base's type.
  SILType BaseTy = Base.getType();

  // If BaseTy is not an address type, bail.
  if (!BaseTy.isAddress())
    return nullptr;

  // If this projection is associated with an object type, convert its type to
  // an address type.
  //
  // *NOTE* We purposely do not handle local storage types here since we want to
  // always fail in such a case. That is handled by checking that Ty is an
  // address.
  SILType Ty = Type.isObject()? Type.getAddressType() : Type;

  if (!Ty.isAddress())
    return nullptr;

  // Ok, we now know that the type of Base and the type represented by the base
  // of this projection match and that this projection can be represented as
  // value. Create the instruction if we can. Otherwise, return nullptr.
  switch (getKind()) {
  case ProjectionKind::Struct:
    return B.createStructElementAddr(Loc, Base, cast<VarDecl>(getDecl()));
  case ProjectionKind::Tuple:
    return B.createTupleElementAddr(Loc, Base, getIndex());
  case ProjectionKind::Enum:
    return B.createUncheckedTakeEnumDataAddr(Loc, Base,
                                             cast<EnumElementDecl>(getDecl()));
  case ProjectionKind::Class:
    return B.createRefElementAddr(Loc, Base, cast<VarDecl>(getDecl()));
  }
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
  while (Projection::isAddrProjection(Iter) && Start != Iter) {
    Projection AP = *Projection::addressProjectionForValue(Iter);
    P.Path.push_back(AP);

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

bool ProjectionPath::
findMatchingValueProjectionPaths(SILInstruction *I,
                                 SmallVectorImpl<SILInstruction *> &T) const {
  // We maintain the head of our worklist so we can use our worklist as a queue
  // and work in breadth first order. This makes sense since we want to process
  // in levels so we can maintain one tail list and delete the tail list when we
  // move to the next level.
  unsigned WorkListHead = 0;
  llvm::SmallVector<SILInstruction *, 8> WorkList;
  WorkList.push_back(I);

  // Start at the root of the list.
  for (auto PI = rbegin(), PE = rend(); PI != PE; ++PI) {
    // When we start a new level, clear T.
    T.clear();

    // If we have an empty worklist, return false. We have been unable to
    // complete the list.
    unsigned WorkListSize = WorkList.size();
    if (WorkListHead == WorkListSize)
      return false;

    // Otherwise, process each instruction in the worklist.
    for (; WorkListHead != WorkListSize; WorkListHead++) {
      SILInstruction *Ext = WorkList[WorkListHead];

      // If the current projection does not match I, continue and process the
      // next instruction.
      if (!PI->matchesValueProjection(Ext)) {
        continue;
      }

      // Otherwise, we know that Ext matched this projection path and we should
      // visit all of its uses and add Ext itself to our tail list.
      T.push_back(Ext);
      for (auto *Op : Ext->getUses()) {
        WorkList.push_back(Op->getUser());
      }
    }

    // Reset the worklist size.
    WorkListSize = WorkList.size();
  }

  return true;
}

Optional<ProjectionPath>
ProjectionPath::subtractPaths(const ProjectionPath &LHS, const ProjectionPath &RHS) {
  // If RHS is greater than or equal to LHS in size, RHS can not be a prefix of
  // LHS. Return None.
  unsigned RHSSize = RHS.size();
  unsigned LHSSize = LHS.size();
  if (RHSSize >= LHSSize)
    return llvm::NoneType::None;

  // First make sure that the prefix matches.
  Optional<ProjectionPath> P = ProjectionPath();
  for (unsigned i = 0; i < RHSSize; i++) {
    if (LHS.Path[i] != RHS.Path[i]) {
      P.reset();
      return P;
    }
  }

  // Add the rest of LHS to P and return P.
  for (unsigned i = RHSSize, e = LHSSize; i != e; ++i) {
    P->Path.push_back(LHS.Path[i]);
  }

  return P;
}

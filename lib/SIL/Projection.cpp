//===--- Projection.cpp ---------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2017 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#define DEBUG_TYPE "sil-projection"
#include "swift/SIL/Projection.h"
#include "swift/Basic/NullablePtr.h"
#include "swift/SIL/SILBuilder.h"
#include "swift/SIL/InstructionUtils.h"
#include "swift/SIL/DebugUtils.h"
#include "swift/SIL/SILUndef.h"
#include "llvm/ADT/None.h"
#include "llvm/Support/Debug.h"

using namespace swift;

//===----------------------------------------------------------------------===//
//                         Projection Static Asserts
//===----------------------------------------------------------------------===//

/// These are just for performance and verification. If one needs to make
/// changes that cause the asserts the fire, please update them. The purpose is
/// to prevent these predicates from changing values by mistake.
static_assert(std::is_standard_layout<Projection>::value,
              "Expected projection to be a standard layout type");


//===----------------------------------------------------------------------===//
//                              Utility 
//===----------------------------------------------------------------------===//

/// Extract an integer index from a SILValue.
///
/// Return true if IndexVal is a constant index representable as unsigned
/// int. We do not support symbolic projections yet, only 32-bit unsigned
/// integers.
bool swift::getIntegerIndex(SILValue IndexVal, unsigned &IndexConst) {
  if (auto *IndexLiteral = dyn_cast<IntegerLiteralInst>(IndexVal)) {
    APInt ConstInt = IndexLiteral->getValue();
    // IntegerLiterals are signed.
    if (ConstInt.isIntN(32) && ConstInt.isNonNegative()) {
      IndexConst = (unsigned)ConstInt.getSExtValue();
      return true;
    }    
  }
  return false;
}

//===----------------------------------------------------------------------===//
//                               Projection
//===----------------------------------------------------------------------===//

Projection::Projection(SILInstruction *I) : Value() {
  if (!I)
    return;
  /// Initialize given the specific instruction type and verify with asserts
  /// that we constructed it correctly.
  switch (I->getKind()) {
  // If we do not support this instruction kind, then just bail. Index will
  // be None so the Projection will be invalid.
  default:
    return;
  case ValueKind::StructElementAddrInst: {
    auto *SEAI = cast<StructElementAddrInst>(I);
    Value = ValueTy(ProjectionKind::Struct, SEAI->getFieldNo());
    assert(getKind() == ProjectionKind::Struct);
    assert(getIndex() == SEAI->getFieldNo());
    assert(getType(SEAI->getOperand()->getType(), SEAI->getModule()) ==
           SEAI->getType());
    break;
  }
  case ValueKind::StructExtractInst: {
    auto *SEI = cast<StructExtractInst>(I);
    Value = ValueTy(ProjectionKind::Struct, SEI->getFieldNo());
    assert(getKind() == ProjectionKind::Struct);
    assert(getIndex() == SEI->getFieldNo());
    assert(getType(SEI->getOperand()->getType(), SEI->getModule()) ==
           SEI->getType());
    break;
  }
  case ValueKind::RefElementAddrInst: {
    auto *REAI = cast<RefElementAddrInst>(I);
    Value = ValueTy(ProjectionKind::Class, REAI->getFieldNo());
    assert(getKind() == ProjectionKind::Class);
    assert(getIndex() == REAI->getFieldNo());
    assert(getType(REAI->getOperand()->getType(), REAI->getModule()) ==
           REAI->getType());
    break;
  }
  case ValueKind::RefTailAddrInst: {
    auto *RTAI = cast<RefTailAddrInst>(I);
    auto *Ty = RTAI->getTailType().getSwiftRValueType().getPointer();
    Value = ValueTy(ProjectionKind::TailElems, Ty);
    assert(getKind() == ProjectionKind::TailElems);
    break;
  }
  case ValueKind::ProjectBoxInst: {
    auto *PBI = cast<ProjectBoxInst>(I);
    Value = ValueTy(ProjectionKind::Box, static_cast<uintptr_t>(0));
    assert(getKind() == ProjectionKind::Box);
    assert(getIndex() == 0);
    assert(getType(PBI->getOperand()->getType(), PBI->getModule()) ==
           PBI->getType());
    (void) PBI;
    break;
  }
  case ValueKind::TupleExtractInst: {
    auto *TEI = cast<TupleExtractInst>(I);
    Value = ValueTy(ProjectionKind::Tuple, TEI->getFieldNo());
    assert(getKind() == ProjectionKind::Tuple);
    assert(getIndex() == TEI->getFieldNo());
    assert(getType(TEI->getOperand()->getType(), TEI->getModule()) ==
           TEI->getType());
    break;
  }
  case ValueKind::TupleElementAddrInst: {
    auto *TEAI = cast<TupleElementAddrInst>(I);
    Value = ValueTy(ProjectionKind::Tuple, TEAI->getFieldNo());
    assert(getKind() == ProjectionKind::Tuple);
    assert(getIndex() == TEAI->getFieldNo());
    assert(getType(TEAI->getOperand()->getType(), TEAI->getModule()) ==
           TEAI->getType());
    break;
  }
  case ValueKind::UncheckedEnumDataInst: {
    auto *UEDI = cast<UncheckedEnumDataInst>(I);
    Value = ValueTy(ProjectionKind::Enum, UEDI->getElementNo());
    assert(getKind() == ProjectionKind::Enum);
    assert(getIndex() == UEDI->getElementNo());
    assert(getType(UEDI->getOperand()->getType(), UEDI->getModule()) ==
           UEDI->getType());
    break;
  }
  case ValueKind::UncheckedTakeEnumDataAddrInst: {
    auto *UTEDAI = cast<UncheckedTakeEnumDataAddrInst>(I);
    Value = ValueTy(ProjectionKind::Enum, UTEDAI->getElementNo());
    assert(getKind() == ProjectionKind::Enum);
    assert(getIndex() == UTEDAI->getElementNo());
    assert(getType(UTEDAI->getOperand()->getType(), UTEDAI->getModule()) ==
           UTEDAI->getType());
    break;
  }
  case ValueKind::IndexAddrInst: {
    // We can represent all integers provided here since getIntegerIndex only
    // returns 32 bit values. When that changes, this code will need to be
    // updated and a MaxLargeIndex will need to be used here. Currently we
    // represent large Indexes using a 64 bit integer, so we don't need to mess
    // with anything.
    unsigned NewIndex = 0;
    auto *IAI = cast<IndexAddrInst>(I);
    if (getIntegerIndex(IAI->getIndex(), NewIndex)) {
      Value = ValueTy(ProjectionKind::Index, NewIndex);
      assert(getKind() == ProjectionKind::Index);
      assert(getIndex() == NewIndex);
    }
    break;
  }
  case ValueKind::UpcastInst: {
    auto *Ty = I->getType().getSwiftRValueType().getPointer();
    assert(Ty->isCanonical());
    Value = ValueTy(ProjectionKind::Upcast, Ty);
    assert(getKind() == ProjectionKind::Upcast);
    assert(getType(I->getOperand(0)->getType(), I->getModule()) ==
           I->getType());
    break;
  }
  case ValueKind::UncheckedRefCastInst: {
    auto *Ty = I->getType().getSwiftRValueType().getPointer();
    assert(Ty->isCanonical());
    Value = ValueTy(ProjectionKind::RefCast, Ty);
    assert(getKind() == ProjectionKind::RefCast);
    assert(getType(I->getOperand(0)->getType(), I->getModule()) ==
           I->getType());
    break;
  }
  case ValueKind::UncheckedBitwiseCastInst:
  case ValueKind::UncheckedAddrCastInst: {
    auto *Ty = I->getType().getSwiftRValueType().getPointer();
    assert(Ty->isCanonical());
    Value = ValueTy(ProjectionKind::BitwiseCast, Ty);
    assert(getKind() == ProjectionKind::BitwiseCast);
    assert(getType(I->getOperand(0)->getType(), I->getModule()) ==
           I->getType());
    break;
  }
  }
}

NullablePtr<SILInstruction>
Projection::createObjectProjection(SILBuilder &B, SILLocation Loc,
                                   SILValue Base) const {
  SILType BaseTy = Base->getType();

  // We can only create a value projection from an object.
  if (!BaseTy.isObject())
    return nullptr;

  // Ok, we now know that the type of Base and the type represented by the base
  // of this projection match and that this projection can be represented as
  // value. Create the instruction if we can. Otherwise, return nullptr.
  switch (getKind()) {
  case ProjectionKind::Struct:
    return B.createStructExtract(Loc, Base, getVarDecl(BaseTy));
  case ProjectionKind::Tuple:
    return B.createTupleExtract(Loc, Base, getIndex());
  case ProjectionKind::Index:
    return nullptr;
  case ProjectionKind::Enum:
    return B.createUncheckedEnumData(Loc, Base, getEnumElementDecl(BaseTy));
  case ProjectionKind::Class:
    return nullptr;
  case ProjectionKind::TailElems:
    return nullptr;
  case ProjectionKind::Box:
    return nullptr;
  case ProjectionKind::Upcast:
    return B.createUpcast(Loc, Base, getCastType(BaseTy));
  case ProjectionKind::RefCast:
    return B.createUncheckedRefCast(Loc, Base, getCastType(BaseTy));
  case ProjectionKind::BitwiseCast:
    return B.createUncheckedBitwiseCast(Loc, Base, getCastType(BaseTy));
  }

  llvm_unreachable("Unhandled ProjectionKind in switch.");
}

NullablePtr<SILInstruction>
Projection::createAddressProjection(SILBuilder &B, SILLocation Loc,
                                    SILValue Base) const {
  SILType BaseTy = Base->getType();

  // We can only create an address projection from an object, unless we have a
  // class.
  if (BaseTy.getClassOrBoundGenericClass() || !BaseTy.isAddress())
    return nullptr;

  // Ok, we now know that the type of Base and the type represented by the base
  // of this projection match and that this projection can be represented as
  // value. Create the instruction if we can. Otherwise, return nullptr.
  switch (getKind()) {
  case ProjectionKind::Struct:
    return B.createStructElementAddr(Loc, Base, getVarDecl(BaseTy));
  case ProjectionKind::Tuple:
    return B.createTupleElementAddr(Loc, Base, getIndex());
  case ProjectionKind::Index: {
    auto IntLiteralTy =
        SILType::getBuiltinIntegerType(64, B.getModule().getASTContext());
    auto IntLiteralIndex =
        B.createIntegerLiteral(Loc, IntLiteralTy, getIndex());
    return B.createIndexAddr(Loc, Base, IntLiteralIndex);
  }
  case ProjectionKind::Enum:
    return B.createUncheckedTakeEnumDataAddr(Loc, Base,
                                             getEnumElementDecl(BaseTy));
  case ProjectionKind::Class:
    return B.createRefElementAddr(Loc, Base, getVarDecl(BaseTy));
  case ProjectionKind::TailElems:
    return B.createRefTailAddr(Loc, Base, getCastType(BaseTy));
  case ProjectionKind::Box:
    return B.createProjectBox(Loc, Base, getIndex());
  case ProjectionKind::Upcast:
    return B.createUpcast(Loc, Base, getCastType(BaseTy));
  case ProjectionKind::RefCast:
  case ProjectionKind::BitwiseCast:
    return B.createUncheckedAddrCast(Loc, Base, getCastType(BaseTy));
  }

  llvm_unreachable("Unhandled ProjectionKind in switch.");
}

void Projection::getFirstLevelProjections(SILType Ty, SILModule &Mod,
                                  llvm::SmallVectorImpl<Projection> &Out) {
  if (auto *S = Ty.getStructOrBoundGenericStruct()) {
    unsigned Count = 0;
    for (auto *VDecl : S->getStoredProperties()) {
      (void) VDecl;
      Projection P(ProjectionKind::Struct, Count++);
      DEBUG(ProjectionPath X(Ty);
            assert(X.getMostDerivedType(Mod) == Ty);
            X.append(P);
            assert(X.getMostDerivedType(Mod) == Ty.getFieldType(VDecl, Mod));
            X.verify(Mod););
      Out.push_back(P);
    }
    return;
  }

  if (auto TT = Ty.getAs<TupleType>()) {
    for (unsigned i = 0, e = TT->getNumElements(); i != e; ++i) {
      Projection P(ProjectionKind::Tuple, i);
      DEBUG(ProjectionPath X(Ty);
            assert(X.getMostDerivedType(Mod) == Ty);
            X.append(P);
            assert(X.getMostDerivedType(Mod) == Ty.getTupleElementType(i));
            X.verify(Mod););
      Out.push_back(P);
    }
    return;
  }

  if (auto *C = Ty.getClassOrBoundGenericClass()) {
    unsigned Count = 0;
    for (auto *VDecl : C->getStoredProperties()) {
      (void) VDecl;
      Projection P(ProjectionKind::Class, Count++);
      DEBUG(ProjectionPath X(Ty);
            assert(X.getMostDerivedType(Mod) == Ty);
            X.append(P);
            assert(X.getMostDerivedType(Mod) == Ty.getFieldType(VDecl, Mod));
            X.verify(Mod););
      Out.push_back(P);
    }
    return;
  }

  if (auto Box = Ty.getAs<SILBoxType>()) {
    for (unsigned field : indices(Box->getLayout()->getFields())) {
      Projection P(ProjectionKind::Box, field);
      DEBUG(ProjectionPath X(Ty);
            assert(X.getMostDerivedType(Mod) == Ty);
            X.append(P);
            assert(X.getMostDerivedType(Mod) == Box->getFieldType(Mod, field));
            X.verify(Mod););
      (void)Box;
      Out.push_back(P);
    }
    return;
  }
}

//===----------------------------------------------------------------------===//
//                            Projection Path
//===----------------------------------------------------------------------===//

Optional<ProjectionPath> ProjectionPath::getProjectionPath(SILValue Start,
                                                           SILValue End) {
  ProjectionPath P(Start->getType(), End->getType());

  // If Start == End, there is a "trivial" projection path in between the
  // two. This is represented by returning an empty ProjectionPath.
  if (Start == End)
    return std::move(P);

  // Do not inspect the body of types with unreferenced types such as bitfields
  // and unions. This is currently only associated with structs.
  if (Start->getType().aggregateHasUnreferenceableStorage() ||
      End->getType().aggregateHasUnreferenceableStorage())
    return llvm::NoneType::None;

  auto Iter = End;
  while (Start != Iter) {
    Projection AP(Iter);
    if (!AP.isValid())
      break;
    P.Path.push_back(AP);
    Iter = cast<SILInstruction>(*Iter).getOperand(0);
  }

  // Return None if we have an empty projection list or if Start == Iter.
  // We do not worry about th implicit #0 in case of index_addr, as the
  // ProjectionPath never allow paths to be compared as a list of indices.
  // Only the encoded type+index pair will be compared.
  if (P.empty() || Start != Iter)
    return llvm::NoneType::None;

  // Reverse to get a path from base to most-derived.
  std::reverse(P.Path.begin(), P.Path.end());

  // Otherwise, return P.
  return std::move(P);
}

/// Returns true if the two paths have a non-empty symmetric difference.
///
/// This means that the two objects have the same base but access different
/// fields of the base object.
bool 
ProjectionPath::hasNonEmptySymmetricDifference(const ProjectionPath &RHS) const{
  // First make sure that both of our base types are the same.
  if (BaseType != RHS.BaseType)
    return false;

  // Otherwise, we have a common base and perhaps some common subpath.
  auto LHSIter = Path.begin();
  auto RHSIter = RHS.Path.begin();

  bool FoundDifferingProjections = false;

  // For each index i until min path size...
  unsigned i = 0;
  for (unsigned e = std::min(size(), RHS.size()); i != e; ++i) {
    // Grab the current projections.
    const Projection &LHSProj = *LHSIter;
    const Projection &RHSProj = *RHSIter;

    // If we are accessing different fields of a common object, the two
    // projection paths may have a non-empty symmetric difference. We check if
    if (LHSProj != RHSProj) {
      DEBUG(llvm::dbgs() << "        Path different at index: " << i << '\n');
      FoundDifferingProjections = true;
      break;
    }

    // Continue if we are accessing the same field.
    LHSIter++;
    RHSIter++;
  }

  // All path elements are the same. The symmetric difference is empty.
  if (!FoundDifferingProjections)
    return false;

  // We found differing projections, but we need to make sure that there are no
  // casts in the symmetric difference. To be conservative, we only wish to
  // allow for casts to appear in the common parts of projections.
  for (unsigned li = i, e = size(); li != e; ++li) {
    if (LHSIter->isAliasingCast())
      return false;
    LHSIter++;
  }
  for (unsigned ri = i, e = RHS.size(); ri != e; ++ri) {
    if (RHSIter->isAliasingCast())
      return false;
    RHSIter++;
  }

  // If we don't have any casts in our symmetric difference (i.e. only typed
  // GEPs), then we can say that these actually have a symmetric difference we
  // can understand. The fundamental issue here is that since we do not have any
  // notion of size, we cannot know the effect of a cast + gep on the final
  // location that we are reaching.
  return true;
}

/// TODO: Integrate has empty non-symmetric difference into here.
SubSeqRelation_t
ProjectionPath::computeSubSeqRelation(const ProjectionPath &RHS) const {
  // Make sure that both base types are the same. Otherwise, we can not compare
  // the projections as sequences.
  if (BaseType != RHS.BaseType)
    return SubSeqRelation_t::Unknown;

  // If both paths are empty, return Equal.
  if (empty() && RHS.empty())
    return SubSeqRelation_t::Equal;

  auto LHSIter = begin();
  auto RHSIter = RHS.begin();

  unsigned MinPathSize = std::min(size(), RHS.size());

  // For each index i until min path size...
  for (unsigned i = 0; i != MinPathSize; ++i) {
    // Grab the current projections.
    const Projection &LHSProj = *LHSIter;
    const Projection &RHSProj = *RHSIter;

    // If the two projections do not equal exactly, return Unrelated.
    //
    // TODO: If Index equals zero, then we know that the two lists have nothing
    // in common and should return unrelated. If Index is greater than zero,
    // then we know that the two projection paths have a common base but a
    // non-empty symmetric difference. For now we just return Unrelated since I
    // can not remember why I had the special check in the
    // hasNonEmptySymmetricDifference code.
    if (LHSProj != RHSProj)
      return SubSeqRelation_t::Unknown;

    // Otherwise increment reverse iterators.
    LHSIter++;
    RHSIter++;
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

Optional<ProjectionPath>
ProjectionPath::removePrefix(const ProjectionPath &Path,
                             const ProjectionPath &Prefix) {
  // We can only subtract paths that have the same base.
  if (Path.BaseType != Prefix.BaseType)
    return llvm::NoneType::None;

  // If Prefix is greater than or equal to Path in size, Prefix can not be a
  // prefix of Path. Return None.
  unsigned PrefixSize = Prefix.size();
  unsigned PathSize = Path.size();

  if (PrefixSize >= PathSize)
    return llvm::NoneType::None;

  // First make sure that the prefix matches.
  Optional<ProjectionPath> P = ProjectionPath(Path.BaseType);
  for (unsigned i = 0; i < PrefixSize; i++) {
    if (Path.Path[i] != Prefix.Path[i]) {
      P.reset();
      return P;
    }
  }

  // Add the rest of Path to P and return P.
  for (unsigned i = PrefixSize, e = PathSize; i != e; ++i) {
    P->Path.push_back(Path.Path[i]);
  }

  return P;
}

raw_ostream &ProjectionPath::print(raw_ostream &os, SILModule &M) {
  os << "Projection Path [";
  SILType IterType = getBaseType();
  for (const Projection &IterProj : Path) {
    SILType BaseType = IterType;
    IterType = IterProj.getType(IterType, M);

    os << BaseType.getAddressType() << "\n  ";

    if (IterProj.isNominalKind()) {
      auto *Decl = IterProj.getVarDecl(BaseType);
      os << "Field: ";
      Decl->print(os);
      os << " of: ";
      continue;
    }

    if (IterProj.getKind() == ProjectionKind::Tuple) {
      os << "Index: " << IterProj.getIndex() << " into: ";
      continue;
    }

    if (IterProj.getKind() == ProjectionKind::BitwiseCast) {
      os << "BitwiseCast to: ";
      continue;
    }
    if (IterProj.getKind() == ProjectionKind::Index) {
      os << "Index: " << IterProj.getIndex() << " into: ";
      continue;
    }
    if (IterProj.getKind() == ProjectionKind::Upcast) {
      os << "UpCast to: ";
      continue;
    }
    if (IterProj.getKind() == ProjectionKind::RefCast) {
      os << "RefCast to: ";
      continue;
    }
    if (IterProj.getKind() == ProjectionKind::Box) {
      os << " Box over: ";
      continue;
    }
    if (IterProj.getKind() == ProjectionKind::TailElems) {
      os << " TailElems of: ";
      continue;
    }
    os << "<unexpected projection> into: ";
  }
  os << IterType.getAddressType() << "]\n";
  return os;
}

void ProjectionPath::dump(SILModule &M) {
  print(llvm::dbgs(), M);
}

void ProjectionPath::verify(SILModule &M) {
#ifndef NDEBUG
  SILType IterTy = getBaseType();
  assert(IterTy);
  for (auto &Proj : Path) {
    IterTy = Proj.getType(IterTy, M);
    assert(IterTy);
  }
#endif
}

void
ProjectionPath::expandTypeIntoLeafProjectionPaths(SILType B, SILModule *Mod,
                                                  ProjectionPathList &Paths) {
  // Perform a BFS to expand the given type into projectionpath each of
  // which contains 1 field from the type.
  llvm::SmallVector<ProjectionPath, 8> Worklist;
  llvm::SmallVector<Projection, 8> Projections;

  // Push an empty projection path to get started.
  ProjectionPath P(B);
  Worklist.push_back(P);
  do {
    // Get the next level projections based on current projection's type.
    ProjectionPath PP = Worklist.pop_back_val();
    // Get the current type to process.
    SILType Ty = PP.getMostDerivedType(*Mod);

    DEBUG(llvm::dbgs() << "Visiting type: " << Ty << "\n");

    // Get the first level projection of the current type.
    Projections.clear();
    Projection::getFirstLevelProjections(Ty, *Mod, Projections);

    // Reached the end of the projection tree, this field can not be expanded
    // anymore.
    if (Projections.empty()) {
      DEBUG(llvm::dbgs() << "    No projections. Finished projection list\n");
      Paths.push_back(PP);
      continue;
    }

    // If this is a class type, we also have reached the end of the type
    // tree for this type.
    //
    // We do not push its next level projection into the worklist,
    // if we do that, we could run into an infinite loop, e.g.
    //
    //   class SelfLoop {
    //     var p : SelfLoop
    //   }
    //
    //   struct XYZ {
    //     var x : Int
    //     var y : SelfLoop
    //   }
    //
    // The worklist would never be empty in this case !.
    //
    if (Ty.getClassOrBoundGenericClass()) {
      DEBUG(llvm::dbgs() << "    Found class. Finished projection list\n");
      Paths.push_back(PP);
      continue;
    }

    // Keep expanding the location.
    for (auto &P : Projections) {
      ProjectionPath X(B);
      X.append(PP);
      ///assert(PP.getMostDerivedType(*Mod) == X.getMostDerivedType(*Mod));
      X.append(P);
      Worklist.push_back(X);
    }
    // Keep iterating if the worklist is not empty.
  } while (!Worklist.empty());
}

bool ProjectionPath::
hasUncoveredNonTrivials(SILType B, SILModule *Mod, ProjectionPathSet &CPaths) {
  llvm::SmallVector<ProjectionPath, 4> Worklist, Paths;
  // Push an empty projection path to get started.
  ProjectionPath P(B);
  Worklist.push_back(P);
  do {
    // Get the next level projections based on current projection's type.
    ProjectionPath PP = Worklist.pop_back_val();
 
    // If this path is part of the covered path, then continue.
    if (CPaths.find(PP) != CPaths.end())
      continue;
      
    // Get the current type to process.
    SILType Ty = PP.getMostDerivedType(*Mod);

    // Get the first level projection of the current type.
    llvm::SmallVector<Projection, 4> Projections;
    Projection::getFirstLevelProjections(Ty, *Mod, Projections);

    // Reached the end of the projection tree, this field can not be expanded
    // anymore.
    if (Projections.empty()) {
      Paths.push_back(PP);
      continue;
    }

    // There is at least one projection path that leads to a type with
    // reference semantics.
    if (Ty.getClassOrBoundGenericClass()) {
      Paths.push_back(PP);
      continue;
    }

    // Keep expanding the location.
    for (auto &P : Projections) {
      ProjectionPath X(B);
      X.append(PP);
      assert(PP.getMostDerivedType(*Mod) == X.getMostDerivedType(*Mod));
      X.append(P);
      Worklist.push_back(X);
    }
    // Keep iterating if the worklist is not empty.
  } while (!Worklist.empty());

  // Check whether any path leads to a non-trivial type.
  for (auto &X : Paths) {
    if (!X.getMostDerivedType(*Mod).isTrivial(*Mod))
       return true;
  }   
  return false;
}

SILValue
ProjectionPath::
createExtract(SILValue Base, SILInstruction *Inst, bool IsVal) const {
  // If we found a projection path, but there are no projections, then the two
  // loads must be the same, return PrevLI.
  if (Path.empty())
    return Base;

  // Ok, at this point we know that we can construct our aggregate projections
  // from our list of address projections.
  SILValue LastExtract = Base;
  SILBuilder Builder(Inst);
  Builder.setCurrentDebugScope(Inst->getFunction()->getDebugScope());

  // We use an auto-generated SILLocation for now.
  // TODO: make the sil location more precise.
  SILLocation Loc = RegularLocation::getAutoGeneratedLocation();

  // Construct the path!
  for (auto PI = Path.begin(), PE = Path.end(); PI != PE; ++PI) {
    if (IsVal) {
      LastExtract =
          PI->createObjectProjection(Builder, Loc, LastExtract).get();
      continue;
    }
    LastExtract =
        PI->createAddressProjection(Builder, Loc, LastExtract).get();
  }

  // Return the last extract we created.
  return LastExtract;
}

bool
Projection::operator<(const Projection &Other) const {
  // If we have a nominal kind...
  if (isNominalKind()) {
    // And Other is also nominal...
    if (Other.isNominalKind()) {
      // Just compare the value decl pointers.
      return getIndex() < Other.getIndex();
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

NullablePtr<SILInstruction>
Projection::
createAggFromFirstLevelProjections(SILBuilder &B, SILLocation Loc,
                                   SILType BaseType,
                                   llvm::SmallVectorImpl<SILValue> &Values) {
  if (BaseType.getStructOrBoundGenericStruct()) {
    return B.createStruct(Loc, BaseType, Values);
  }

  if (BaseType.is<TupleType>()) {
    return B.createTuple(Loc, BaseType, Values);
  }

  return nullptr;
}

SILValue Projection::getOperandForAggregate(SILInstruction *I) const {
  switch (getKind()) {
    case ProjectionKind::Struct:
      if (isa<StructInst>(I))
        return I->getOperand(getIndex());
      break;
    case ProjectionKind::Tuple:
      if (isa<TupleInst>(I))
        return I->getOperand(getIndex());
      break;
    case ProjectionKind::Index:
      break;
    case ProjectionKind::Enum:
      if (EnumInst *EI = dyn_cast<EnumInst>(I)) {
        if (EI->getElement() == getEnumElementDecl(I->getType())) {
          assert(EI->hasOperand() && "expected data operand");
          return EI->getOperand();
        }
      }
      break;
    case ProjectionKind::Class:
    case ProjectionKind::TailElems:
    case ProjectionKind::Box:
    case ProjectionKind::Upcast:
    case ProjectionKind::RefCast:
    case ProjectionKind::BitwiseCast:
      // There is no SIL instruction to create a class or box by aggregating
      // values.
      break;
  }
  return SILValue();
}

//===----------------------------------------------------------------------===//
//                             ProjectionTreeNode
//===----------------------------------------------------------------------===//

ProjectionTreeNode *
ProjectionTreeNode::getChildForProjection(ProjectionTree &Tree,
                                          const Projection &P) {
  for (unsigned Index : ChildProjections) {
    ProjectionTreeNode *N = Tree.getNode(Index);
    if (N->Proj && N->Proj.getValue() == P) {
      return N;
    }
  }
  return nullptr;
}

ProjectionTreeNode *
ProjectionTreeNode::getParent(ProjectionTree &Tree) {
  if (!Parent)
    return nullptr;
  return Tree.getNode(Parent.getValue());
}

const ProjectionTreeNode *
ProjectionTreeNode::getParent(const ProjectionTree &Tree) const {
  if (!Parent)
    return nullptr;
  return Tree.getNode(Parent.getValue());
}

NullablePtr<SILInstruction>
ProjectionTreeNode::
createProjection(SILBuilder &B, SILLocation Loc, SILValue Arg) const {
  if (!Proj)
    return nullptr;

  return Proj->createProjection(B, Loc, Arg);
}

void
ProjectionTreeNode::
processUsersOfValue(ProjectionTree &Tree,
                    llvm::SmallVectorImpl<ValueNodePair> &Worklist,
                    SILValue Value) {
  DEBUG(llvm::dbgs() << "    Looking at Users:\n");

  // For all uses of V...
  for (Operand *Op : getNonDebugUses(Value)) {
    // Grab the User of V.
    SILInstruction *User = Op->getUser();

    DEBUG(llvm::dbgs() << "        " << *User);

    // First try to create a Projection for User.
    auto P = Projection(User);

    // If we fail to create a projection, add User as a user to this node and
    // continue.
    if (!P.isValid()) {
      DEBUG(llvm::dbgs() << "            Failed to create projection. Adding "
            "to non projection user!\n");
      addNonProjectionUser(Op);
      continue;
    }

    DEBUG(llvm::dbgs() << "            Created projection.\n");

    assert(User->hasValue() && "Projections should have a value");

    // we have a projection to the next level children, create the next
    // level children nodes lazily.
    if (!Initialized)
      createNextLevelChildren(Tree);

    // Look up the Node for this projection add {User, ChildNode} to the
    // worklist.
    //
    // *NOTE* This means that we will process ChildNode multiple times
    // potentially with different projection users.
    if (auto *ChildNode = getChildForProjection(Tree, P)) {
      DEBUG(llvm::dbgs() << "            Found child for projection: "
            << ChildNode->getType() << "\n");

      SILValue V = SILValue(User);
      Worklist.push_back({V, ChildNode});
    } else {
      DEBUG(llvm::dbgs() << "            Did not find a child for projection!. "
            "Adding to non projection user!\b");

      // The only projection which we do not currently handle are enums since we
      // may not know the correct case. This can be extended in the future.
      // Is the user an epilogue release ?
      addNonProjectionUser(Op);
    }
  }
}

void
ProjectionTreeNode::
createNextLevelChildrenForStruct(ProjectionTree &Tree, StructDecl *SD) {
  SILModule &Mod = Tree.getModule();
  unsigned ChildIndex = 0;
  SILType Ty = getType();
  for (VarDecl *VD : SD->getStoredProperties()) {
    assert(Tree.getNode(Index) == this && "Node is not mapped to itself?");
    SILType NodeTy = Ty.getFieldType(VD, Mod);
    auto *Node = Tree.createChildForStruct(this, NodeTy, VD, ChildIndex++);
    DEBUG(llvm::dbgs() << "        Creating child for: " << NodeTy << "\n");
    DEBUG(llvm::dbgs() << "            Projection: " 
          << Node->getProjection().getValue().getIndex() << "\n");
    ChildProjections.push_back(Node->getIndex());
    assert(getChildForProjection(Tree, Node->getProjection().getValue()) == Node &&
           "Child not matched to its projection in parent!");
    assert(Node->getParent(Tree) == this && "Parent of Child is not Parent?!");
  }
}

void
ProjectionTreeNode::
createNextLevelChildrenForTuple(ProjectionTree &Tree, TupleType *TT) {
  SILType Ty = getType();
  for (unsigned i = 0, e = TT->getNumElements(); i != e; ++i) {
    assert(Tree.getNode(Index) == this && "Node is not mapped to itself?");
    SILType NodeTy = Ty.getTupleElementType(i);
    auto *Node = Tree.createChildForTuple(this, NodeTy, i);
    DEBUG(llvm::dbgs() << "        Creating child for: " << NodeTy << "\n");
    DEBUG(llvm::dbgs() << "            Projection: "
          << Node->getProjection().getValue().getIndex() << "\n");
    ChildProjections.push_back(Node->getIndex());
    assert(getChildForProjection(Tree, Node->getProjection().getValue()) == Node &&
           "Child not matched to its projection in parent!");
    assert(Node->getParent(Tree) == this && "Parent of Child is not Parent?!");
  }
}

void
ProjectionTreeNode::
createNextLevelChildren(ProjectionTree &Tree) {
  DEBUG(llvm::dbgs() << "    Creating children for: " << getType() << "\n");
  if (Initialized) {
    DEBUG(llvm::dbgs() << "        Already initialized! bailing!\n");
    return;
  }

  Initialized = true;

  SILType Ty = getType();

  if (Ty.aggregateHasUnreferenceableStorage()) {
    DEBUG(llvm::dbgs() << "        Has unreferenced storage bailing!\n");
    return;
  }

  if (auto *SD = Ty.getStructOrBoundGenericStruct()) {
    DEBUG(llvm::dbgs() << "        Found a struct!\n");
    createNextLevelChildrenForStruct(Tree, SD);
    return;
  }

  auto TT = Ty.getAs<TupleType>();
  if (!TT) {
    DEBUG(llvm::dbgs() << "        Did not find a tuple or struct, "
          "bailing!\n");
    return;
  }

  DEBUG(llvm::dbgs() << "        Found a tuple.");
  createNextLevelChildrenForTuple(Tree, TT);
}

SILInstruction *
ProjectionTreeNode::
createAggregate(SILBuilder &B, SILLocation Loc, ArrayRef<SILValue> Args) const {
  assert(Initialized && "Node must be initialized to create aggregates");

  SILType Ty = getType();

  if (Ty.getStructOrBoundGenericStruct()) {
    return B.createStruct(Loc, Ty, Args);
  }

  if (Ty.is<TupleType>()) {
    return B.createTuple(Loc, Ty, Args);
  }

  llvm_unreachable("Unhandled type");
}

class ProjectionTreeNode::NewAggregateBuilder {
  ProjectionTreeNode *Node;
  SILBuilder &Builder;
  SILLocation Loc;
  llvm::SmallVector<SILValue, 8> Values;

  // Did this aggregate already create an aggregate and thus is "invalidated".
  bool Invalidated;

public:
  NewAggregateBuilder(ProjectionTreeNode *N, SILBuilder &B, SILLocation L)
    : Node(N), Builder(B), Loc(L), Values(), Invalidated(false) {
    assert(N->Initialized && "N must be initialized since we are mapping Node "
           "Children -> SILValues");

    // Initialize the Values array with empty SILValues.
    for (unsigned Child : N->ChildProjections) {
      (void)Child;
      Values.push_back(SILValue());
    }
  }

  bool isInvalidated() const { return Invalidated; }

  /// If all SILValues have been set, we are complete.
  bool isComplete() const {
    return std::all_of(Values.begin(), Values.end(), [](SILValue V) -> bool {
      return V;
    });
  }

  SILInstruction *createInstruction() const {
    assert(isComplete() && "Cannot create instruction until the aggregate is "
           "complete");
    assert(!Invalidated && "Must not be invalidated to create an instruction");
    const_cast<NewAggregateBuilder *>(this)->Invalidated = true;
    return Node->createAggregate(Builder, Loc, Values);
  }

  void setValueForChild(ProjectionTreeNode *Child, SILValue V) {
    assert(!Invalidated && "Must not be invalidated to set value for child");
    Values[Child->Proj.getValue().getIndex()] = V;
  }
};

namespace {

using NewAggregateBuilder = ProjectionTreeNode::NewAggregateBuilder;

/// A wrapper around a MapVector with generalized operations on the map.
///
/// TODO: Replace this with a simple RPOT and use GraphUtils. Since we do not
/// look through enums or classes, in the current type system it should not be
/// possible to have a cycle implying that a RPOT should be fine.
class NewAggregateBuilderMap {
  SILBuilder &Builder;
  SILLocation Loc;
  llvm::MapVector<ProjectionTreeNode *, NewAggregateBuilder> NodeBuilderMap;

public:

  NewAggregateBuilderMap(SILBuilder &B, SILLocation Loc)
    : Builder(B), Loc(Loc), NodeBuilderMap() {}

  /// Get the NewAggregateBuilder associated with Node or if none is created,
  /// create one for Node.
  NewAggregateBuilder &getBuilder(ProjectionTreeNode *Node) {
    auto I = NodeBuilderMap.find(Node);
    if (I != NodeBuilderMap.end()) {
      return I->second;
    } else {
      auto AggIt = NodeBuilderMap.insert({Node, NewAggregateBuilder(Node, Builder,
                                                                 Loc)});
      return AggIt.first->second;
    }
  }

  /// Get the NewAggregateBuilder associated with Node. Assert on failure.
  NewAggregateBuilder &get(ProjectionTreeNode *Node) {
    auto It = NodeBuilderMap.find(Node);
    assert(It != NodeBuilderMap.end() && "Every item in the worklist should have "
           "an NewAggregateBuilder associated with it");
    return It->second;
  }

  bool isComplete(ProjectionTreeNode *Node) {
    return get(Node).isComplete();
  }

  bool isInvalidated(ProjectionTreeNode *Node) {
    return get(Node).isInvalidated();
  }

  ProjectionTreeNode *
  getNextValidNode(llvm::SmallVectorImpl<ProjectionTreeNode *> &Worklist,
                   bool CheckForDeadLock=false);
};

} // end anonymous namespace

//===----------------------------------------------------------------------===//
//                               ProjectionTree
//===----------------------------------------------------------------------===//

ProjectionTree::
ProjectionTree(SILModule &Mod, SILType BaseTy) : Mod(Mod) {
  DEBUG(llvm::dbgs() << "Constructing Projection Tree For : " << BaseTy);

  // Create the root node of the tree with our base type.
  createRoot(BaseTy);

  // Create the rest of the type tree lazily based on uses.
}

ProjectionTree::~ProjectionTree() {
  // Do nothing !. Eventually the all the projection tree nodes will be freed
  // when the BPA allocator is free.
}

SILValue
ProjectionTree::computeExplodedArgumentValueInner(SILBuilder &Builder,
                                                  SILLocation Loc,
                                                  ProjectionTreeNode *Node,
                                                  LeafValueMapTy &LeafValues) {
  // Use the child node value if the child is alive.
  if (Node->ChildProjections.empty()) {
    auto Iter = LeafValues.find(Node->getIndex());
    if (Iter != LeafValues.end())
      return Iter->second;
    // Return undef for dead node.
    return SILUndef::get(Node->getType(), Mod);
  }

  // This is an aggregate node, construct its value from its children
  // recursively. 
  //
  // NOTE: We do not expect to have too many levels of nesting, so
  // recursion should be fine.
  llvm::SmallVector<SILValue, 8> ChildValues;
  for (unsigned ChildIdx : Node->ChildProjections) {
    ProjectionTreeNode *Child = getNode(ChildIdx);
    ChildValues.push_back(computeExplodedArgumentValueInner(Builder, Loc, Child,
                                                            LeafValues));
  }

  // Form and return the aggregate.
  NullablePtr<swift::SILInstruction> AI =
      Projection::createAggFromFirstLevelProjections(Builder, Loc,
                                                     Node->getType(),
                                                     ChildValues);

  assert(AI.get() && "Failed to get a part of value");
  return SILValue(AI.get());
}

SILValue
ProjectionTree::computeExplodedArgumentValue(
                            SILBuilder &Builder, SILLocation Loc,
                            llvm::SmallVector<SILValue, 8> &LeafValues) {
  // Construct the leaf index to leaf value map.
  llvm::DenseMap<unsigned, SILValue> LeafIndexToValue;
  for (unsigned i = 0; i < LeafValues.size(); ++i) {
    LeafIndexToValue[LiveLeafIndices[i]] = LeafValues[i];
  }

  // Compute the full root node debug node by walking down the projection tree.
  return computeExplodedArgumentValueInner(Builder, Loc, getRoot(),
                                           LeafIndexToValue);
}

void
ProjectionTree::
computeUsesAndLiveness(SILValue Base) {
  // Propagate liveness and users through the tree.
  llvm::SmallVector<ProjectionTreeNode::ValueNodePair, 32> UseWorklist;
  UseWorklist.push_back({Base, getRoot()});

  // Then until the worklist is empty...
  while (!UseWorklist.empty()) {
    DEBUG(llvm::dbgs() << "Current Worklist:\n");
    DEBUG(for (auto &T : UseWorklist) {
        llvm::dbgs() << "    Type: " << T.second->getType() << "; Value: ";
        if (T.first) {
          llvm::dbgs() << T.first;
        } else {
          llvm::dbgs() << "<null>\n";
        }
    });

    SILValue Value;
    ProjectionTreeNode *Node;

    // Pop off the top type, value, and node.
    std::tie(Value, Node) = UseWorklist.pop_back_val();

    DEBUG(llvm::dbgs() << "Visiting: " << Node->getType() << "\n");

    // If Value is not null, collate all users of Value the appropriate child
    // nodes and add such items to the worklist.
    if (Value) {
      Node->processUsersOfValue(*this, UseWorklist, Value);
    }

    // If this node is live due to a non projection user, propagate down its
    // liveness to its children and its children with an empty value to the
    // worklist so we propagate liveness down to any further descendants.
    if (Node->IsLive) {
      DEBUG(llvm::dbgs() << "Node Is Live. Marking Children Live!\n");
      for (unsigned ChildIdx : Node->ChildProjections) {
        ProjectionTreeNode *Child = getNode(ChildIdx);
        Child->IsLive = true;
        DEBUG(llvm::dbgs() << "    Marking child live: " << Child->getType() << "\n");
        UseWorklist.push_back({SILValue(), Child});
      }
    }
  }

  // Then setup the leaf list by iterating through our Nodes looking for live
  // leafs. We use a DFS order, always processing the left leafs first so that
  // we match the order in which we will lay out arguments.
  llvm::SmallVector<ProjectionTreeNode *, 8> Worklist;
  Worklist.push_back(getRoot());
  while (!Worklist.empty()) {
    ProjectionTreeNode *Node = Worklist.pop_back_val();

    // If node is not a leaf, add its children to the worklist and continue.
    if (!Node->ChildProjections.empty()) {
      for (unsigned ChildIdx : reversed(Node->ChildProjections)) {
        Worklist.push_back(getNode(ChildIdx));
      }
      continue;
    }

    // If the node is a leaf and is not a live, continue.
    if (!Node->IsLive)
      continue;

    // Otherwise we have a live leaf, add its index to our LiveLeafIndices list.
    LiveLeafIndices.push_back(Node->getIndex());
  }

#ifndef NDEBUG
  DEBUG(llvm::dbgs() << "Final Leafs: \n");
  llvm::SmallVector<SILType, 8> LeafTypes;
  getLeafTypes(LeafTypes);
  for (SILType Leafs : LeafTypes) {
    DEBUG(llvm::dbgs() << "    " << Leafs << "\n");
  }
#endif
}

void
ProjectionTree::
createTreeFromValue(SILBuilder &B, SILLocation Loc, SILValue NewBase,
                    llvm::SmallVectorImpl<SILValue> &Leafs) const {
  DEBUG(llvm::dbgs() << "Recreating tree from value: " << NewBase);

  using WorklistEntry =
    std::tuple<const ProjectionTreeNode *, SILValue>;
  llvm::SmallVector<WorklistEntry, 32> Worklist;

  // Start our worklist with NewBase and Root.
  Worklist.push_back(std::make_tuple(getRoot(), NewBase));

  // Then until our worklist is clear...
  while (Worklist.size()) {
    // Pop off the top of the list.
    const ProjectionTreeNode *Node = std::get<0>(Worklist.back());
    SILValue V = std::get<1>(Worklist.back());
    Worklist.pop_back();

    DEBUG(llvm::dbgs() << "Visiting: " << V->getType() << ": " << V);

    // If we have any children...
    unsigned NumChildren = Node->ChildProjections.size();
    if (NumChildren) {
      DEBUG(llvm::dbgs() << "    Not Leaf! Adding children to list.\n");

      // Create projections for each one of them and the child node and
      // projection to the worklist for processing.
      for (unsigned ChildIdx : reversed(Node->ChildProjections)) {
        const ProjectionTreeNode *ChildNode = getNode(ChildIdx);
        SILInstruction *I = ChildNode->createProjection(B, Loc, V).get();
        DEBUG(llvm::dbgs() << "    Adding Child: " << I->getType() << ": " << *I);
        Worklist.push_back(std::make_tuple(ChildNode, SILValue(I)));
      }
    } else {
      // Otherwise, we have a leaf node. If the leaf node is not alive, do not
      // add it to our leaf list.
      if (!Node->IsLive)
        continue;

      // Otherwise add it to our leaf list.
      DEBUG(llvm::dbgs() << "    Is a Leaf! Adding to leaf list\n");
      Leafs.push_back(V);
    }
  }
}

ProjectionTreeNode *
NewAggregateBuilderMap::
getNextValidNode(llvm::SmallVectorImpl<ProjectionTreeNode *> &Worklist,
                 bool CheckForDeadLock) {
  if (Worklist.empty())
    return nullptr;

  ProjectionTreeNode *Node = Worklist.back();

  // If the Node is not complete, then we have reached a dead lock. This should
  // never happen.
  //
  // TODO: Prove this and put the proof here.
  if (CheckForDeadLock && !isComplete(Node)) {
    llvm_unreachable("Algorithm Dead Locked!");
  }

  // This block of code, performs the pop back and also if the Node has been
  // invalidated, skips until we find a non invalidated value.
  while (isInvalidated(Node)) {
    assert(isComplete(Node) && "Invalidated values must be complete");

    // Pop Node off the back of the worklist.
    Worklist.pop_back();

    if (Worklist.empty())
      return nullptr;

    Node = Worklist.back();
  }

  return Node;
}

void
ProjectionTree::
replaceValueUsesWithLeafUses(SILBuilder &Builder, SILLocation Loc,
                             llvm::SmallVectorImpl<SILValue> &Leafs) {
  assert(Leafs.size() == LiveLeafIndices.size() && "Leafs and leaf indices must "
         "equal in size.");

  NewAggregateBuilderMap AggBuilderMap(Builder, Loc);
  llvm::SmallVector<ProjectionTreeNode *, 8> Worklist;

  DEBUG(llvm::dbgs() << "Replacing all uses in callee with leafs!\n");

  // For each Leaf we have as input...
  for (unsigned i = 0, e = Leafs.size(); i != e; ++i) {
    SILValue Leaf = Leafs[i];
    ProjectionTreeNode *Node = getNode(LiveLeafIndices[i]);

    DEBUG(llvm::dbgs() << "    Visiting leaf: " << Leaf);

    assert(Node->IsLive && "Unexpected dead node in LiveLeafIndices!");

    // Otherwise replace all uses at this level of the tree with uses of the
    // Leaf value.
    DEBUG(llvm::dbgs() << "        Replacing operands with leaf!\n");
    for (auto *Op : Node->NonProjUsers) {
      DEBUG(llvm::dbgs() << "            User: " << *Op->getUser());
      Op->set(Leaf);
    }

    // Grab the parent of this node.
    ProjectionTreeNode *Parent = Node->getParent(*this);

    // If the parent is dead, continue.
    if (!Parent || !Parent->IsLive) {
      DEBUG(llvm::dbgs() << "        Parent is dead... continuing.\n");
      continue;
    }

    DEBUG(llvm::dbgs() << "        Parent is alive! Adding to parent "
          "builder\n");

    // Otherwise either create an aggregate builder for the parent or reuse one
    // that has already been created for it.
    AggBuilderMap.getBuilder(Parent).setValueForChild(Node, Leaf);

    DEBUG(llvm::dbgs() << "            Is parent complete: "
          << (AggBuilderMap.isComplete(Parent)? "yes" : "no") << "\n");

    // Finally add the parent to the worklist.
    Worklist.push_back(Parent);
  }

  // A utility array to add new Nodes to the list so we maintain while
  // processing the current worklist we maintain only completed items at the end
  // of the list.
  llvm::SmallVector<ProjectionTreeNode *, 8> NewNodes;

  DEBUG(llvm::dbgs() << "Processing worklist!\n");

  // Then until we have no work left...
  while (!Worklist.empty()) {
    // Sort the worklist so that complete items are first.
    //
    // TODO: Just change this into a partition method. Should be significantly
    // faster.
    std::sort(Worklist.begin(), Worklist.end(),
              [&AggBuilderMap](ProjectionTreeNode *N1,
                     ProjectionTreeNode *N2) -> bool {
                bool IsComplete1 = AggBuilderMap.isComplete(N1);
                bool IsComplete2 = AggBuilderMap.isComplete(N2);

                // Sort N1 after N2 if N1 is complete and N2 is not. This puts
                // complete items at the end of our list.
                return unsigned(IsComplete1) < unsigned(IsComplete2);
              });

    DEBUG(llvm::dbgs() << "    Current Worklist:\n");
#ifndef NDEBUG
    for (auto *_work : Worklist) {
      DEBUG(llvm::dbgs() << "        Type: " << _work->getType()
                   << "; Complete: "
                   << (AggBuilderMap.isComplete(_work)? "yes" : "no")
                   << "; Invalidated: "
            << (AggBuilderMap.isInvalidated(_work)? "yes" : "no") << "\n");
    }
#endif

    // Find the first non invalidated node. If we have all invalidated nodes,
    // this will return nullptr.
    ProjectionTreeNode *Node = AggBuilderMap.getNextValidNode(Worklist, true);

    // Then until we find a node that is not complete...
    while (Node && AggBuilderMap.isComplete(Node)) {
      // Create the aggregate for the current complete Node we are processing...
      SILValue Agg = AggBuilderMap.get(Node).createInstruction();

      // Replace all uses at this level of the tree with uses of the newly
      // constructed aggregate.
      for (auto *Op : Node->NonProjUsers) {
        Op->set(Agg);
      }

      // If this node has a parent and that parent is alive...
      ProjectionTreeNode *Parent = Node->getParentOrNull(*this);
      if (Parent && Parent->IsLive) {
        // Create or lookup the node builder for the parent and associate the
        // newly created aggregate with this node.
        AggBuilderMap.getBuilder(Parent).setValueForChild(Node, SILValue(Agg));
        // Then add the parent to NewNodes to be added to our list.
        NewNodes.push_back(Parent);
      }

      // Grab the next non-invalidated node for the next iteration. If we had
      // all invalidated nodes, this will return nullptr.
      Node = AggBuilderMap.getNextValidNode(Worklist);
    }

    // Copy NewNodes onto the back of our Worklist now that we have finished
    // this iteration.
    std::copy(NewNodes.begin(), NewNodes.end(), std::back_inserter(Worklist));
    NewNodes.clear();
  }
}

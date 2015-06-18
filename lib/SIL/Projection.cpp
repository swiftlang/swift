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
#include "swift/SIL/DebugUtils.h"
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
static_assert(sizeof(Projection) == ((sizeof(uintptr_t) * 2)
                                     + (sizeof (unsigned int) * 2)),
              "Projection size changed");

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
  case ValueKind::IndexAddrInst:
    assert(isAddrProjection(I) && "isAddrProjection out of sync");
    return Projection(cast<IndexAddrInst>(I));
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

/// We do not support symbolic projections yet, only 32-bit unsigned integers.
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

Projection::Projection(StructElementAddrInst *SEA)
    : Type(SEA->getType()), Decl(SEA->getField()),
      Index(getIndexForValueDecl(Decl)),
      Kind(unsigned(ProjectionKind::Struct)) {}

Projection::Projection(TupleElementAddrInst *TEA)
    : Type(TEA->getType()), Decl(nullptr), Index(TEA->getFieldNo()),
      Kind(unsigned(ProjectionKind::Tuple)) {}

Projection::Projection(IndexAddrInst *IA)
    : Type(IA->getType()), Decl(nullptr),
      Kind(unsigned(ProjectionKind::Index)) {
  bool valid = getIntegerIndex(IA->getIndex(), Index);
  (void)valid;
  assert(valid && "only index_addr taking integer literal is supported");
}

Projection::Projection(RefElementAddrInst *REA)
    : Type(REA->getType()), Decl(REA->getField()),
      Index(getIndexForValueDecl(Decl)), Kind(unsigned(ProjectionKind::Class)) {
}

/// UncheckedTakeEnumDataAddrInst always have an index of 0 since enums only
/// have one payload.
Projection::Projection(UncheckedTakeEnumDataAddrInst *UTEDAI)
    : Type(UTEDAI->getType()), Decl(UTEDAI->getElement()), Index(0),
      Kind(unsigned(ProjectionKind::Enum)) {}

Projection::Projection(StructExtractInst *SEI)
    : Type(SEI->getType()), Decl(SEI->getField()),
      Index(getIndexForValueDecl(Decl)),
      Kind(unsigned(ProjectionKind::Struct)) {}

Projection::Projection(TupleExtractInst *TEI)
    : Type(TEI->getType()), Decl(nullptr), Index(TEI->getFieldNo()),
      Kind(unsigned(ProjectionKind::Tuple)) {}

/// UncheckedEnumData always have an index of 0 since enums only have one
/// payload.
Projection::Projection(UncheckedEnumDataInst *UEDAI)
    : Type(UEDAI->getType()), Decl(UEDAI->getElement()), Index(0),
      Kind(unsigned(ProjectionKind::Enum)) {}

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
  case ProjectionKind::Index:
    return nullptr;
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
  case ProjectionKind::Index: {
    auto Ty = SILType::getBuiltinIntegerType(32, B.getASTContext());
    auto *IntLiteral = B.createIntegerLiteral(Loc, Ty, getIndex());
    return B.createIndexAddr(Loc, Base, IntLiteral);
  }
  case ProjectionKind::Enum:
    return B.createUncheckedTakeEnumDataAddr(Loc, Base,
                                             cast<EnumElementDecl>(getDecl()));
  case ProjectionKind::Class:
    return B.createRefElementAddr(Loc, Base, cast<VarDecl>(getDecl()));
  }
}

SILValue Projection::getOperandForAggregate(SILInstruction *I) const {
  switch (getKind()) {
    case ProjectionKind::Struct:
      if (isa<StructInst>(I))
        return I->getOperand(getDeclIndex());
      break;
    case ProjectionKind::Tuple:
      if (isa<TupleInst>(I))
        return I->getOperand(getIndex());
      break;
    case ProjectionKind::Index:
      break;
    case ProjectionKind::Enum:
      if (EnumInst *EI = dyn_cast<EnumInst>(I)) {
        if (EI->getElement() == Decl) {
          assert(EI->hasOperand() && "expected data operand");
          return EI->getOperand();
        }
      }
      break;
    case ProjectionKind::Class:
      // There is no SIL instruction to create a class by aggregating values.
      break;
  }
  return SILValue();
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
  bool NextAddrIsIndex = false;
  while (Projection::isAddrProjection(Iter) && Start != Iter) {
    Projection AP = *Projection::addressProjectionForValue(Iter);
    P.Path.push_back(AP);
    NextAddrIsIndex = (AP.getKind() == ProjectionKind::Index);

    Iter = cast<SILInstruction>(*Iter).getOperand(0);
    if (IgnoreCasts)
      Iter = Iter.stripCasts();
  }

  // Return None if we have an empty projection list or if Start == Iter.
  // If the next project is index_addr, then Start and End actually point to
  // disjoint locations (the value at Start has an implicit index_addr #0).
  if (P.empty() || Start != Iter || NextAddrIsIndex)
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
    auto P = Projection::projectionForInstruction(User);

    // If we fail to create a projection, add User as a user to this node and
    // continue.
    if (!P) {
      DEBUG(llvm::dbgs() << "            Failed to create projection. Adding "
            "to non projection user!\n");
      addNonProjectionUser(Op);
      continue;
    }

    DEBUG(llvm::dbgs() << "            Created projection.\n");

    assert(User->getNumTypes() == 1 && "Projections should only have one use");

    // Look up the Node for this projection add add {User, ChildNode} to the
    // worklist.
    //
    // *NOTE* This means that we will process ChildNode multiple times
    // potentially with different projection users.
    if (auto *ChildNode = getChildForProjection(Tree, *P)) {
      DEBUG(llvm::dbgs() << "            Found child for projection: "
            << ChildNode->getType() << "\n");

      SILValue V = SILValue(User);
      Worklist.push_back({V, ChildNode});
    } else {
      DEBUG(llvm::dbgs() << "            Did not find a child for projection!. "
            "Adding to non projection user!\b");

      // The only projection which we do not currently handle are enums since we
      // may not know the correct case. This can be xtended in the future.
      addNonProjectionUser(Op);
    }
  }
}

void
ProjectionTreeNode::
createChildrenForStruct(ProjectionTree &Tree,
                        llvm::SmallVectorImpl<ProjectionTreeNode *> &Worklist,
                        StructDecl *SD) {
  SILModule &Mod = Tree.getModule();
  unsigned ChildIndex = 0;
  SILType Ty = getType();
  for (VarDecl *VD : SD->getStoredProperties()) {
    assert(Tree.getNode(Index) == this && "Node is not mapped to itself?");
    SILType NodeTy = Ty.getFieldType(VD, Mod);
    auto *Node = Tree.createChildForStruct(this, NodeTy, VD, ChildIndex++);
    DEBUG(llvm::dbgs() << "        Creating child for: " << NodeTy << "\n");
    DEBUG(llvm::dbgs() << "            Projection: " << Node->getProjection().getValue().getGeneralizedIndex() << "\n");
    ChildProjections.push_back(Node->getIndex());
    assert(getChildForProjection(Tree, Node->getProjection().getValue()) == Node &&
           "Child not matched to its projection in parent!");
    assert(Node->getParent(Tree) == this && "Parent of Child is not Parent?!");
    Worklist.push_back(Node);
  }
}

void
ProjectionTreeNode::
createChildrenForTuple(ProjectionTree &Tree,
                       llvm::SmallVectorImpl<ProjectionTreeNode *> &Worklist,
                       TupleType *TT) {
  SILType Ty = getType();
  for (unsigned i = 0, e = TT->getNumElements(); i != e; ++i) {
    assert(Tree.getNode(Index) == this && "Node is not mapped to itself?");
    SILType NodeTy = Ty.getTupleElementType(i);
    auto *Node = Tree.createChildForTuple(this, NodeTy, i);
    DEBUG(llvm::dbgs() << "        Creating child for: " << NodeTy << "\n");
    DEBUG(llvm::dbgs() << "            Projection: " << Node->getProjection().getValue().getGeneralizedIndex() << "\n");
    ChildProjections.push_back(Node->getIndex());
    assert(getChildForProjection(Tree, Node->getProjection().getValue()) == Node &&
           "Child not matched to its projection in parent!");
    assert(Node->getParent(Tree) == this && "Parent of Child is not Parent?!");
    Worklist.push_back(Node);
  }
}

void
ProjectionTreeNode::
createChildren(ProjectionTree &Tree,
               llvm::SmallVectorImpl<ProjectionTreeNode *> &Worklist) {
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
    createChildrenForStruct(Tree, Worklist, SD);
    return;
  }

  auto TT = Ty.getAs<TupleType>();
  if (!TT) {
    DEBUG(llvm::dbgs() << "        Did not find a tuple or struct, "
          "bailing!\n");
    return;
  }

  DEBUG(llvm::dbgs() << "        Found a tuple.");
  createChildrenForTuple(Tree, Worklist, TT);
}

SILInstruction *
ProjectionTreeNode::
createAggregate(SILBuilder &B, SILLocation Loc, ArrayRef<SILValue> Args) const {
  assert(Initialized && "Node must be initialized to create aggregates");

  SILType Ty = getType();

  if (Ty.getStructOrBoundGenericStruct()) {
    return B.createStruct(Loc, Ty, Args);
  }

  if (Ty.getAs<TupleType>()) {
    return B.createTuple(Loc, Ty, Args);
  }

  llvm_unreachable("Unhandled type");
}

//===----------------------------------------------------------------------===//
//                               ProjectionTree
//===----------------------------------------------------------------------===//

ProjectionTree::ProjectionTree(SILModule &Mod, llvm::BumpPtrAllocator &BPA,
                               SILType BaseTy) : Mod(Mod), Allocator(BPA) {
  DEBUG(llvm::dbgs() << "Constructing Projection Tree For : " << BaseTy);

  // Create the root node of the tree with our base type.
  createRoot(BaseTy);

  // Initialize the worklist with the root node.
  llvm::SmallVector<ProjectionTreeNode *, 8> Worklist;
  Worklist.push_back(getRoot());

  // Then until the worklist is empty...
  while (!Worklist.empty()) {
    DEBUG(llvm::dbgs() << "Current Worklist:\n");
    DEBUG(for (auto *N : Worklist) {
        llvm::dbgs() << "    " << N->getType() << "\n";
    });

    // Pop off the top of the list.
    ProjectionTreeNode *Node = Worklist.pop_back_val();

    DEBUG(llvm::dbgs() << "Visiting: " << Node->getType() << "\n");

    // Initialize the worklist and its children, adding them to the worklist as
    // we create them.
    Node->createChildren(*this, Worklist);
  }
}

void
ProjectionTree::computeUsesAndLiveness(SILValue Base) {
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
    // worklist so we propagate liveness down to any further descendents.
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

    // Otherwise we have a live leaf, add its index to our LeafIndices list.
    LeafIndices.push_back(Node->getIndex());
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

    DEBUG(llvm::dbgs() << "Visiting: " << V.getType() << ": " << V);

    // If we have any children...
    unsigned NumChildren = Node->ChildProjections.size();
    if (NumChildren) {
      DEBUG(llvm::dbgs() << "    Not Leaf! Adding children to list.\n");

      // Create projections for each one of them and the child node and
      // projection to the worklist for processing.
      for (unsigned ChildIdx : reversed(Node->ChildProjections)) {
        const ProjectionTreeNode *ChildNode = getNode(ChildIdx);
        SILInstruction *I = ChildNode->createProjection(B, Loc, V).get();
        DEBUG(llvm::dbgs() << "    Adding Child: " << I->getType(0) << ": " << *I);
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

class ProjectionTreeNode::AggregateBuilder {
  ProjectionTreeNode *Node;
  SILBuilder &Builder;
  SILLocation Loc;
  llvm::SmallVector<SILValue, 8> Values;

  // Did this aggregate already create an aggregate and thus is "invalidated".
  bool Invalidated;

public:
  AggregateBuilder(ProjectionTreeNode *N, SILBuilder &B, SILLocation L)
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
      return V.getDef();
    });
  }

  SILInstruction *createInstruction() const {
    assert(isComplete() && "Can not create instruction until the aggregate is "
           "complete");
    assert(!Invalidated && "Must not be invalidated to create an instruction");
    const_cast<AggregateBuilder *>(this)->Invalidated = true;
    return Node->createAggregate(Builder, Loc, Values);
  }

  void setValueForChild(ProjectionTreeNode *Child, SILValue V) {
    assert(!Invalidated && "Must not be invalidated to set value for child");
    Values[Child->Proj.getValue().getGeneralizedIndex()] = V;
  }
};

namespace {

using AggregateBuilder = ProjectionTreeNode::AggregateBuilder;

/// A wrapper around a MapVector with generalized operations on the map.
///
/// TODO: Replace this with a simple RPOT and use GraphUtils. Since we do not
/// look through enums or classes, in the current type system it should not be
/// possible to have a cycle implying that a RPOT should be fine.
class AggregateBuilderMap {
  SILBuilder &Builder;
  SILLocation Loc;
  llvm::MapVector<ProjectionTreeNode *, AggregateBuilder> NodeBuilderMap;

public:

  AggregateBuilderMap(SILBuilder &B, SILLocation Loc)
    : Builder(B), Loc(Loc), NodeBuilderMap() {}

  /// Get the AggregateBuilder associated with Node or if none is created,
  /// create one for Node.
  AggregateBuilder &getBuilder(ProjectionTreeNode *Node) {
    auto I = NodeBuilderMap.find(Node);
    if (I != NodeBuilderMap.end()) {
      return I->second;
    } else {
      auto AggIt = NodeBuilderMap.insert({Node, AggregateBuilder(Node, Builder,
                                                                 Loc)});
      return AggIt.first->second;
    }
  }

  /// Get the AggregateBuilder associated with Node. Assert on failure.
  AggregateBuilder &get(ProjectionTreeNode *Node) {
    auto It = NodeBuilderMap.find(Node);
    assert(It != NodeBuilderMap.end() && "Every item in the worklist should have "
           "an AggregateBuilder associated with it");
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

ProjectionTreeNode *
AggregateBuilderMap::
getNextValidNode(llvm::SmallVectorImpl<ProjectionTreeNode *> &Worklist,
                 bool CheckForDeadLock) {
  if (Worklist.empty())
    return nullptr;

  ProjectionTreeNode *Node = Worklist.back();

  // If the Node is not complete, then we have reached a dead lock. This should never happen.
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
  assert(Leafs.size() == LeafIndices.size() && "Leafs and leaf indices must "
         "equal in size.");

  AggregateBuilderMap AggBuilderMap(Builder, Loc);
  llvm::SmallVector<ProjectionTreeNode *, 8> Worklist;

  DEBUG(llvm::dbgs() << "Replacing all uses in callee with leafs!\n");

  // For each Leaf we have as input...
  for (unsigned i = 0, e = Leafs.size(); i != e; ++i) {
    SILValue Leaf = Leafs[i];
    ProjectionTreeNode *Node = getNode(LeafIndices[i]);

    DEBUG(llvm::dbgs() << "    Visiting leaf: " << Leaf);

    assert(Node->IsLive && "Unexpected dead node in LeafIndices!");

    // Otherwise replace all uses at this level of the tree with uses of the
    // Leaf value.
    DEBUG(llvm::dbgs() << "        Replacing operands with leaf!\n");
    for (auto *Op : Node->NonProjUsers) {
      DEBUG(llvm::dbgs() << "            User: " << *Op->getUser());
      Op->set(Leaf);
    }

    // Grab the parent of this node.
    ProjectionTreeNode *Parent = Node->getParent(*this);
    DEBUG(llvm::dbgs() << "        Visiting parent of leaf: " <<
          Parent->getType() << "\n");

    // If the parent is dead, continue.
    if (!Parent->IsLive) {
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
    for (auto *_N : Worklist) {
      DEBUG(llvm::dbgs() << "        Type: " << _N->getType()
                   << "; Complete: "
                   << (AggBuilderMap.isComplete(_N)? "yes" : "no")
                   << "; Invalidated: "
            << (AggBuilderMap.isInvalidated(_N)? "yes" : "no") << "\n");
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

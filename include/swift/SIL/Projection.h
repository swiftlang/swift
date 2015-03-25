//===--- Projection.h - Utilities for working with  Projections -*- C++ -*-===//
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
//
// This file defines the class Projection and related utilities. A projection is
// a representation of type projections that is nominal, tuple agnostic. These
// utilities are useful for working with aggregate type trees at a high level.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_SIL_PROJECTION_H
#define SWIFT_SIL_PROJECTION_H

#include "swift/Basic/NullablePtr.h"
#include "swift/SIL/SILValue.h"
#include "swift/SIL/SILInstruction.h"
#include "llvm/ADT/Optional.h"
#include "llvm/ADT/PointerIntPair.h"
#include "llvm/ADT/SetVector.h"
#include "llvm/Support/Allocator.h"
#include <type_traits>

namespace swift {

class SILBuilder;

/// The kind of projection that we are representing.
enum class ProjectionKind : uint8_t {
  Struct,
  Tuple,
  Class,
  Enum,
  LastProjectionKind = Enum,
};

/// An abstract representation of a SIL Projection that allows one to work with
/// value projections and address projections at an abstract level.
class Projection {
  friend class ProjectionTree;

  /// The type of this projection.
  SILType Type;

  /// The decl associated with this projection if the projection is
  /// representing a nominal type.
  ///
  /// TODO: We could use a pointer int pair here with kind. I expect to expand
  /// projection kind in the future to include other types of projections,
  /// i.e. indexing.
  ValueDecl *Decl;

  /// The index associated with the projection if the projection is representing
  /// a tuple type or the decl's index in its parent. We assume no more than 64
  /// indices so we can merge this with Kind into two pointers. We can lose a
  /// bit here and be ok.
  unsigned Index : 6;

  /// The kind of projection that we are processing.
  unsigned Kind : 2;
  static_assert(unsigned(ProjectionKind::LastProjectionKind) == 3,
                "Need to update size of kind");

public:
  Projection(const Projection &P)
      : Type(P.getType()), Index(0), Kind(unsigned(P.getKind())) {
    if (P.isNominalKind()) {
      Decl = P.getDecl();
      Index = P.getDeclIndex();
    } else {
      Index = P.getIndex();
    }
  }

  ~Projection() = default;

  /// If I is representable as a projection, return the projection. Otherwise,
  /// return None.
  static llvm::Optional<Projection>
  projectionForInstruction(SILInstruction *I);

  /// If I is an address projection, return the projection. Otherwise return
  /// None.
  static llvm::Optional<Projection>
  addressProjectionForInstruction(SILInstruction *I);

  /// Helper method that returns None if V is not an instruction or not an
  /// address projection. Otherwise, returns the address projection.
  static llvm::Optional<Projection> addressProjectionForValue(SILValue V) {
    auto *I = dyn_cast<SILInstruction>(V);
    if (!I)
      return llvm::NoneType::None;
    return addressProjectionForInstruction(I);
  }

  /// If I is a value projection, return the projection. Otherwise return None.
  static llvm::Optional<Projection>
  valueProjectionForInstruction(SILInstruction *I);

  bool operator==(const Projection &Other) const;

  bool operator!=(const Projection &Other) const {
    return !(*this == Other);
  }

  /// This is a weak ordering that does not have a real meaning beyond being
  /// stable and keeping projections of the same type together.
  bool operator<(Projection Other) const;

  /// Determine if I is a value projection instruction whose corresponding
  /// projection equals this projection.
  bool matchesValueProjection(SILInstruction *I) const;

  /// Helper method that returns isAddrProjection(I->getKind());
  static bool isAddrProjection(SILValue V) {
    return isAddrProjection(V->getKind());
  }

  /// Returns true if K is a ValueKind that Projection recognizes as an address
  /// projection.
  static bool isAddrProjection(ValueKind K) {
    switch (K) {
    case ValueKind::StructElementAddrInst:
    case ValueKind::TupleElementAddrInst:
    case ValueKind::RefElementAddrInst:
    case ValueKind::UncheckedTakeEnumDataAddrInst:
      return true;
    default:
      return false;
    }
  }

  /// Helper method that returns isValueProjection(I->getKind());
  static bool isValueProjection(SILValue V) {
    return isValueProjection(V->getKind());
  }

  /// Returns true if this is K is a value kind that the projection class
  /// recognizes as a value projection.
  static bool isValueProjection(ValueKind K) {
    switch (K) {
    case ValueKind::StructExtractInst:
    case ValueKind::TupleExtractInst:
    case ValueKind::UncheckedEnumDataInst:
      return true;
    default:
      return false;
    }
  }

  /// Returns the ProjectionKind of this instruction.
  ProjectionKind getKind() const { return ProjectionKind(Kind); }

  /// Return the type of this projection.
  SILType getType() const { return Type; }

  /// If this is a nominal kind projection, return the value decl of the
  /// projection.
  ValueDecl *getDecl() const {
    assert(isNominalKind() && "Must have a nominal kind to return a decl");
    return Decl;
  }

  /// If this is a nominal decl, return the index of the decl in its parent.
  unsigned getDeclIndex() const {
    assert(isNominalKind() && "Must have a nominal kind to return a decl");
    return Index;
  }

  /// If this is an indexed kind projection, return the index of the projection.
  unsigned getIndex() const {
    assert(isIndexedKind() && "Must have an indexed kind to return an index");
    return Index;
  }

  /// Return the generalized index that works for both decls and tuples.
  unsigned getGeneralizedIndex() const {
    return Index;
  }

  /// Returns true if this projection is a nominal kind projection.
  bool isNominalKind() const {
    switch (getKind()) {
    case ProjectionKind::Struct:
    case ProjectionKind::Class:
    case ProjectionKind::Enum:
      return true;
    case ProjectionKind::Tuple:
      return false;
    }
  }

  /// Returns true if this projection is an indexed kind projection.
  ///
  /// This looks very sparse now, but in the future it will include type indexed
  /// and byte indexed kinds for index_addr and index_raw_addr instructions.
  bool isIndexedKind() const {
    switch (getKind()) {
    case ProjectionKind::Tuple:
      return true;
    case ProjectionKind::Struct:
    case ProjectionKind::Class:
    case ProjectionKind::Enum:
      return false;
    }
  }

  /// If Base's type matches this Projections type ignoring Address vs Object
  /// type differences and this Projection is representable as a value
  /// projection, create the relevant value projection and return it. Otherwise,
  /// return nullptr.
  NullablePtr<SILInstruction>
  createValueProjection(SILBuilder &B, SILLocation Loc, SILValue Base) const;

  /// If Base's type matches this Projections type ignoring Address vs Object
  /// type and this Projection is representable as an address projection, create
  /// the relevant address projection and return it. Otherwise, return nullptr.
  NullablePtr<SILInstruction>
  createAddrProjection(SILBuilder &B, SILLocation Loc, SILValue Base) const;

  NullablePtr<SILInstruction>
  createProjection(SILBuilder &B, SILLocation Loc, SILValue Base) const {
    if (Base.getType().isAddress()) {
      return createAddrProjection(B, Loc, Base);
    } else if (Base.getType().isObject()) {
      return createValueProjection(B, Loc, Base);
    } else {
      llvm_unreachable("Unsupported SILValueCategory");
    }
  }

  /// Returns the operand of a struct, tuple or enum instruction which is
  /// associated with this projection. Returns an invalid SILValue if \p I is
  /// not a matching aggregate instruction.
  SILValue getOperandForAggregate(SILInstruction *I) const;

private:
  Projection(ProjectionKind Kind, SILType Type, ValueDecl *Decl, unsigned Index)
      : Type(Type), Decl(Decl), Index(Index), Kind(unsigned(Kind)) {}

  explicit Projection(StructElementAddrInst *SEA);
  explicit Projection(TupleElementAddrInst *TEA);
  explicit Projection(RefElementAddrInst *REA);
  explicit Projection(UncheckedTakeEnumDataAddrInst *UTEDAI);
  explicit Projection(StructExtractInst *SEI);
  explicit Projection(TupleExtractInst *TEI);
  explicit Projection(UncheckedEnumDataInst *UEDAI);
};

enum class SubSeqRelation_t : uint8_t {
  Unrelated = 0,
  LHSStrictSubSeqOfRHS = 1,
  RHSStrictSubSeqOfLHS = 2,
  Equal = 3
};

/// Returns true if Seq is either LHSStrictSubSeqOfRHS or
/// RHSStrictSubSeqOfLHS. Returns false if Seq is one of either Equal or
/// Unrelated.
inline bool isStrictSubSeqRelation(SubSeqRelation_t Seq) {
  switch (Seq) {
  case SubSeqRelation_t::Unrelated:
  case SubSeqRelation_t::Equal:
    return false;
  case SubSeqRelation_t::LHSStrictSubSeqOfRHS:
  case SubSeqRelation_t::RHSStrictSubSeqOfLHS:
    return true;
  }
}

/// A "path" of projections abstracting either value or aggregate projections
/// upon a value.
///
/// The main purpose of this class is to enable one to reason about iterated
/// chains of projections. Some example usages are:
///
/// 1. Converting value projections to aggregate projections or vis-a-versa.
/// 2. Performing tuple operations on two paths (using the mathematical
///    definition of tuples as ordered sets).
class ProjectionPath {
public:
  using PathTy = llvm::SmallVector<Projection, 8>;

private:
  PathTy Path;

public:
  /// Create an empty path which serves as a stack. Use push_back() to populate
  /// the stack with members.
  ProjectionPath() : Path() {}

  ~ProjectionPath() = default;

  /// Do not allow copy construction. The only way to get one of these is from
  /// getAddressProjectionPathBetweenValues which involves the move constructor.
  ProjectionPath(const ProjectionPath &Other) = delete;

  /// We only allow for moves of ProjectionPath since we only want them to be
  /// able to be constructed by calling our factory method.
  ProjectionPath(ProjectionPath &&Other) : Path(Other.Path) {}

  /// Create a new address projection path from the pointer Start through
  /// various address projections to End. Returns Nothing::None if there is no
  /// such path.
  static Optional<ProjectionPath>
  getAddrProjectionPath(SILValue Start, SILValue End, bool IgnoreCasts=false);

  static Optional<ProjectionPath>
  subtractPaths(const ProjectionPath &LHS, const ProjectionPath &RHS);

  /// Returns true if the two paths have a non-empty symmetric difference.
  ///
  /// This means that the two objects have the same base but access different
  /// fields of the base object.
  bool hasNonEmptySymmetricDifference(const ProjectionPath &RHS) const;

  /// Compute the subsequence relation in between LHS and RHS which tells the
  /// user whether or not the two sequences are unrelated, equal, or if one is a
  /// subsequence of the other.
  SubSeqRelation_t computeSubSeqRelation(const ProjectionPath &RHS) const;

  /// Find all value projection paths from I that matches this projection
  /// path. Return the tails of each extract path in T.
  bool
  findMatchingValueProjectionPaths(SILInstruction *I,
                                   SmallVectorImpl<SILInstruction *> &T) const;

  /// Pushes an element to the path.
  void push_back(const Projection &Proj) { Path.push_back(Proj); }

  /// Removes the last element from the path.
  void pop_back() { Path.pop_back(); }

  /// Returns the last element of the path.
  const Projection &back() const { return Path.back(); }

  /// Returns true if LHS and RHS have all the same projections in the same
  /// order.
  bool operator==(const ProjectionPath &RHS) const {
    return computeSubSeqRelation(RHS) == SubSeqRelation_t::Equal;
  }

  bool operator!=(const ProjectionPath &RHS) const {
    return !(*this == RHS);
  }

  /// Returns true if the contained projection path is empty.
  bool empty() const { return Path.empty(); }

  /// Returns the number of path elements in the given projection path.
  unsigned size() const { return Path.size(); }

  using iterator = PathTy::iterator;
  using const_iterator = PathTy::const_iterator;
  using reverse_iterator = PathTy::reverse_iterator;
  using const_reverse_iterator = PathTy::const_reverse_iterator;

  iterator begin() { return Path.begin(); }
  iterator end() { return Path.end(); }
  const_iterator begin() const { return Path.begin(); }
  const_iterator end() const { return Path.end(); }

  reverse_iterator rbegin() { return Path.rbegin(); }
  reverse_iterator rend() { return Path.rend(); }
  const_reverse_iterator rbegin() const { return Path.rbegin(); }
  const_reverse_iterator rend() const { return Path.rend(); }
};

class ProjectionTree;

class ProjectionTreeNode {
  friend class ProjectionTree;

  /// The index of the current node in the tree. Can be used to lookup this node
  /// from the ProjectionTree. The reason why we use an Index instead of a
  /// pointer is that the SmallVector that we use to store these can reallocate
  /// invalidating our pointers.
  unsigned Index;

  /// The base type from which this projection tree node is derived via Proj. It
  /// is necessary to maintain a separate such entry from BaseValues since we
  /// may have projection tree nodes without any BaseValues since we completely
  /// explode non-enum scalar values to the leafs of our tree.
  ///
  /// In the root of the tree, this is the actual type.
  SILType BaseType;

  /// The base values we are tracking for which there is a Proj projection from.
  llvm::SmallVector<SILValue, 4> BaseValues;

  /// The projection that this node represents. None in the root.
  llvm::Optional<Projection> Proj;

  /// The index of the parent of this projection tree node in the projection
  /// tree. None in the root.
  llvm::Optional<unsigned> Parent;

  /// The list of 'non-projection' users of this projection.
  ///
  /// *NOTE* This also includes projections like enums we do not handle.
  llvm::SmallVector<Operand *, 4> NonProjUsers;

  /// The indices of the child projections of this node. Each one of these
  /// projections is associated with a field type from BaseType and will contain
  /// references to
  llvm::SmallVector<unsigned, 4> ChildProjections;

  /// Flag to see if ChildProjections have been initialized.
  bool Initialized;

  /// The index to the first ancestor of this node in the projection tree with a
  /// non-projection user.
  ///
  /// The reason that we track this information is that all nodes below such an
  /// ancestor must necessarily be alive. This makes it easy to fold together
  /// "levels" of leaf nodes recursively to create aggregate structures until we
  /// get to this ancestor.
  bool IsLive;

  enum {
    RootIndex = 0
  };

  /// Constructor for the root of the tree.
  ProjectionTreeNode(SILType BaseTy)
    : Index(0), BaseType(BaseTy), BaseValues(), Proj(), Parent(),
      NonProjUsers(), ChildProjections(), Initialized(false), IsLive(false) {}

  // Normal constructor for non-root nodes.
  ProjectionTreeNode(ProjectionTreeNode *Parent, unsigned Index, SILType BaseTy,
                     Projection P)
    : Index(Index), BaseType(BaseTy), BaseValues(), Proj(P),
      Parent(Parent->getIndex()), NonProjUsers(), ChildProjections(),
      Initialized(false), IsLive(false) {}

public:
  class AggregateBuilder;

  ~ProjectionTreeNode() = default;
  ProjectionTreeNode(const ProjectionTreeNode &) = default;

  bool isRoot() const {
    // Root does not have a parent. So if we have a parent, we can not be root.
    if (Parent.hasValue()) {
      assert(Proj.hasValue() && "If parent is not none, then P should be not "
             "none");
      assert(Index != RootIndex && "If parent is not none, we can not be root");
      return false;
    } else {
      assert(!Proj.hasValue() && "If parent is none, then P should be none");
      assert(Index == RootIndex && "Index must be root index");
      return true;
    }
  }

  SILType getType() const {
    if (isRoot())
      return BaseType;
    return Proj.getValue().getType();
  }

  ProjectionTreeNode *getChildForProjection(ProjectionTree &Tree,
                                            const Projection &P);

  NullablePtr<SILInstruction> createProjection(SILBuilder &B, SILLocation Loc,
                                               SILValue Arg) const;

  SILInstruction *
  createAggregate(SILBuilder &B, SILLocation Loc,
                  ArrayRef<SILValue> Args) const;

  unsigned getIndex() const { return Index; }

  ProjectionTreeNode *getParent(ProjectionTree &Tree);
  const ProjectionTreeNode *getParent(const ProjectionTree &Tree) const;

  ProjectionTreeNode *getParentOrNull(ProjectionTree &Tree) {
    if (!Parent.hasValue())
      return nullptr;
    return getParent(Tree);
  }

  llvm::Optional<Projection> getProjection() const { return Proj; }

private:
  void addNonProjectionUser(Operand *Op) {
    IsLive = true;
    NonProjUsers.push_back(Op);
  }

  using ValueNodePair = std::pair<SILValue, ProjectionTreeNode *>;

  void processUsersOfValue(ProjectionTree &Tree,
                           llvm::SmallVectorImpl<ValueNodePair> &Worklist,
                           SILValue Value);


  void
  createChildren(ProjectionTree &Tree,
                 llvm::SmallVectorImpl<ProjectionTreeNode *> &Worklist);

  void
  createChildrenForStruct(ProjectionTree &Tree,
                          llvm::SmallVectorImpl<ProjectionTreeNode *> &Worklist,
                          StructDecl *SD);

  void
  createChildrenForTuple(ProjectionTree &Tree,
                         llvm::SmallVectorImpl<ProjectionTreeNode *> &Worklist,
                         TupleType *TT);
};

class ProjectionTree {
  friend class ProjectionTreeNode;

  SILModule &Mod;

  llvm::BumpPtrAllocator &Allocator;
  llvm::SmallVector<ProjectionTreeNode *, 32> ProjectionTreeNodes;
  llvm::SmallVector<unsigned, 16> LeafIndices;

public:
  /// Construct a projection tree from BaseTy.
  ProjectionTree(SILModule &Mod, llvm::BumpPtrAllocator &Allocator,
                 SILType BaseTy);

  /// Compute liveness and use information in this projection tree using Base.
  void computeUsesAndLiveness(SILValue Base);

  /// Return the module associated with this tree.
  SILModule &getModule() const { return Mod; }

  /// Iterate over all values in the tree. The function should return false if
  /// it wants the iteration to end and true if it wants to continue.
  void visitProjectionTreeNodes(std::function<bool (ProjectionTreeNode &)> F);

  ProjectionTreeNode *getRoot() {
    return getNode(ProjectionTreeNode::RootIndex);
  }
  const ProjectionTreeNode *getRoot() const {
    return getNode(ProjectionTreeNode::RootIndex);
  }

  ProjectionTreeNode *getNode(unsigned i) {
    return ProjectionTreeNodes[i];
  }

  const ProjectionTreeNode *getNode(unsigned i) const {
    return ProjectionTreeNodes[i];
  }

  bool canExplodeValue() const {
    return ProjectionTreeNodes.size() > 1;
  }

  void getLeafTypes(llvm::SmallVectorImpl<SILType> &OutArray) const {
    for (unsigned LeafIndex : LeafIndices) {
      const ProjectionTreeNode *Node = getNode(LeafIndex);
      assert(Node->IsLive && "We are only interested in leafs that are live");
      OutArray.push_back(Node->getType());
    }
  }

  void createTreeFromValue(SILBuilder &B, SILLocation Loc, SILValue NewBase,
                           llvm::SmallVectorImpl<SILValue> &Leafs) const;

  void
  replaceValueUsesWithLeafUses(SILBuilder &B, SILLocation Loc,
                               llvm::SmallVectorImpl<SILValue> &Leafs);

private:

  void createRoot(SILType BaseTy) {
    assert(ProjectionTreeNodes.empty() &&
           "Should only create root when ProjectionTreeNodes is empty");
    auto *Node = new (Allocator) ProjectionTreeNode(BaseTy);
    ProjectionTreeNodes.push_back(Node);
  }  

  ProjectionTreeNode *createChild(ProjectionTreeNode *Parent,
                                  SILType BaseTy,
                                  const Projection &P) {
    unsigned Index = ProjectionTreeNodes.size();
    auto *Node = new (Allocator) ProjectionTreeNode(Parent, Index, BaseTy, P);
    ProjectionTreeNodes.push_back(Node);
    return ProjectionTreeNodes[Index];
  }

  ProjectionTreeNode *
  createChildForStruct(ProjectionTreeNode *Parent, SILType Ty, ValueDecl *VD,
                       unsigned Index) {
    Projection P = Projection(ProjectionKind::Struct, Ty, VD, Index);
    ProjectionTreeNode *N = createChild(Parent, Ty, P);
    return N;
  }

  ProjectionTreeNode *
  createChildForClass(ProjectionTreeNode *Parent, SILType Ty, ValueDecl *VD,
                      unsigned Index) {
    Projection P = Projection(ProjectionKind::Class, Ty, VD, Index);
    ProjectionTreeNode *N = createChild(Parent, Ty, P);
    return N;
  }

  ProjectionTreeNode *
  createChildForTuple(ProjectionTreeNode *Parent, SILType Ty, unsigned Index) {
    Projection P = Projection(ProjectionKind::Tuple, Ty, nullptr, Index);
    ProjectionTreeNode *N = createChild(Parent, Ty, P);
    return N;
  }
};

} // end swift namespace

#endif

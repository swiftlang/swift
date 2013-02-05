//===--- TypeCheckConstraints.cpp - Constraint-based Type Checking --------===//
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
// This file implements constraint-based type checking, including type
// inference.
//
//===----------------------------------------------------------------------===//
#include "TypeChecker.h"
#include "swift/AST/ASTVisitor.h"
#include "swift/AST/ASTWalker.h"
#include "swift/AST/Attr.h"
#include "swift/AST/NameLookup.h"
#include "llvm/ADT/APInt.h"
#include "llvm/ADT/DenseMap.h"
#include "llvm/ADT/MapVector.h"
#include "llvm/ADT/SmallPtrSet.h"
#include "llvm/ADT/SmallVector.h"
#include "llvm/ADT/Statistic.h"
#include "llvm/Support/Allocator.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/Support/SaveAndRestore.h"
#include <iterator>
#include <map>
#include <memory>
#include <utility>
#include <tuple>

using namespace swift;
using llvm::SmallPtrSet;

namespace {
  class ConstraintSystem;
}

void *operator new(size_t bytes, ConstraintSystem& cs,
                   size_t alignment = 8);

//===--------------------------------------------------------------------===//
// Constraint solver statistics
//===--------------------------------------------------------------------===//
#define DEBUG_TYPE "Constraint solver"
STATISTIC(NumExploredConstraintSystems, "# of constraint systems explored");
STATISTIC(NumSimplifyIterations, "# of simplification iterations");
STATISTIC(NumSupertypeFallbacks, "# of supertype fallbacks");
STATISTIC(NumLameNonDefinitive, "# of type variables lamely non-definitive");

//===--------------------------------------------------------------------===//
// Type variable implementation.
//===--------------------------------------------------------------------===//
#pragma mark Type variable implementation

namespace {
  
/// \brief A handle that holds the saved state of a type variable, which
/// can be restored
class SavedTypeVariableBinding {
  /// \brief The type variable.
  TypeVariableType *TypeVar;

  /// \brief The parent or fixed type.
  llvm::PointerUnion<TypeVariableType *, TypeBase *> ParentOrFixed;

public:
  explicit SavedTypeVariableBinding(TypeVariableType *typeVar);

  /// \brief Restore the state of the type variable to the saved state.
  void restore();
};
  
}

/// \brief The implementation object for a type variable used within the
/// constraint-solving type checker.
///
/// The implementation object for a type variable contains information about
/// the type variable, where it was generated, what protocols it must conform
/// to, what specific types it might be and, eventually, the fixed type to
/// which it is assigned.
class TypeVariableType::Implementation {
  /// \brief The unique number assigned to this type variable.
  unsigned ID;

  /// \brief The archetype that this type variable describes.
  ArchetypeType *Archetype;

  /// \brief Either the parent of this type variable within an equivalence
  /// class of type variables, or the fixed type to which this type variable
  /// type is bound.
  llvm::PointerUnion<TypeVariableType *, TypeBase *> ParentOrFixed;

  friend class SavedTypeVariableBinding;

public:
  explicit Implementation(unsigned ID)
    : ID(ID), Archetype(nullptr),
      ParentOrFixed(getTypeVariable()) { }

  explicit Implementation(unsigned ID, Expr *TheExpr)
    : ID(ID), Archetype(nullptr),
      ParentOrFixed(getTypeVariable()) { }

  explicit Implementation(unsigned ID, ArchetypeType *Archetype)
    : ID(ID), Archetype(Archetype),
      ParentOrFixed(getTypeVariable()) { }

  /// \brief Retrieve the unique ID corresponding to this type variable.
  unsigned getID() const { return ID; }
  
  /// \brief Retrieve the archetype that this type variable replaced.
  ArchetypeType *getArchetype() const { return Archetype; }

  /// \brief Retrieve the type variable associated with this implementation.
  TypeVariableType *getTypeVariable() {
    return reinterpret_cast<TypeVariableType *>(this) - 1;
  }

  /// \brief Retrieve the state of this type variable.
  SavedTypeVariableBinding getSavedBinding() {
    return SavedTypeVariableBinding(getTypeVariable());
  }

  /// \brief Retrieve the representative of the equivalence class to which this
  /// type variable belongs.
  ///
  /// \param record The record of changes made by retrieving the representative,
  /// which can happen due to path compression. If null, path compression is
  /// not performed.
  TypeVariableType *
  getRepresentative(SmallVectorImpl<SavedTypeVariableBinding> *record) {
    // Find the representative type variable.
    auto result = getTypeVariable();
    Implementation *impl = this;
    while (impl->ParentOrFixed.is<TypeVariableType *>()) {
      // Extract the representative.
      auto nextTV = impl->ParentOrFixed.get<TypeVariableType *>();
      if (nextTV == result)
        break;

      result = nextTV;
      impl = &nextTV->getImpl();
    }

    if (impl == this || !record)
      return result;

    // Perform path compression.
    impl = this;
    while (impl->ParentOrFixed.is<TypeVariableType *>()) {
      // Extract the representative.
      auto nextTV = impl->ParentOrFixed.get<TypeVariableType *>();
      if (nextTV == result)
        break;

      // Record the state change.
      record->push_back(impl->getSavedBinding());

      impl->ParentOrFixed = result;
      impl = &nextTV->getImpl();
    }

    return result;
  }

  /// \brief Merge the equivalence class of this type variable with the
  /// equivalence class of another type variable.
  ///
  /// \param other The type variable to merge with.
  ///
  /// \param record The record of state changes.
  void mergeEquivalenceClasses(TypeVariableType *other,
                               SmallVectorImpl<SavedTypeVariableBinding> &record) {
    // Merge the equivalence classes corresponding to these two type
    // variables. Always merge 'up' the constraint stack, because it is simpler.
    if (getID() < other->getImpl().getID()) {
      auto rep = other->getImpl().getRepresentative(&record);
      record.push_back(rep->getImpl().getSavedBinding());
      rep->getImpl().ParentOrFixed = getTypeVariable();
    } else {
      auto rep = getRepresentative(&record);
      record.push_back(rep->getImpl().getSavedBinding());
      rep->getImpl().ParentOrFixed = other;
    }
  }

  /// \brief Retrieve the fixed type that corresponds to this type variable,
  /// if there is one.
  ///
  /// \returns the fixed type associated with this type variable, or a null
  /// type if there is no fixed type.
  ///
  /// \param record The record of changes made by retrieving the representative,
  /// which can happen due to path compression. If null, path compression is
  /// not performed.
  Type getFixedType(SmallVectorImpl<SavedTypeVariableBinding> *record) {
    // Find the representative type variable.
    Implementation *impl = this;
    while (impl->ParentOrFixed.is<TypeVariableType *>()) {
      auto nextTV = impl->ParentOrFixed.get<TypeVariableType *>();

      // If we found the representative, there is no fixed type.
      if (nextTV == impl->getTypeVariable()) {
        return Type();
      }

      impl = &nextTV->getImpl();
    }

    Type result = impl->ParentOrFixed.get<TypeBase *>();
    if (impl == this || !record)
      return result;

    // Perform path compression.
    impl = this;
    while (impl->ParentOrFixed.is<TypeVariableType *>()) {
      // Extract the representative.
      auto nextTV = impl->ParentOrFixed.get<TypeVariableType *>();
      if (nextTV == impl->getTypeVariable())
        return result;

      record->push_back(impl->getSavedBinding());
      impl->ParentOrFixed = result.getPointer();
      impl = &nextTV->getImpl();
    }

    return result;
  }

  /// \brief Assign a fixed type to this equivalence class.
  void assignFixedType(Type type, SmallVectorImpl<SavedTypeVariableBinding> &record) {
    assert((!getFixedType(0) || getFixedType(0)->isEqual(type)) &&
           "Already has a fixed type!");
    auto rep = getRepresentative(&record);
    record.push_back(rep->getImpl().getSavedBinding());
    rep->getImpl().ParentOrFixed = type.getPointer();
  }

  void print(llvm::raw_ostream &Out) {
    Out << "$T" << ID;
  }
};

template<typename ...Args>
TypeVariableType *TypeVariableType::getNew(ASTContext &C, Args &&...args) {
  // FIXME: Use the constraint-system's local allocator!

  // Allocate memory
  void *mem = C.Allocate(sizeof(TypeVariableType) + sizeof(Implementation),
                         alignof(TypeVariableType),
                         AllocationArena::ConstraintSolver);

  // Construct the type variable.
  auto *result = ::new (mem) TypeVariableType(C);

  // Construct the implementation object.
  new (result+1) TypeVariableType::Implementation(std::forward<Args>(args)...);
  
  return result;
}

void TypeVariableType::print(raw_ostream &OS) const {
  OS << "$T" << getImpl().getID();
}

SavedTypeVariableBinding::SavedTypeVariableBinding(TypeVariableType *typeVar)
  : TypeVar(typeVar), ParentOrFixed(typeVar->getImpl().ParentOrFixed) { }

void SavedTypeVariableBinding::restore() {
  TypeVar->getImpl().ParentOrFixed = ParentOrFixed;
}

//===--------------------------------------------------------------------===//
// Constraints
//===--------------------------------------------------------------------===//
#pragma mark Constraints

namespace {
  /// \brief Describes the kind of constraint placed on one or more types.
  enum class ConstraintKind : char {
    /// \brief The two types must be bound to the same type. This is the only
    /// truly symmetric constraint.
    Bind,
    /// \brief The two types must be bound to the same type, dropping
    /// lvalueness when comparing a type variable to a type.
    Equal,
    /// \brief The first type is the rvalue of the second type.
    EqualRvalue,
    /// \brief The first type is a "trivial" subtype of the second type,
    /// meaning that it is a subtype that is also guaranteed to have the same
    // in-memory representation.
    TrivialSubtype,
    /// \brief The first type is a subtype of the second type, i.e., a value
    /// of the type of the first type can be used wherever a value of the
    /// second type is expected.
    Subtype,
    /// \brief The first type is convertible to the second type.
    Conversion,
    /// \brief The first type can be converted to the second type or can be
    /// used as an argument to a constructor for the second (non-reference)
    /// type.
    Construction,
    /// \brief The first type is the type of a literal. There is no second
    /// type.
    Literal,
    /// \brief The first type has a member with the given name, and the
    /// type of that member, when referenced as a value, is the second type.
    ValueMember,
    /// \brief The first type has a type member with the given name, and the
    /// type of that member, when referenced as a type, is the second type.
    TypeMember,
    /// \brief The first type must be an archetype.
    Archetype
  };

  /// \brief Classification of the different kinds of constraints.
  enum class ConstraintClassification : char {
    /// \brief A relational constraint, which relates two types.
    Relational,

    /// \brief A literal constraint, which specifies that a type must be
    /// able to be created from a particular kind of literal.
    Literal,

    /// \brief A member constraint, which names a member of a type and assigns
    /// it a reference type.
    Member,

    /// \brief An archetype constraint, which simply requires that the type
    /// variable be bound to an archetype.
    Archetype
  };

  /// \brief A constraint between two type variables.
  class Constraint {
    /// \brief The kind of constraint.
    ConstraintKind Kind;

    /// \brief The first type.
    Type First;

    /// \brief The second type.
    Type Second;

    /// \brief If non-null, the name of a member of the first type is that
    /// being related to the second type.
    Identifier Member;

    /// \brief For a literal-type constraint, the kind of literal we're
    /// expecting.
    LiteralKind Literal;

    /// \brief For a constraint that was directly generated from an expression,
    /// the expression from which it was generated.
    ///
    /// FIXME: We need more information for cases where a given expression
    /// might have generated several constraints. Which one is it?
    Expr *Anchor;

    /// \brief Constraints are always allocated within a given constraint
    /// system.
    void *operator new(size_t) = delete;

  public:
    Constraint(ConstraintKind Kind, Type First, Type Second, Identifier Member,
               Expr *anchor)
      : Kind(Kind), First(First), Second(Second), Member(Member), Anchor(anchor)
    {
      switch (Kind) {
      case ConstraintKind::Bind:
      case ConstraintKind::Equal:
      case ConstraintKind::EqualRvalue:
      case ConstraintKind::TrivialSubtype:
      case ConstraintKind::Subtype:
      case ConstraintKind::Conversion:
      case ConstraintKind::Construction:
        assert(Member.empty() && "Relational constraint cannot have a member");
        break;

      case ConstraintKind::Literal:
        llvm_unreachable("Wrong constructor for literal constraint");
        break;

      case ConstraintKind::TypeMember:
      case ConstraintKind::ValueMember:
        assert(!Member.empty() && "Member constraint has no member");
        break;

      case ConstraintKind::Archetype:
        assert(Member.empty() && "Archetype constraint cannot have a member");
        assert(Second.isNull() && "Archetype constraint with second type");
        break;
      }
    }

    Constraint(Type type, LiteralKind literal, Expr *anchor = nullptr)
      : Kind(ConstraintKind::Literal), First(type), Literal(literal),
        Anchor(anchor){ }

    // FIXME: Need some context information here.

    /// \brief Determine the kind of constraint.
    ConstraintKind getKind() const { return Kind; }

    /// \brief Determine the classification of this constraint, providing
    /// a broader categorization than \c getKind().
    ConstraintClassification getClassification() const {
      switch (Kind) {
      case ConstraintKind::Bind:
      case ConstraintKind::Equal:
      case ConstraintKind::EqualRvalue:
      case ConstraintKind::TrivialSubtype:
      case ConstraintKind::Subtype:
      case ConstraintKind::Conversion:
      case ConstraintKind::Construction:
        return ConstraintClassification::Relational;

      case ConstraintKind::Literal:
        return ConstraintClassification::Literal;

      case ConstraintKind::ValueMember:
      case ConstraintKind::TypeMember:
        return ConstraintClassification::Member;

      case ConstraintKind::Archetype:
        return ConstraintClassification::Archetype;
      }
    }

    /// \brief Retrieve the first type in the constraint.
    Type getFirstType() const { return First; }

    /// \brief Retrieve the second type in the constraint.
    Type getSecondType() const {
      assert(Kind != ConstraintKind::Literal &&
             "No second type for literal constraints");
      return Second;
    }

    /// \brief Retrieve the name of the member for a member constraint.
    Identifier getMember() const {
      assert(Kind == ConstraintKind::ValueMember ||
             Kind == ConstraintKind::TypeMember);
      return Member;
    }

    /// \brief Determine the kind of literal for a literal constraint.
    LiteralKind getLiteralKind() const {
      assert(Kind == ConstraintKind::Literal && "Not a literal constraint!");
      return Literal;
    }

    /// \brief Retrieve the expression that directly generated this
    /// constraint.
    Expr *getAnchorExpr() const { return Anchor; }

    void print(llvm::raw_ostream &Out) {
      First->print(Out);
      switch (Kind) {
      case ConstraintKind::Bind: Out << " := "; break;
      case ConstraintKind::Equal: Out << " == "; break;
      case ConstraintKind::EqualRvalue: Out << " ==R "; break;
      case ConstraintKind::TrivialSubtype: Out << " <t "; break;
      case ConstraintKind::Subtype: Out << " < "; break;
      case ConstraintKind::Conversion: Out << " <c "; break;
      case ConstraintKind::Construction: Out << " <C "; break;
      case ConstraintKind::Literal:
        Out << " is ";
        switch (getLiteralKind()) {
        case LiteralKind::ASCIIString:
          Out << "an ASCII string";
          break;

        case LiteralKind::Char:
          Out << "a character";
          break;

        case LiteralKind::Float:
          Out << "a floating-point";
          break;

        case LiteralKind::Int:
          Out << "an integer";
          break;

        case LiteralKind::UTFString:
          Out << "a UTF-8 string";
          break;
        }
        Out << " literal";
        return;
      case ConstraintKind::ValueMember:
        Out << "[." << Member.str() << ": value] == ";
        break;
      case ConstraintKind::TypeMember:
        Out << "[." << Member.str() << ": type] == ";
        break;
      case ConstraintKind::Archetype:
        Out << " is an archetype";
        return;
      }

      Second->print(Out);
    }

    void dump() LLVM_ATTRIBUTE_USED { print(llvm::errs()); }

    void *operator new(size_t bytes, ConstraintSystem& cs,
                       size_t alignment = alignof(Constraint)) {
      return ::operator new (bytes, cs, alignment);
    }

    inline void operator delete(void *, const ConstraintSystem &cs, size_t) {}
  };

  /// \brief The kind of overload choice.
  enum class OverloadChoiceKind : int {
    /// \brief The overload choice selects a particular declaration from a
    /// set of declarations.
    Decl,
    /// \brief The overload choice equates the member type with the
    /// base type. Used for unresolved member expressions like ".none" that
    /// refer to oneof members with unit type.
    BaseType,
    /// \brief The overload choice equates the member type with a function
    /// of arbitrary input type whose result type is the base type. Used for
    /// unresolved member expressions like ".internal" that refer to oneof
    /// members with non-unit type.
    FunctionReturningBaseType,
    /// \brief The overload choice equates the member type with a function
    /// from the base type to itself.
    IdentityFunction,
    /// \brief The overload choice indexes into a tuple. Index zero will
    /// have the value of this enumerator, index one will have the value of this
    /// enumerator + 1, and so on. Thus, this enumerator must always be last.
    TupleIndex
  };

  /// \brief Describes a particular choice within an overload set.
  ///
  /// 
  class OverloadChoice {
    /// \brief The base type to be used when referencing the declaration.
    Type Base;

    /// \brief Either the declaration pointer (if the low bit is clear) or the
    /// overload choice kind shifted by 1 with the low bit set.
    uintptr_t DeclOrKind;

  public:
    OverloadChoice(Type base, ValueDecl *value) : Base(base) {
      DeclOrKind = reinterpret_cast<uintptr_t>(value);
      assert((DeclOrKind & (uintptr_t)0x01) == 0 && "Badly aligned decl");
    }

    OverloadChoice(Type base, OverloadChoiceKind kind)
      : Base(base), DeclOrKind((uintptr_t)kind << 1 | (uintptr_t)0x01)
    {
      assert(base && "Must have a base type for overload choice");
      assert(kind != OverloadChoiceKind::Decl && "wrong constructor for decl");
    }

    OverloadChoice(Type base, unsigned index)
      : Base(base),
        DeclOrKind(((uintptr_t)index
                    + (uintptr_t)OverloadChoiceKind::TupleIndex) << 1
                   | (uintptr_t)0x01) {
      assert(base->getRValueType()->is<TupleType>() && "Must have tuple type");
    }

    /// \brief Retrieve the base type used to refer to the declaration.
    Type getBaseType() const { return Base; }

    /// \brief Determines the kind of overload choice this is.
    OverloadChoiceKind getKind() const {
      if (DeclOrKind & 0x01) {
        uintptr_t value = DeclOrKind >> 1;
        if (value >= (uintptr_t)OverloadChoiceKind::TupleIndex)
          return OverloadChoiceKind::TupleIndex;

        return (OverloadChoiceKind)value;
      }
      
      return OverloadChoiceKind::Decl;
    }

    /// \brief Retrieve the declaraton that corresponds to this overload choice.
    ValueDecl *getDecl() const {
      assert(getKind() == OverloadChoiceKind::Decl && "Not a declaration");
      return reinterpret_cast<ValueDecl *>(DeclOrKind);
    }

    /// \brief Retrieve the tuple index that corresponds to this overload
    /// choice.
    unsigned getTupleIndex() const {
      assert(getKind() == OverloadChoiceKind::TupleIndex);
      return (DeclOrKind >> 1) - (uintptr_t)OverloadChoiceKind::TupleIndex;
    }
  };

  /// \brief An overload set, which is a set of overloading choices from which
  /// only one can be selected.
  class OverloadSet {
    /// \brief ID number that uniquely identifies this overload set.
    unsigned ID;

    /// \brief The number of choices in the overload set.
    unsigned NumChoices;

    /// \brief The expression or constraint that introduced the overload choice.
    llvm::PointerUnion<Expr *, const Constraint *> ExprOrConstraint;

    /// \brief The type bound by this overload set.
    Type BoundType;
    
    /// \brief Overload sets are always allocated within a given constraint
    /// system.
    void *operator new(size_t) = delete;

    OverloadSet(unsigned ID,
                llvm::PointerUnion<Expr *, const Constraint *> from,
                Type boundType,
                ArrayRef<OverloadChoice> choices)
      : ID(ID), NumChoices(choices.size()), ExprOrConstraint(from),
        BoundType(boundType)
    {
      memmove(this+1, choices.data(), sizeof(OverloadChoice)*choices.size());
    }

  public:
    /// \brief Retrieve the expression that introduced this overload set, or
    /// null if it was not introduced by an expression.
    Expr *getIntroducingExpr() const {
      return ExprOrConstraint.dyn_cast<Expr *>();
    }

    /// \brief Retrieve the constraint that introduced this overload set, or
    /// null if it was not introduced by a constraint.
    const Constraint *getIntroducingConstraint() const {
      return ExprOrConstraint.dyn_cast<const Constraint *>();
    }

    /// \brief Retrieve the ID associated with this overload set.
    unsigned getID() const { return ID; }
    
    /// \brief Retrieve the set of choices provided by this overload set.
    ArrayRef<OverloadChoice> getChoices() const {
      return { reinterpret_cast<const OverloadChoice *>(this + 1),
               NumChoices };
    }

    /// \brief Retrieve the type that is bound (via a same-type
    /// constraint) by this overload set.
    Type getBoundType() const { return BoundType; }

    /// \brief Create a new overload set, using (and copying) the given choices.
    static OverloadSet *getNew(ConstraintSystem &CS,
                               Type boundType,
                               Expr *expr,
                               ArrayRef<OverloadChoice> choices);

    /// \brief Create a new overload set, using (and copying) the given choices.
    static OverloadSet *getNew(ConstraintSystem &CS,
                               Type boundType,
                               const Constraint *constraint,
                               ArrayRef<OverloadChoice> choices);
  };

  /// \brief A representative type variable with the list of constraints
  /// that apply to it.
  struct TypeVariableConstraints {
    TypeVariableConstraints(TypeVariableType *typeVar)
      : HasNonConcreteConstraints(false), TypeVar(typeVar) {}

    /// \brief Whether there are any non-concrete constraints placed on this
    /// type variable that aren't represented by the stored \c Constraints.
    bool HasNonConcreteConstraints;

    /// \brief The representative type variable.
    TypeVariableType *TypeVar;

    /// \brief The set of constraints "above" the type variable.
    SmallVector<std::pair<Constraint *, Type>, 4> Above;

    /// \brief The set of constraints "below" the type variable.
    SmallVector<std::pair<Constraint *, Type>, 4> Below;

    /// \brief The set of archetype and literal constraints directly applicable
    /// to the type variable T.
    SmallVector<Constraint *, 4> KindConstraints;
  };

  //===--------------------------------------------------------------------===//
  // Constraint system
  //===--------------------------------------------------------------------===//
#pragma mark Constraint system

  /// \brief The kind of type matching to perform in matchTypes().
  enum class TypeMatchKind : char {
    /// \brief Bind the types together directly.
    BindType,
    /// \brief Require the types to match exactly, but strips lvalueness from
    /// a type when binding to a type variable.
    SameType,
    /// \brief Require the type of the first to match the rvalue type of the
    /// second.
    SameTypeRvalue,
    /// \brief Require the first type to be a "trivial" subtype of the second
    /// type or be an exact match.
    TrivialSubtype,
    /// \brief Require the first type to be a subtype of the second type
    /// (or be an exact match or trivial subtype).
    Subtype,
    /// \brief Requires the first type to be convertible to the second type,
    /// which includes exact matches and both forms of subtyping.
    Conversion,
    /// \brief Requires the first type to either be convertible to the second
    /// type or to be a suitable constructor argument for the second type.
    Construction
  };

  /// \brief The result of comparing two constraint systems that are a solutions
  /// to the given set of constraints.
  enum class SolutionCompareResult {
    /// \brief The two solutions are incomparable, because, e.g., because one
    /// solution has some better decisions and some worse decisions than the
    /// other.
    Incomparable,
    /// \brief The two solutions are identical.
    Identical,
    /// \brief The first solution is better than the second.
    Better,
    /// \brief The second solution is better than the first.
    Worse
  };

  /// \brief Describes a system of constraints on type variables, the
  /// solution of which assigns concrete types to each of the type variables.
  /// Constraint systems are typically generated given an (untyped) expression.
  class ConstraintSystem {
    TypeChecker &TC;
    ConstraintSystem *Parent = nullptr;
    Constraint *failedConstraint = nullptr;

    /// \brief Contains state that is shared among all of the child constraint
    /// systems for a given type-checking problem.
    struct SharedStateType {
      /// \brief Allocator used for all of the related constraint systems.
      llvm::BumpPtrAllocator Allocator;

      /// \brief Arena used for memory management of constraint-checker-related
      /// allocations.
      ///
      /// This arena will automatically be destroyed when the top constraint
      /// system is destroyed.
      ConstraintCheckerArenaRAII Arena;

      /// \brief Counter for type variables introduced.
      unsigned TypeCounter = 0;

      /// \brief Counter for the overload sets introduced.
      unsigned OverloadSetCounter = 0;

      /// \brief Cached member lookups.
      llvm::DenseMap<std::pair<Type, Identifier>, std::unique_ptr<MemberLookup>>
        MemberLookups;

      /// \brief Cached literal checks.
      std::map<std::pair<TypeBase *, LiteralKind>, bool> LiteralChecks;

    public:
      SharedStateType(TypeChecker &tc) : Arena(tc.Context, Allocator) {}
    };

    /// \brief The state shared among all of the related constraint systems.
    SharedStateType *SharedState;

    // ---Track the state space we've already explored---
    /// \brief The number of child systems that are still active, i.e., haven't
    /// been found to be unsolvable.
    unsigned NumActiveChildren = 0;

    /// \brief Whether we have resolved an overload by creating child systems.
    bool ResolvedOverloadsInChildSystems = false;

    /// \brief Keeps track of the attempted type variable bindings we have
    /// performed in child systems.
    llvm::DenseMap<TypeVariableType *, llvm::SmallPtrSet<CanType, 16>>
      ExploredTypeBindings;

    /// \brief Keeps track of type variable bindings we should try next,
    /// assuming that none of the current set of type bindings pans out.
    ///
    /// These bindings tend to be for supertypes of type variable bindings
    /// we've already attempted.
    SmallVector<std::pair<TypeVariableType *, Type>, 16> PotentialBindings;

    // Valid everywhere
    
    /// \brief A mapping from each overload set that is resolved in this
    /// constraint system to a pair (index, type), where index is the index of
    /// the overload choice (within the overload set) and type is the type
    /// implied by that overload.
    llvm::DenseMap<OverloadSet *, std::pair<unsigned, Type>> ResolvedOverloads;

    /// \brief The type variable for which we are assuming a given particular
    /// type.
    TypeVariableType *assumedTypeVar = nullptr;

    enum { Active, Unsolvable, Solved } State = Active;

    SmallVector<TypeVariableType *, 16> TypeVariables;
    SmallVector<Constraint *, 16> Constraints;
    llvm::SmallPtrSet<Constraint *, 4> ExternallySolved;
    SmallVector<OverloadSet *, 4> UnresolvedOverloadSets;
    llvm::DenseMap<Expr *, OverloadSet *> GeneratedOverloadSets;
    SmallVector<std::unique_ptr<ConstraintSystem>, 2> Children;

    /// \brief The set of type variable bindings that have changed while
    /// processing this constraint system.
    SmallVector<SavedTypeVariableBinding, 16> SavedBindings;

    typedef llvm::PointerUnion<TypeVariableType *, TypeBase *>
      RepresentativeOrFixed;

    /// \brief For each type variable, maps to either the representative of that
    /// type variable or to the fixed type to which that type variable is
    /// bound.
    llvm::DenseMap<TypeVariableType *, RepresentativeOrFixed>
      TypeVariableInfo;

    // Valid everywhere, for debugging
    SmallVector<Constraint *, 16> SolvedConstraints;

    unsigned assignTypeVariableID() {
      return SharedState->TypeCounter++;
    }

    unsigned assignOverloadSetID() {
      return SharedState->OverloadSetCounter++;
    }
    friend class OverloadSet;

  public:
    ConstraintSystem(TypeChecker &tc)
      : TC(tc), SharedState(new SharedStateType(tc))
    {
      ++NumExploredConstraintSystems;
    }

    /// \brief Creates a child constraint system that selects a
    /// particular overload from an unresolved overload set.
    ///
    /// \param parent The parent constraint system.
    ///
    /// \param overloadSetIdx The index into the parent's unresolved
    /// overload set, selecting the overload to be resolved.
    ///
    /// \param overloadChoiceIdx The index into the unresolved
    /// overload set identified by \p overloadSetIdx. This index
    /// selects which overload choice this child system will explore.
    ConstraintSystem(ConstraintSystem *parent, 
                     unsigned overloadSetIdx,
                     unsigned overloadChoiceIdx)
      : TC(parent->TC), Parent(parent), SharedState(parent->SharedState),
        // FIXME: Lazily copy from parent system.
        Constraints(parent->Constraints)
    {
      ++NumExploredConstraintSystems;
      
      // Copy all of the parent's unresolved overload sets *except*
      // the one we're resolving in this child system.
      UnresolvedOverloadSets.append(
        parent->UnresolvedOverloadSets.begin(),
        parent->UnresolvedOverloadSets.begin() + overloadSetIdx);
      UnresolvedOverloadSets.append(
        parent->UnresolvedOverloadSets.begin() + overloadSetIdx + 1,
        parent->UnresolvedOverloadSets.end());

      // Resolve this overload, as requested by the caller.
      resolveOverload(parent->UnresolvedOverloadSets[overloadSetIdx], 
                      overloadChoiceIdx);
    }

    /// \brief Creates a child constraint system that binds a given
    /// type variable to a given type.
    ///
    /// \param parent The parent constraint system.
    ///
    /// \param typeVar The type variable whose binding we will be exploring
    /// within this child system.
    ///
    /// \param type The type to which type variable will be bound
    /// within this child system.
    ConstraintSystem(ConstraintSystem *parent, TypeVariableType *typeVar,
                     Type type)
      : TC(parent->TC), Parent(parent), SharedState(parent->SharedState),
        assumedTypeVar(typeVar),
        // FIXME: Lazily copy from parent system.
        Constraints(parent->Constraints),
        UnresolvedOverloadSets(parent->UnresolvedOverloadSets)
    {
      ++NumExploredConstraintSystems;
      
      addConstraint(ConstraintKind::Equal, typeVar, type);
    }

    ~ConstraintSystem() {
      if (!Parent)
        delete SharedState;
    }

    /// \brief Retrieve the type checker associated with this constraint system.
    TypeChecker &getTypeChecker() const { return TC; }

    /// \brief Retrieve the AST context.
    ASTContext &getASTContext() const { return TC.Context; }

    /// \brief Retrieve the top-level constraint system.
    ConstraintSystem &getTopConstraintSystem() {
      ConstraintSystem *cs = this;
      while (cs->Parent)
        cs = cs->Parent;
      return *cs;
    }

    /// \brief Retrieve the depth of this constraint system, i.e., the
    /// number of hops from this constraint system to the top-level constraint
    /// system.
    unsigned getConstraintSystemDepth() const {
      unsigned depth = 0;
      const ConstraintSystem *cs = this;
      while (cs->Parent) {
        ++depth;
        cs = cs->Parent;
      }
      return depth;
    }

    /// \brief Determine whether this constraint system has any child
    /// constraint systems, active or not.
    bool hasChildren() const { return !Children.empty(); }
    
    /// \brief Determine whether this constraint system has any active
    /// child constraint systems.
    bool hasActiveChildren() const { return NumActiveChildren > 0; }

    /// \brief Whether this system has resolved an overload by creating
    /// child systems.
    bool hasResolvedOverloadsInChildSystems() const {
      return ResolvedOverloadsInChildSystems;
    }

    /// \brief Determine whether this constraint system has any free type
    /// variables.
    bool hasFreeTypeVariables() {
      // Look for any free type variables.
      for (auto cs = this; cs; cs = cs->Parent) {
        for (auto tv : cs->TypeVariables) {
          // We only care about the representatives.
          if (getRepresentative(tv) != tv)
            continue;

          if (auto fixed = getFixedType(tv)) {
            if (simplifyType(fixed)->hasTypeVariable())
              return false;

            continue;
          }
          
          return true;
        }
      }
      
      return false;
    }

    /// \brief Retrieve the constraint that caused this system to fail.
    Constraint *getFailedConstraint() const { return failedConstraint; }

    /// \brief Create a new constraint system that is derived from this
    /// constraint system, referencing the rules of the parent system but
    /// also introducing its own (likely dependent) constraints.
    ///
    /// The new constraint system will be immediately simplified, and deleted
    /// if simplification fails.
    ///
    /// \returns the new constraint system, or null if simplification failed.
    template<typename ...Args>
    ConstraintSystem *createDerivedConstraintSystem(Args &&...args){
      ++NumActiveChildren;
      auto result = new ConstraintSystem(this, std::forward<Args>(args)...);

      // Attempt simplification of the resulting constraint system.
      if (result->simplify()) {
        // The constraint system constraints an error. Delete it now and
        // return a null pointer to indicate failure.
        result->restoreTypeVariableBindings();
        delete result;
        return nullptr;
      }
      
      // The system may be solvable. Record and return it.
      Children.push_back(std::unique_ptr<ConstraintSystem>(result));
      return result;
    }

  private:
    /// \brief Indicates that the given child constraint system is inactive
    /// (i.e., unsolvable).
    void markChildInactive(ConstraintSystem *childCS);

    /// \brief Indicates that this constraint system is unsolvable.
    void markUnsolvable() {
      State = Unsolvable;
      if (Parent)
        Parent->markChildInactive(this);
    }

    /// \brief Finalize this constraint system; we're done attempting to solve
    /// it.
    ///
    /// \returns true if this constraint system is unsolvable.
    bool finalize() {
      switch (State) {
      case Unsolvable:
        removeInactiveChildren();
        restoreTypeVariableBindings();
        return true;

      case Solved:
        llvm_unreachable("Already finalized (?)");
        break;

      case Active:
        if (hasActiveChildren()) {
          // There is a solution below, but by itself this isn't a solution.
          removeInactiveChildren();
          clearIntermediateData();
          restoreTypeVariableBindings();
          return true;
        }

        // Check whether we have a proper solution.
        if (Constraints.empty() && UnresolvedOverloadSets.empty() &&
            !hasFreeTypeVariables()) {
          State = Solved;
          makeStandalone();
          removeInactiveChildren();
          restoreTypeVariableBindings();
          return false;
        }

        // We don't have a solution;
        markUnsolvable();
        removeInactiveChildren();
          restoreTypeVariableBindings();
        return true;
      }
    }

  private:
    /// \brief Remove any inactive (== unsolvable) children.
    void removeInactiveChildren() {
      Children.erase(std::remove_if(Children.begin(), Children.end(),
                       [&](std::unique_ptr<ConstraintSystem> &child) -> bool {
                         return !child->hasActiveChildren() &&
                                (!child->Children.empty() ||
                                 !child->isSolved());
                       }),
                     Children.end());
    }

    /// \brief Make this constraint system 'standalone', in the sense that it
    /// will not need to look to its parent for any information.
    void makeStandalone() {
      decltype(ExploredTypeBindings)().swap(ExploredTypeBindings);
      PotentialBindings.clear();
      decltype(ExternallySolved)().swap(ExternallySolved);

      // For each of the type variables, get its fixed type.
      for (auto cs = this; cs; cs = cs->Parent) {
        for (auto tv : cs->TypeVariables) {
          TypeVariableInfo[tv] = getFixedType(tv).getPointer();
        }
      }
    }

    /// \brief Clear out any 'intermediate' data that is no longer useful when
    /// all of the child systems are standalone.
    void clearIntermediateData() {
      decltype(ExploredTypeBindings)().swap(ExploredTypeBindings);
      PotentialBindings.clear();
      decltype(ExternallySolved)().swap(ExternallySolved);
      decltype(TypeVariableInfo)().swap(TypeVariableInfo);
    }

    /// \brief Restore the type variable bindings to what they were before
    /// we attempted to solve this constraint system.
    void restoreTypeVariableBindings() {
      std::for_each(SavedBindings.rbegin(), SavedBindings.rend(),
                    [](SavedTypeVariableBinding &saved) {
                      saved.restore();
                    });
      SavedBindings.clear();
    }

    /// \brief Take the permanent type variable bindings and push them into
    /// the type variables.
    void injectPermanentTypeVariableBindings() {
      for (const auto &tvi : TypeVariableInfo) {
        tvi.first->getImpl().assignFixedType(tvi.second.get<TypeBase *>(),
                                             SavedBindings);
      }
    }

    friend class ReinstateTypeVariableBindingsRAII;

    /// \brief Lookup for a member with the given name in the given base type.
    ///
    /// This routine caches the results of member lookups in the top constraint
    /// system, to avoid.
    ///
    /// FIXME: This caching should almost certainly be performed at the
    /// translation unit level, since type checking occurs after name binding,
    /// and no new names are introduced after name binding.
    ///
    /// \returns A reference to the member-lookup result.
    MemberLookup &lookupMember(Type base, Identifier name) {
      base = base->getCanonicalType();
      auto &ptr = SharedState->MemberLookups[{base, name}];
      if (!ptr)
        ptr.reset(new MemberLookup(base, name, TC.TU));
      return *ptr;
    }

  public:
    /// \brief Retrieve an unresolved overload set.
    OverloadSet *getUnresolvedOverloadSet(unsigned Idx) const {
      return UnresolvedOverloadSets[Idx];
    }

    /// \brief Determine the number of unresolved overload sets in
    /// this constraint system.
    unsigned getNumUnresolvedOverloadSets() const {
      return UnresolvedOverloadSets.size();
    }

    /// \brief Determine whether this constraint system has any potential
    /// bindings.
    bool hasPotentialBindings() const {
      return !PotentialBindings.empty();
    }

    /// \brief Create a new type variable.
    template<typename ...Args>
    TypeVariableType *createTypeVariable(Args &&...args) {
      auto tv = TypeVariableType::getNew(TC.Context, assignTypeVariableID(),
                                         std::forward<Args>(args)...);
      TypeVariables.push_back(tv);
      return tv;
    }

    /// \brief Add a newly-allocated constraint after attempting to simplify
    /// it.
    ///
    /// \returns true if this constraint was solved.
    bool addConstraint(Constraint *constraint,
                       bool isExternallySolved = false) {
      switch (simplifyConstraint(*constraint)) {
      case SolutionKind::Error:
        if (!failedConstraint) {
          failedConstraint = constraint;
          markUnsolvable();
        }
        return false;

      case SolutionKind::TriviallySolved:
      case SolutionKind::Solved:
        // This constraint has already been solved; there is nothing more
        // to do.
        if (TC.getLangOpts().DebugConstraintSolver)
          SolvedConstraints.push_back(constraint);

        if (isExternallySolved)
          ExternallySolved.insert(constraint);
        return true;

      case SolutionKind::Unsolved:
        // We couldn't solve this constraint; add it to the pile.
        if (!isExternallySolved)
          Constraints.push_back(constraint);
        return false;
      }
    }

    /// \brief Add a constraint to the constraint system.
    void addConstraint(ConstraintKind kind, Type first, Type second,
                       Expr *anchor = nullptr) {
      assert(first && "Missing first type");
      assert(second && "Missing first type");
      addConstraint(new (*this) Constraint(kind, first, second, Identifier(),
                                           anchor));
    }

    ///\ brief Add a literal constraint to the constraint system.
    void addLiteralConstraint(Type type, LiteralKind kind,
                              Expr *anchor = nullptr) {
      assert(type && "missing type for literal constraint");
      addConstraint(new (*this) Constraint(type, kind, anchor));
    }

    /// \brief Add a value member constraint to the constraint system.
    void addValueMemberConstraint(Type baseTy, Identifier name, Type memberTy,
                                  Expr *anchor = nullptr) {
      assert(baseTy);
      assert(memberTy);
      assert(!name.empty());
      addConstraint(new (*this) Constraint(ConstraintKind::ValueMember,
                                           baseTy, memberTy, name, anchor));
    }

    /// \brief Add a type member constraint to the constraint system.
    void addTypeMemberConstraint(Type baseTy, Identifier name, Type memberTy,
                                 Expr *anchor = nullptr) {
      assert(baseTy);
      assert(memberTy);
      assert(!name.empty());
      addConstraint(new (*this) Constraint(ConstraintKind::TypeMember,
                                           baseTy, memberTy, name, anchor));
    }

    /// \brief Add an archetype constraint.
    void addArchetypeConstraint(Type baseTy, Expr *anchor = nullptr) {
      assert(baseTy);
      addConstraint(new (*this) Constraint(ConstraintKind::Archetype,
                                           baseTy, Type(), Identifier(),
                                           anchor));
    }

    /// \brief Retrieve the representative of the equivalence class containing
    /// this type variable.
    TypeVariableType *getRepresentative(TypeVariableType *typeVar) {
      return typeVar->getImpl().getRepresentative(&SavedBindings);
    }

    /// \brief Merge the equivalence sets of the two type variables.
    ///
    /// Note that both \c typeVar1 and \c typeVar2 must be the
    /// representatives of their equivalence classes, and must be
    /// distinct.
    void mergeEquivalenceClasses(TypeVariableType *typeVar1,
                                 TypeVariableType *typeVar2) {
      assert(typeVar1 == getRepresentative(typeVar1) &&
             "typeVar1 is not the representative");
      assert(typeVar2 == getRepresentative(typeVar2) &&
             "typeVar2 is not the representative");
      assert(typeVar1 != typeVar2 && "cannot merge type with itself");
      typeVar1->getImpl().mergeEquivalenceClasses(typeVar2, SavedBindings);
    }

    /// \brief Retrieve the fixed type corresponding to the given type variable,
    /// or a null type if there is no fixed type.
    Type getFixedType(TypeVariableType *typeVar) {
      return typeVar->getImpl().getFixedType(&SavedBindings);
    }

    /// \brief Assign a fixed type to the given type variable.
    void assignFixedType(TypeVariableType *typeVar, Type type) {
      typeVar->getImpl().assignFixedType(type, SavedBindings);
    }

    /// \brief Determine whether we have already explored type bindings for
    /// the given type variable via child systems.
    bool haveExploredTypeVar(TypeVariableType *typeVar) {
      return ExploredTypeBindings.count(typeVar) > 0;
    }

    /// \brief Add a new potential binding of a type variable to a particular
    /// type, to be explored by a child system.
    void addPotentialBinding(TypeVariableType *typeVar, Type binding) {
      PotentialBindings.push_back({typeVar, binding});
    }

    /// \brief Indicate a design to explore a specific binding for the given
    /// type variable.
    ///
    /// \returns true if this type variable binding should be explored,
    /// or false if it has already been explored.
    bool exploreBinding(TypeVariableType *typeVar, Type binding) {
      return ExploredTypeBindings[typeVar].insert(binding->getCanonicalType());
    }

    /// \brief "Open" the given type by replacing any occurrences of archetypes
    /// (including those implicit in unbound generic types) with fresh type
    /// variables.
    ///
    /// \param type The type to open.
    /// \returns The opened type, or \c type if there are no archetypes in it.
    Type openType(Type type) {
      llvm::DenseMap<ArchetypeType *, TypeVariableType *> replacements;
      return openType(type, { }, replacements);
    }

    /// \brief "Open" the given type by replacing any occurrences of archetypes
    /// (including those implicit in unbound generic types) with fresh type
    /// variables.
    ///
    /// \param type The type to open.
    ///
    /// \param archetypes The set of archetypes we're opening.
    ///
    /// \param replacements The mapping from opened archetypes to the type
    /// variables to which they were opened.
    ///
    /// \returns The opened type, or \c type if there are no archetypes in it.
    Type openType(Type type, ArrayRef<ArchetypeType *> archetypes,
           llvm::DenseMap<ArchetypeType *, TypeVariableType *> &replacements);

    /// \brief Retrieve the type of a reference to the given value declaration.
    ///
    /// For references to polymorphic function types, this routine "opens up"
    /// the type by replacing each instance of an archetype with a fresh type
    /// variable.
    Type getTypeOfReference(ValueDecl *decl);

    /// \brief Retrieve the type of a reference to the given value declaration,
    /// as a member with a base of the given type.
    ///
    /// For references to polymorphic function types, this routine "opens up"
    /// the type by replacing each instance of an archetype with a fresh type
    /// variable.
    ///
    /// \param isTypeReference Indicates that we want to refer to the declared
    /// type of the type declaration rather than referring to it as a value.
    Type getTypeOfMemberReference(Type baseTy, ValueDecl *decl,
                                  bool isTypeReference);

    /// \brief Add a new overload set to the list of unresolved overload
    /// sets.
    void addOverloadSet(OverloadSet *ovl) {
      // If we have an introducing expression, record it.
      if (auto introducer = ovl->getIntroducingExpr()) {
        GeneratedOverloadSets[introducer] = ovl;
      } else if (auto constraint = ovl->getIntroducingConstraint()) {
        if (auto anchor = constraint->getAnchorExpr()) {
          GeneratedOverloadSets[anchor] = ovl;
        }
      }

      // If there are fewer than two choices, then we can simply resolve this
      // now.
      if (ovl->getChoices().size() < 2) {
        resolveOverload(ovl, 0);
        return;
      }

      UnresolvedOverloadSets.push_back(ovl);
    }

    /// \brief Find the overload set generated by the current expression, if
    /// any.
    OverloadSet *getGeneratedOverloadSet(Expr *expr) {
      for (auto cs = this; cs; cs = cs->Parent) {
        auto known = cs->GeneratedOverloadSets.find(expr);
        if (known != cs->GeneratedOverloadSets.end())
          return known->second;
      }

      return nullptr;
    }

    /// \brief Find the overload choice that was assumed by this constraint
    /// system (or one of its parents), along with the type it was given.
    Optional<std::pair<OverloadChoice, Type>>
    getSelectedOverloadFromSet(OverloadSet *ovl) {
      for (auto cs = this; cs; cs = cs->Parent) {
        auto known = cs->ResolvedOverloads.find(ovl);
        if (known != cs->ResolvedOverloads.end()) {
          return std::make_pair(ovl->getChoices()[known->second.first],
                                known->second.second);
        }
      }

      return Nothing;
    }

    /// \brief Retrieve the allocator used by this constraint system.
    llvm::BumpPtrAllocator &getAllocator() { return SharedState->Allocator; }

    template <typename It>
    ArrayRef<typename std::iterator_traits<It>::value_type>
    allocateCopy(It start, It end) {
      typedef typename std::iterator_traits<It>::value_type T;
      T *result = (T*)getAllocator().Allocate(sizeof(T)*(end-start),
                                              __alignof__(T));
      unsigned i;
      for (i = 0; start != end; ++start, ++i)
        new (result+i) T(*start);
      return ArrayRef<T>(result, i);
    }

    template<typename T>
    ArrayRef<T> allocateCopy(ArrayRef<T> array) {
      return allocateCopy(array.begin(), array.end());
    }

    /// \brief Generate constraints for the given (unchecked) expression.
    ///
    /// \returns true if an error occurred.
    bool generateConstraints(Expr *E);

    /// \brief The result of attempting to resolve a constraint or set of
    /// constraints.
    enum class SolutionKind : char {
      /// \brief The constraint has been trivially solved, by not introducing
      /// any additional constraints.
      TriviallySolved,
      /// \brief The constraint has been solved completely, and provides no
      /// more information.
      Solved,
      /// \brief The constraint could not be solved at this point.
      Unsolved,
      /// \brief The constraint uncovers an inconsistency in the system.
      Error
    };

  private:
    /// \brief Enumerates all of the 'direct' supertypes of the given type.
    ///
    /// The direct supertype S of a type T is a supertype of T (e.g., T < S)
    /// such that there is no type U where T < U and U < S.
    SmallVector<Type, 4> enumerateDirectSupertypes(Type type);

    /// \brief Flags that direct type matching.
    enum TypeMatchFlags {
      TMF_None = 0,

      /// \brief Indicates that we are in a context where we should be
      /// generating constraints for any unsolvable problems.
      ///
      /// This flag is automatically introduced when type matching destructures
      /// a type constructor (tuple, function type, etc.), solving that
      /// constraint while potentially generating others.
      TMF_GenerateConstraints = 0x01
    };

    /// \brief Subroutine of \c matchTypes(), which matches up two tuple types.
    SolutionKind matchTupleTypes(TupleType *tuple1, TupleType *tuple2,
                                 TypeMatchKind kind, unsigned flags,
                                 bool &trivial);

    /// \brief Subroutine of \c matchTypes(), which matches up two function
    /// types.
    SolutionKind matchFunctionTypes(FunctionType *func1, FunctionType *func2,
                                    TypeMatchKind kind, unsigned flags,
                                    bool &trivial);

    /// \brief Attempt to match up types \c type1 and \c type2, which in effect
    /// is solving the given type constraint between these two types.
    ///
    /// \param type1 The first type, which is on the left of the type relation.
    ///
    /// \param type2 The second type, which is on the right of the type relation.
    ///
    /// \param kind The kind of type match being performed, e.g., exact match,
    /// trivial subtyping, subtyping, or conversion.
    ///
    /// \param flags A set of flags composed from the TMF_* constants, which
    /// indicates how
    ///
    /// \param trivial Will be set false if any non-trivial subtyping or
    /// conversion is applied.
    ///
    /// \returns the result of attempting to solve this constraint.
    SolutionKind matchTypes(Type type1, Type type2, TypeMatchKind kind,
                            unsigned flags, bool &trivial);

  public:
    /// \brief Resolve the given overload set to the choice with the given
    /// index within this constraint system.
    void resolveOverload(OverloadSet *ovl, unsigned idx);

    /// \brief Simplify a type, by replacing type variables with either their
    /// fixed types (if available) or their representatives.
    ///
    /// The resulting types can be compared canonically, so long as additional
    /// type equivalence requirements aren't introduced between comparisons.
    Type simplifyType(Type type){
      llvm::SmallPtrSet<TypeVariableType *, 16> substituting;
      return simplifyType(type, substituting);
    }

  private:
    /// \brief Simplify a type, by replacing type variables with either their
    /// fixed types (if available) or their representatives.
    ///
    /// \param type the type to be simplified.
    ///
    /// \param substituting the set of type variables that we're already
    /// substituting for. These type variables will not be substituted again,
    /// to avoid infinite recursion.
    ///
    /// The resulting types can be compared canonically, so long as additional
    /// type equivalence requirements aren't introduced between comparisons.
    Type simplifyType(Type type,
                      llvm::SmallPtrSet<TypeVariableType *, 16> &substituting);


    /// \brief Attempt to simplify the given literal constraint.
    SolutionKind simplifyLiteralConstraint(Type type, LiteralKind kind);

    /// \brief Attempt to simplify the given member constraint.
    SolutionKind simplifyMemberConstraint(const Constraint &constraint);

    /// \brief Attempt to simplify the given archetype constraint.
    SolutionKind simplifyArchetypeConstraint(const Constraint &constraint);

    /// \brief Simplify the given constaint.
    SolutionKind simplifyConstraint(const Constraint &constraint);

  public:
    /// \brief Walks through the list of constraints, collecting the constraints
    /// that directly apply to each representative type variable.
    ///
    /// \param typeVarConstraints will be populated with a list of
    /// representative type variables and the constraints that apply directly
    /// to them.
    void collectConstraintsForTypeVariables(
           SmallVectorImpl<TypeVariableConstraints> &typeVarConstraints);

  public:
    /// \brief Simplify the system of constraints, by breaking down complex
    /// constraints into simpler constraints.
    ///
    /// The result of simplification is a constraint system that consisting of
    /// only simple constraints relating type variables to each other or
    /// directly to fixed types. There are no constraints that involve
    /// type constructors on both sides. the simplified constraint system may,
    /// of course, include type variables for which we have constraints but
    /// no fixed type. Such type variables are left to the solver to bind.
    ///
    /// \returns true if an error occurred, false otherwise.
    bool simplify();

    /// \brief Solve the system of constraints.
    ///
    /// \param viable The set of constraint systems that are still viable.
    ///
    /// \returns true if an error occurred, false otherwise.
    bool solve(SmallVectorImpl<ConstraintSystem *> &viable);

    /// \brief Determine whether this constraint system is fully solved, with
    /// no free variables.
    bool isSolved() const { return State == Solved; }

  private:
    /// \brief Determine whether the given \p type matches the default literal
    /// type for a literal constraint placed on the type variable \p tv.
    bool typeMatchesDefaultLiteralConstraint(TypeVariableType *tv,
                                             Type type);

    // \brief Compare two solutions to the same set of constraints.
    static SolutionCompareResult compareSolutions(ConstraintSystem &cs1,
                                                  ConstraintSystem &cs2);

  public:
    /// \brief Given a set of viable constraint systems, find the best
    /// solution.
    ///
    /// \returns the best solution, or null if there is no best solution.
    static ConstraintSystem *
    findBestSolution(SmallVectorImpl<ConstraintSystem *> &viable);

    /// \brief Convert the expression to the given type.
    Expr *convertToType(Expr *expr, Type toType, bool isAssignment = false);

    /// \brief Convert the object argumet to the given type.
    Expr *convertObjectArgumentToType(Expr *expr, Type toType);

    /// \brief Apply the solution to the given expression, returning the
    /// rewritten, fully-typed expression.
    Expr *applySolution(Expr *expr);

    void dump();
  };

  /// \brief RAII object that re-instates the type variable bindings for
  /// the given constraint system.
  class ReinstateTypeVariableBindingsRAII {
    ConstraintSystem &CS;

  public:
    ReinstateTypeVariableBindingsRAII(ConstraintSystem &cs) : CS(cs) {
      cs.injectPermanentTypeVariableBindings();
    }

    ~ReinstateTypeVariableBindingsRAII() {
      CS.restoreTypeVariableBindings();
    }

    ReinstateTypeVariableBindingsRAII(const ReinstateTypeVariableBindingsRAII&)
      = delete;
    ReinstateTypeVariableBindingsRAII(ReinstateTypeVariableBindingsRAII&&)
      = delete;
    ReinstateTypeVariableBindingsRAII &
    operator=(const ReinstateTypeVariableBindingsRAII&) = delete;
    ReinstateTypeVariableBindingsRAII &
    operator=(ReinstateTypeVariableBindingsRAII&&) = delete;
  };
}

void *operator new(size_t bytes, ConstraintSystem& cs,
                   size_t alignment) {
  return cs.getAllocator().Allocate(bytes, alignment);
}

OverloadSet *OverloadSet::getNew(ConstraintSystem &CS,
                                 Type boundType,
                                 Expr *expr,
                                 ArrayRef<OverloadChoice> choices) {
  unsigned size = sizeof(OverloadSet)
                + sizeof(OverloadChoice) * choices.size();
  void *mem = CS.getAllocator().Allocate(size, alignof(OverloadSet));
  return ::new (mem) OverloadSet(CS.assignOverloadSetID(), expr, boundType,
                                 choices);
}

OverloadSet *OverloadSet::getNew(ConstraintSystem &CS,
                                 Type boundType,
                                 const Constraint *constraint,
                                 ArrayRef<OverloadChoice> choices) {
  unsigned size = sizeof(OverloadSet)
                + sizeof(OverloadChoice) * choices.size();
  void *mem = CS.getAllocator().Allocate(size, alignof(OverloadSet));
  return ::new (mem) OverloadSet(CS.assignOverloadSetID(), constraint,
                                 boundType, choices);
}

void ConstraintSystem::markChildInactive(ConstraintSystem *childCS) {
  assert(NumActiveChildren > 0);
  --NumActiveChildren;

  // If the child system made an assumption about a type variable that
  // didn't pan out, try weaker assumptions.
  if (auto typeVar = childCS->assumedTypeVar) {
    auto boundTy = childCS->getFixedType(typeVar);
    auto supertypes = enumerateDirectSupertypes(boundTy);
    auto &explored = ExploredTypeBindings[typeVar];

    bool addedAny = false;
    for (auto supertype : supertypes) {
      if (explored.count(supertype->getCanonicalType()) > 0)
        continue;

      ++NumSupertypeFallbacks;
      PotentialBindings.push_back( { typeVar, supertype } );
      addedAny = true;
    }

    if (!addedAny) {
      // If we haven't added any constraints for this type variable, check
      // whether we can fall back to a default literal type.
      // FIXME: This would be far, far more efficient if we keep constraints
      // related to a type variable on-line.
      for (auto constraint : Constraints) {
        if (constraint->getClassification() !=ConstraintClassification::Literal)
          continue;

        if (auto constrainedVar
              = dyn_cast<TypeVariableType>(
                  constraint->getFirstType().getPointer())) {
          if (getRepresentative(constrainedVar) != typeVar)
            continue;

          if (auto literalType =
                TC.getDefaultLiteralType(constraint->getLiteralKind())) {
            if (explored.count(literalType->getCanonicalType()) == 0)
              PotentialBindings.push_back({typeVar, literalType});
          }
        }
      }
    }
  }
}

Type ConstraintSystem::openType(
       Type startingType,
       ArrayRef<ArchetypeType *> archetypes,
       llvm::DenseMap<ArchetypeType *, TypeVariableType *> &replacements) {
  struct GetTypeVariable {
    ConstraintSystem &CS;
    llvm::DenseMap<ArchetypeType *, TypeVariableType *> &Replacements;

    TypeVariableType *operator()(ArchetypeType *archetype) const {
      // Check whether we already have a replacement for this archetype.
      auto known = Replacements.find(archetype);
      if (known != Replacements.end())
        return known->second;

      // Create a new type variable to replace this archetype.
      auto tv = CS.createTypeVariable(archetype);

      // If there is a superclass for the archetype, add the appropriate
      // trivial subtype requirement on the type variable.
      if (auto superclass = archetype->getSuperclass()) {
        CS.addConstraint(ConstraintKind::TrivialSubtype, tv, superclass);
      }

      // The type variable must be convertible of the composition of all of
      // its protocol conformance requirements, i.e., it must conform to
      // each of those protocols.
      auto conformsTo = archetype->getConformsTo();
      if (!conformsTo.empty()) {
        // FIXME: Can we do this more efficiently, since we know that the
        // protocol list has already been minimized?
        SmallVector<Type, 4> conformsToTypes;
        conformsToTypes.reserve(conformsTo.size());
        std::transform(conformsTo.begin(), conformsTo.end(),
                       std::back_inserter(conformsToTypes),
                       [](ProtocolDecl *proto) {
                         return proto->getDeclaredType();
                       });

        auto composition = ProtocolCompositionType::get(CS.TC.Context,
                                                        conformsToTypes);
        CS.addConstraint(ConstraintKind::Conversion, tv, composition);
      }

      // Record the type variable that corresponds to this archetype.
      Replacements[archetype] = tv;

      // Build archetypes for each of the nested types.
      for (auto nested : archetype->getNestedTypes()) {
        auto nestedTv = (*this)(nested.second);
        CS.addTypeMemberConstraint(tv, nested.first, nestedTv);
      }

      return tv;
    }
  } getTypeVariable{*this, replacements};

  // Create type variables for each archetype we're opening.
  for (auto archetype : archetypes)
    (void)getTypeVariable(archetype);

  std::function<Type(Type)> replaceArchetypes;
  replaceArchetypes = [&](Type type) -> Type {
    // Replace archetypes with fresh type variables.
    if (auto archetype = type->getAs<ArchetypeType>()) {
      auto known = replacements.find(archetype);
      if (known != replacements.end())
        return known->second;

      return archetype;
    }

    // Create type variables for all of the archetypes in a polymorphic
    // function type.
    if (auto polyFn = type->getAs<PolymorphicFunctionType>()) {
      for (auto archetype : polyFn->getGenericParams().getAllArchetypes())
        (void)getTypeVariable(archetype);

      // Transform the input and output types.
      Type inputTy = TC.transformType(polyFn->getInput(), replaceArchetypes);
      if (!inputTy)
        return Type();

      Type resultTy = TC.transformType(polyFn->getResult(), replaceArchetypes);
      if (!resultTy)
        return Type();

      // Build the resulting (non-polymorphic) function type.
      return FunctionType::get(inputTy, resultTy, TC.Context);
    }

    // Open up unbound generic types, turning them into bound generic
    // types with type variables for each parameter.
    if (auto unbound = type->getAs<UnboundGenericType>()) {
      auto parentTy = unbound->getParent();
      if (parentTy)
        parentTy = TC.transformType(parentTy, replaceArchetypes);

      auto unboundDecl = unbound->getDecl();
      SmallVector<Type, 4> arguments;
      for (auto archetype : unboundDecl->getGenericParams()->getAllArchetypes())
        arguments.push_back(getTypeVariable(archetype));

      return BoundGenericType::get(unboundDecl, parentTy, arguments);
    }

    return type;
  };
  
  return TC.transformType(startingType, replaceArchetypes);
}

/// \brief Adjust lvalue types within the type of a reference to a declaration.
///
/// For an lvalue type, this routine adds the 'implicit' and 'nonheap' bits to
/// the lvalue.
///
/// For the function type of an assignment operator, makes the first argument
/// an implicit byref(settable).
static Type adjustLValueForReference(Type type, bool isAssignment,
                                     ASTContext &context) {
  LValueType::Qual quals = LValueType::Qual::NonHeap|LValueType::Qual::Implicit;
  if (auto lv = type->getAs<LValueType>()) {
    // FIXME: The introduction of 'non-heap' here is an artifact of the type
    // checker's inability to model the address-of operator that carries the
    // heap bit from its input to its output while removing the 'implicit' bit.
    // When we actually apply the inferred types in a constraint system to a
    // concrete expression, the 'implicit' bits will be dropped and the
    // appropriate 'heap' bits will be re-introduced.
    return LValueType::get(lv->getObjectType(),
                           quals | lv->getQualifiers(),
                           context);
  }

  // For an assignment operator, the first parameter is an implicit byref.
  if (isAssignment) {
    if (auto funcTy = type->getAs<FunctionType>()) {
      Type inputTy;
      if (auto inputTupleTy = funcTy->getInput()->getAs<TupleType>()) {
        if (inputTupleTy->getFields().size() > 0) {
          auto &firstParam = inputTupleTy->getFields()[0];
          auto firstParamTy
            = adjustLValueForReference(firstParam.getType(), false, context);
          SmallVector<TupleTypeElt, 2> elements;
          elements.push_back(firstParam.getWithType(firstParamTy));
          elements.append(inputTupleTy->getFields().begin() + 1,
                          inputTupleTy->getFields().end());
          inputTy = TupleType::get(elements, context);
        } else {
          inputTy = funcTy->getInput();
        }
      } else {
        inputTy = adjustLValueForReference(funcTy->getInput(), false, context);
      }

      return FunctionType::get(inputTy, funcTy->getResult(),
                               funcTy->isAutoClosure(),
                               funcTy->isBlock(),
                               context);
    }
  }

  return type;
}

// A property or subscript is settable if:
// - its base type (the type of the 'a' in 'a[n]' or 'a.b') either has
//   reference semantics or has value semantics and is settable, AND
// - the 'var' or 'subscript' decl for the property provides a setter
template<typename SomeValueDecl>
static LValueType::Qual settableQualForDecl(Type baseType,
                                            SomeValueDecl *decl) {
  bool settable = ((!baseType ||
                    baseType->isSettableLValue() ||
                    baseType->getRValueType()->hasReferenceSemantics()) &&
                   decl->isSettable());
  return settable ? LValueType::Qual(0) : LValueType::Qual::NonSettable;
}

// TODO This should replace ValueDecl::getTypeOfReference once the old
// type checker is retired.
static Type getTypeOfValueDeclReference(Type baseType,
                                        ValueDecl *decl,
                                        ASTContext &Context) {
  LValueType::Qual qual = LValueType::Qual::DefaultForVar |
                          settableQualForDecl(baseType, decl);
  
  if (decl->isReferencedAsLValue()) {
    if (LValueType *LVT = decl->getType()->getAs<LValueType>())
      return LValueType::get(LVT->getObjectType(), qual, Context);
    return LValueType::get(decl->getType(), qual, Context);
  }
  
  return decl->getType();
}

Type ConstraintSystem::getTypeOfReference(ValueDecl *value) {
  if (auto proto = dyn_cast<ProtocolDecl>(value->getDeclContext())) {
    // Unqualified lookup can find operator names within protocols.
    auto func = cast<FuncDecl>(value);
    assert(func->isOperator() && "Lookup should only find operators");

    // Skip the 'this' metatype parameter. It's not used for deduction.
    auto type = func->getTypeOfReference()->castTo<FunctionType>()->getResult();

    // Find the archetype for 'This'. We'll be opening it.
    auto thisArchetype
      = proto->getThis()->getDeclaredType()->castTo<ArchetypeType>();
    llvm::DenseMap<ArchetypeType *, TypeVariableType *> replacements;
    type = adjustLValueForReference(openType(type, { &thisArchetype, 1 },
                                             replacements),
                                    func->getAttrs().isAssignment(),
                                    TC.Context);

    // The type variable to which 'This' was opened must be bound to an
    // archetype.
    // FIXME: We may eventually want to loosen this constraint, to allow us
    // to find operator functions both in classes and in protocols to which
    // a class conforms (if there's a default implementation).
    addArchetypeConstraint(replacements[thisArchetype]);
    
    return type;
  }

  // Determine the type of the value, opening up that type if necessary.
  Type valueType = getTypeOfValueDeclReference(nullptr,
                                               value,
                                               TC.Context);
  valueType = adjustLValueForReference(openType(valueType),
                                  value->getAttrs().isAssignment(),
                                  TC.Context);
  return valueType;
}

Type ConstraintSystem::getTypeOfMemberReference(Type baseTy, ValueDecl *value,
                                                bool isTypeReference) {
  // If the base is a module type, just use the type of the decl.
  if (baseTy->is<ModuleType>())
    return getTypeOfReference(value);

  // Figure out the instance type used for the base.
  Type baseObjTy = baseTy->getRValueType();
  bool isInstance = true;
  if (auto baseMeta = baseObjTy->getAs<MetaTypeType>()) {
    baseObjTy = baseMeta->getInstanceType();
    isInstance = false;
  }

  // Figure out the type of the owner.
  auto owner = value->getDeclContext();
  GenericParamList *ownerGenericParams = nullptr;
  Type ownerTy;
  if (auto nominalOwner = dyn_cast<NominalTypeDecl>(owner)) {
    ownerTy = nominalOwner->getDeclaredTypeInContext();
    ownerGenericParams = nominalOwner->getGenericParamsOfContext();
  } else {
    auto extensionOwner = cast<ExtensionDecl>(owner);
    auto extendedTy = extensionOwner->getExtendedType();
    if (auto nominal = extendedTy->getAs<NominalType>()) {
      ownerTy = nominal->getDecl()->getDeclaredTypeInContext();
      ownerGenericParams = nominal->getDecl()->getGenericParamsOfContext();
    } else if (auto unbound = extendedTy->getAs<UnboundGenericType>()) {
      ownerTy = unbound->getDecl()->getDeclaredTypeInContext();
      ownerGenericParams = unbound->getDecl()->getGenericParamsOfContext();
    } else
      llvm_unreachable("unknown owner for type member");
  }

  // The archetypes that have been opened up and replaced with type variables.
  llvm::DenseMap<ArchetypeType *, TypeVariableType *> replacements;

  // If the owner is specialized, we need to open up the types in the owner.
  if (ownerTy->isSpecialized()) {
    ArrayRef<ArchetypeType *> openArchetypes;
    if (ownerGenericParams)
      openArchetypes = ownerGenericParams->getAllArchetypes();
    ownerTy = openType(ownerTy, openArchetypes, replacements);
  }

  // The base type must be convertible to the owner type. For most cases,
  // subtyping suffices. However, the owner might be a protocol and the base a
  // type that implements that protocol, if which case we need to model this
  // with a conversion constraint.
  addConstraint(ConstraintKind::Conversion, baseObjTy, ownerTy);

  // Determine the type of the member.
  Type type;
  if (isTypeReference)
    type = cast<TypeDecl>(value)->getDeclaredType();
  else if (auto subscript = dyn_cast<SubscriptDecl>(value)) {
    auto resultTy = LValueType::get(subscript->getElementType(),
                                    LValueType::Qual::DefaultForMemberAccess|
                                    LValueType::Qual::Implicit|
                                    settableQualForDecl(baseTy, subscript),
                                    TC.Context);
    type = FunctionType::get(subscript->getIndices()->getType(), resultTy,
                             TC.Context);
  } else
    type = getTypeOfValueDeclReference(baseTy, value, TC.Context);

  // For a member of an archetype, substitute the base type for the 'This'
  // type.
  if (baseObjTy->is<ArchetypeType>()) {
    if (auto ownerProtoTy = ownerTy->getAs<ProtocolType>()) {
      auto thisArchetype = ownerProtoTy->getDecl()->getThis()->getDeclaredType()
                             ->castTo<ArchetypeType>();
      type = TC.transformType(type,
               [&](Type type) -> Type {
                 if (auto archetype = type->getAs<ArchetypeType>()) {
                   if (archetype == thisArchetype)
                     return baseObjTy;
                 }

                 return type;
             });
    }
  }

  type = openType(type, { }, replacements);

  // Skip the 'this' argument if it's already been bound by the base.
  if (auto func = dyn_cast<FuncDecl>(value)) {
    if (func->isStatic() || isInstance)
      type = type->castTo<AnyFunctionType>()->getResult();
  } else if (isa<ConstructorDecl>(value) || isa<OneOfElementDecl>(value)) {
    type = type->castTo<AnyFunctionType>()->getResult();
  }
  return adjustLValueForReference(type, value->getAttrs().isAssignment(),
                                  TC.Context);
}

/// \brief Retrieve the name bound by the given (immediate) pattern.
static Identifier findPatternName(Pattern *pattern) {
  switch (pattern->getKind()) {
  case PatternKind::Paren:
  case PatternKind::Any:
  case PatternKind::Tuple:
    return Identifier();

  case PatternKind::Named:
    return cast<NamedPattern>(pattern)->getBoundName();

  case PatternKind::Typed:
    return findPatternName(cast<TypedPattern>(pattern)->getSubPattern());
  }

  llvm_unreachable("Unhandled pattern kind");  
}

//===--------------------------------------------------------------------===//
// Constraint generation
//===--------------------------------------------------------------------===//
#pragma mark Constraint generation

/// \brief Determine whether the given type is constructible, meaning that
/// a construction constraint on the type can enumerate constructors.
static bool isConstructibleType(Type type) {
  if (type->is<StructType>() || type->is<OneOfType>())
    return true;

  if (auto BGT = type->getAs<BoundGenericType>())
    if (isa<StructDecl>(BGT->getDecl()) || isa<OneOfDecl>(BGT->getDecl()))
      return true;

  return false;
}

/// \brief Skip any implicit conversions applied to this expression.
static Expr *skipImplicitConversions(Expr *expr) {
  while (auto ice = dyn_cast<ImplicitConversionExpr>(expr))
    expr = ice->getSubExpr();
  return expr;
}

/// \brief Find the declaration directly referenced by this expression.
static ValueDecl *findReferencedDecl(Expr *expr, SourceLoc &loc) {
  do {
    expr = expr->getSemanticsProvidingExpr();

    if (auto ice = dyn_cast<ImplicitConversionExpr>(expr)) {
      expr = ice->getSubExpr();
      continue;
    }

    if (auto dre = dyn_cast<DeclRefExpr>(expr)) {
      loc = dre->getLoc();
      return dre->getDecl();
    }

    return nullptr;
  } while (true);
}


bool ConstraintSystem::generateConstraints(Expr *expr) {
  class ConstraintGenerator : public ExprVisitor<ConstraintGenerator, Type> {
    ConstraintSystem &CS;

  public:
    ConstraintGenerator(ConstraintSystem &CS) : CS(CS) { }

    ConstraintSystem &getConstraintSystem() const { return CS; }
    
    Type visitErrorExpr(ErrorExpr *E) {
      // FIXME: Can we do anything with error expressions at this point?
      return nullptr;
    }

    Type visitIntegerLiteralExpr(IntegerLiteralExpr *expr) {
      auto tv = CS.createTypeVariable(expr);
      CS.addLiteralConstraint(tv, LiteralKind::Int, expr);
      return tv;
    }

    Type visitFloatLiteralExpr(FloatLiteralExpr *expr) {
      auto tv = CS.createTypeVariable(expr);
      CS.addLiteralConstraint(tv, LiteralKind::Float, expr);
      return tv;
    }

    Type visitCharacterLiteralExpr(CharacterLiteralExpr *expr) {
      auto tv = CS.createTypeVariable(expr);
      CS.addLiteralConstraint(tv, LiteralKind::Char, expr);
      return tv;
    }

    Type visitStringLiteralExpr(StringLiteralExpr *expr) {
      auto tv = CS.createTypeVariable(expr);
      LiteralKind kind = LiteralKind::ASCIIString;
      if (std::find_if(expr->getValue().begin(), expr->getValue().end(),
                       [](char c) { return c > 127; })
            != expr->getValue().end())
        kind = LiteralKind::UTFString;
      CS.addLiteralConstraint(tv, kind, expr);
      return tv;
    }

    Type
    visitInterpolatedStringLiteralExpr(InterpolatedStringLiteralExpr *expr) {
      auto tv = CS.createTypeVariable(expr);
      CS.addLiteralConstraint(tv, LiteralKind::UTFString, expr);
      for (auto segment : expr->getSegments()) {
        CS.addConstraint(ConstraintKind::Construction, segment->getType(), tv,
                         expr);
      }
      return tv;
    }

    Type visitDeclRefExpr(DeclRefExpr *E) {
      // FIXME: If the decl is in error, we get no information from this.
      // We may, alternatively, want to use a type variable in that case,
      // and possibly infer the type of the variable that way.
      return adjustLValueForReference(CS.getTypeOfReference(E->getDecl()),
                                      E->getDecl()->getAttrs().isAssignment(),
                                      CS.getASTContext());
    }

    Type visitDotSyntaxBaseIgnoredExpr(DotSyntaxBaseIgnoredExpr *E) {
      llvm_unreachable("Already type-checked");
    }

    Type visitOverloadedDeclRefExpr(OverloadedDeclRefExpr *expr) {
      // For a reference to an overloaded declaration, we create a type variable
      // that will be equal to different types depending on which overload
      // is selected.
      auto tv = CS.createTypeVariable(expr);
      ArrayRef<ValueDecl*> decls = expr->getDecls();
      SmallVector<OverloadChoice, 4> choices;
      for (unsigned i = 0, n = decls.size(); i != n; ++i) {
        choices.push_back(OverloadChoice(Type(), decls[i]));
      }

      // Record this overload set.
      CS.addOverloadSet(OverloadSet::getNew(CS, tv, expr, choices));
      return tv;
    }

    Type visitOverloadedMemberRefExpr(OverloadedMemberRefExpr *expr) {
      // For a reference to an overloaded declaration, we create a type variable
      // that will be bound to different types depending on which overload
      // is selected.
      auto tv = CS.createTypeVariable(expr);
      ArrayRef<ValueDecl*> decls = expr->getDecls();
      SmallVector<OverloadChoice, 4> choices;
      auto baseTy = expr->getBase()->getType();
      for (unsigned i = 0, n = decls.size(); i != n; ++i) {
        choices.push_back(OverloadChoice(baseTy, decls[i]));
      }
      
      // Record this overload set.
      CS.addOverloadSet(OverloadSet::getNew(CS, tv, expr, choices));
      return tv;
    }
    
    Type visitOverloadedSuperMemberRefExpr(OverloadedSuperMemberRefExpr *e) {
      llvm_unreachable("not implemented");
    }

    Type visitOverloadedSuperConstructorRefExpr(
                                         OverloadedSuperConstructorRefExpr *e) {
      llvm_unreachable("not implemented");
    }
    
    Type visitUnresolvedDeclRefExpr(UnresolvedDeclRefExpr *expr) {
      // This is an error case, where we're trying to use type inference
      // to help us determine which declaration the user meant to refer to.
      // FIXME: Do we need to note that we're doing some kind of recovery?
      return CS.createTypeVariable(expr);
    }

    Type visitMemberRefExpr(MemberRefExpr *expr) {
      auto tv = CS.createTypeVariable(expr);
      OverloadChoice choice(expr->getBase()->getType(), expr->getDecl());
      CS.addOverloadSet(OverloadSet::getNew(CS, tv, expr, { &choice, 1 }));
      return tv;
    }
    
    Type visitSuperMemberRefExpr(SuperMemberRefExpr *expr) {
      llvm_unreachable("not implemented");
    }

    Type visitExistentialMemberRefExpr(ExistentialMemberRefExpr *expr) {
      llvm_unreachable("Already type-checked");
    }

    Type visitArchetypeMemberRefExpr(ArchetypeMemberRefExpr *expr) {
      llvm_unreachable("Already type-checked");
    }

    Type visitGenericMemberRefExpr(GenericMemberRefExpr *expr) {
      auto tv = CS.createTypeVariable(expr);
      OverloadChoice choice(expr->getBase()->getType(), expr->getDecl());
      CS.addOverloadSet(OverloadSet::getNew(CS, tv, expr, { &choice, 1 }));
      return tv;
    }

    Type visitUnresolvedMemberExpr(UnresolvedMemberExpr *expr) {
      auto oneofTy = CS.createTypeVariable(expr);
      auto memberTy = CS.createTypeVariable(expr);

      // An unresolved member expression '.member' is modeled as a value member
      // constraint
      //
      //   T0[.member] == T1
      //
      // for fresh type variables T0 and T1. Depending on whether the member
      // will end up having unit type () or an actual type, T1 will either be
      // T0 or will be T2 -> T0 for some fresh type variable T2. Since T0
      // cannot be determined without picking one of these options, and we
      // cannot know whether to select the value form (T0) or the function
      // form (T2 -> T0) until T0 has been deduced, we cannot model this
      // directly within the constraint system. Instead, we introduce a new
      // overload set with two entries: one for T0 and one for T2 -> T0.
      CS.addValueMemberConstraint(oneofTy, expr->getName(), memberTy, expr);

      OverloadChoice choices[2] = {
        OverloadChoice(oneofTy, OverloadChoiceKind::BaseType),
        OverloadChoice(oneofTy, OverloadChoiceKind::FunctionReturningBaseType),
      };
      CS.addOverloadSet(OverloadSet::getNew(CS, memberTy, expr, choices));
      return memberTy;
    }

    Type visitUnresolvedDotExpr(UnresolvedDotExpr *expr) {
      // The base must have a member of the given name, such that accessing
      // that member through the base returns a value convertible to the type
      // of this expression.
      auto baseTy = expr->getBase()->getType();
      auto tv = CS.createTypeVariable(expr);
      CS.addValueMemberConstraint(baseTy, expr->getName(), tv, expr);
      return tv;
    }
    
    Type visitUnresolvedSuperMemberExpr(UnresolvedSuperMemberExpr *expr) {
      llvm_unreachable("not implemented");
    }

    Type visitSequenceExpr(SequenceExpr *expr) {
      llvm_unreachable("Didn't even parse?");
    }

    Type visitParenExpr(ParenExpr *expr) {
      return expr->getSubExpr()->getType();
    }

    Type visitTupleExpr(TupleExpr *expr) {
      // The type of a tuple expression is simply a tuple of the types of
      // its subexpressions.
      SmallVector<TupleTypeElt, 4> elements;
      elements.reserve(expr->getNumElements());
      for (unsigned i = 0, n = expr->getNumElements(); i != n; ++i) {
        elements.push_back(TupleTypeElt(expr->getElement(i)->getType(),
                                        expr->getElementName(i)));
      }

      return TupleType::get(elements, CS.getASTContext());
    }

    Type visitSubscriptExpr(SubscriptExpr *expr) {
      ASTContext &Context = CS.getASTContext();

      // The base type must have a subscript declaration with type 
      // I -> [byref] O, where I and O are fresh type variables. The index
      // expression must be convertible to I and the subscript expression
      // itself has type [byref] O, where O may or may not be settable.
      auto inputTv = CS.createTypeVariable(expr);
      auto outputTv = CS.createTypeVariable(expr);

      auto outputSuperTv = CS.createTypeVariable(expr);
      auto outputSuperTy = LValueType::get(outputSuperTv,
                                      LValueType::Qual::DefaultForMemberAccess|
                                      LValueType::Qual::Implicit|
                                      LValueType::Qual::NonSettable,
                                      Context);

      // Add the member constraint for a subscript declaration.
      // FIXME: lame name!
      auto baseTy = expr->getBase()->getType();
      auto fnTy = FunctionType::get(inputTv, outputTv, Context);
      CS.addValueMemberConstraint(baseTy, Context.getIdentifier("__subscript"),
                                  fnTy, expr);
      
      // Add the subtype constraint that the output type must be some lvalue
      // subtype.
      CS.addConstraint(ConstraintKind::Subtype,
                       outputTv,
                       outputSuperTy);
      
      // Add the constraint that the index expression's type be convertible
      // to the input type of the subscript operator.
      CS.addConstraint(ConstraintKind::Conversion,
                       expr->getIndex()->getType(),
                       inputTv, expr);
      return outputTv;
    }
    
    Type visitArrayExpr(ArrayExpr *expr) {
      ASTContext &C = CS.getASTContext();
      // An array expression can be of a type T that conforms to the
      // ArrayLiteralConvertible protocol.
      ProtocolDecl *arrayProto = CS.TC.getArrayLiteralProtocol();
      if (!arrayProto) {
        CS.TC.diagnose(expr->getStartLoc(), diag::array_expr_missing_proto);
        return Type();
      }

      auto arrayTy = CS.createTypeVariable(expr);

      // FIXME: Constraint checker appears to be unable to solve a system in
      // which the same type variable is a subtype of multiple types.
      // The protocol conformance checker also has an issue with checking
      // conformances of generic types <rdar://problem/13153805>
      //Type arrayProtoTy = arrayProto->getDeclaredType();
      //CS.addConstraint(ConstraintKind::Subtype,
      //                 arrayTy, arrayProtoTy);
      
      // Its subexpression should be convertible to a tuple (T.Element...).
      auto arrayElementTy = CS.createTypeVariable(expr);
      auto arrayElementMetaTy = MetaTypeType::get(arrayElementTy, C);
      CS.addTypeMemberConstraint(arrayTy,
                                 C.getIdentifier("Element"),
                                 arrayElementMetaTy);
      
      Type arrayEltsTy = CS.TC.getArraySliceType(expr->getLoc(),
                                                 arrayElementTy);
      TupleTypeElt arrayEltsElt{arrayEltsTy,
                                /*name=*/ Identifier(),
                                /*init=*/ nullptr,
                                arrayElementTy};
      Type arrayEltsTupleTy = TupleType::get(arrayEltsElt, C);
      CS.addConstraint(ConstraintKind::Conversion,
                       expr->getSubExpr()->getType(),
                       arrayEltsTupleTy);
      
      // FIXME: Use the protocol constraint instead of this value constraint.
      Type converterFuncTy = FunctionType::get(arrayEltsTupleTy,
                                               arrayTy, C);
      CS.addValueMemberConstraint(arrayTy,
                                  C.getIdentifier("convertFromArrayLiteral"),
                                  converterFuncTy,
                                  expr);
      
      return arrayTy;
    }

    Type visitSuperSubscriptExpr(SuperSubscriptExpr *expr) {
      llvm_unreachable("not implemented");
    }
    
    Type visitOverloadedSubscriptExpr(OverloadedSubscriptExpr *expr) {
      llvm_unreachable("Already type-checked");
    }
    
    Type visitOverloadedSuperSubscriptExpr(OverloadedSuperSubscriptExpr *expr) {
      llvm_unreachable("not implemented");
    }

    Type visitExistentialSubscriptExpr(ExistentialSubscriptExpr *expr) {
      llvm_unreachable("Already type-checked");
    }

    Type visitArchetypeSubscriptExpr(ArchetypeSubscriptExpr *expr) {
      llvm_unreachable("Already type-checked");
    }

    Type visitGenericSubscriptExpr(GenericSubscriptExpr *expr) {
      llvm_unreachable("Already type-checked");
    }

    Type visitTupleElementExpr(TupleElementExpr *expr) {
      llvm_unreachable("Already type-checked");
    }

    /// \brief Produces a type for the given pattern, filling in any missing
    /// type information with fresh type variables.
    ///
    /// \param pattern The pattern.
    /// \param expr The expression that 'anchors' the pattern, which is used
    /// to create fresh type variables.
    Type getTypeForPattern(Pattern *pattern, Expr *expr) {
      switch (pattern->getKind()) {
      case PatternKind::Paren:
        // Parentheses don't affect the type.
        return getTypeForPattern(cast<ParenPattern>(pattern)->getSubPattern(),
                                 expr);

      case PatternKind::Any:
        // For a pattern of unknown type, create a new type variable.
        return CS.createTypeVariable(expr);

      case PatternKind::Named: {
        // For a named pattern without a type, create a new type variable
        // and use it as the type of the variable.
        auto tv = CS.createTypeVariable(expr);
        cast<NamedPattern>(pattern)->getDecl()->setType(tv);
        return tv;
      }

      case PatternKind::Typed:
        // For a typed pattern, simply return the type of the pattern.
        // FIXME: Error recovery if the type is an error type?
        return cast<TypedPattern>(pattern)->getTypeLoc().getType();

      case PatternKind::Tuple: {
        auto tuplePat = cast<TuplePattern>(pattern);
        SmallVector<TupleTypeElt, 4> tupleTypeElts;
        tupleTypeElts.reserve(tuplePat->getNumFields());
        for (auto tupleElt : tuplePat->getFields()) {
          auto eltTy = getTypeForPattern(tupleElt.getPattern(), expr);
          auto name = findPatternName(tupleElt.getPattern());
          Type varArgBaseTy;
          if (tupleElt.isVararg()) {
            varArgBaseTy = eltTy;
            eltTy = CS.getTypeChecker().getArraySliceType(SourceLoc(), eltTy);
          }
          tupleTypeElts.push_back(TupleTypeElt(eltTy, name, tupleElt.getInit(),
                                               varArgBaseTy));
        }
        return TupleType::get(tupleTypeElts, CS.getASTContext());
      }
      }

      llvm_unreachable("Unhandled pattern kind");
    }

    Type visitFuncExpr(FuncExpr *expr) {
      // Func expressions always have function type. In cases where a parameter
      // or return type is omitted, a fresh type variable is used to stand in
      // for that parameter or return type, allowing it to be inferred from
      // context.
      auto funcTy = expr->getBodyResultTypeLoc().getType();
      if (!funcTy) {
        // If no return type was specified, create a fresh type
        // variable for it.
        funcTy = CS.createTypeVariable(expr);
      }

      // Walk through the patterns in the func expression, backwards,
      // computing the type of each pattern (which may involve fresh type
      // variables where parameter types where no provided) and building the
      // eventual function type.
      auto patterns = expr->getArgParamPatterns();
      for (unsigned i = 0, e = patterns.size(); i != e; ++i) {
        Type paramTy =  getTypeForPattern(patterns[e - i - 1], expr);
        funcTy = FunctionType::get(paramTy, funcTy, CS.getASTContext());
      }

      return funcTy;
    }

    Type visitExplicitClosureExpr(ExplicitClosureExpr *expr) {
      // Explicit closure expressions have function type (T1, T2, ... TN) -> T0,
      // for fresh type variables Ti, 0 <= i <= N, where N is the number of
      // the largest closure argument (e.g., N=5 in an explicit closure { $5 }).
      auto varDecls = expr->getParserVarDecls();
      unsigned n = varDecls.size();
      SmallVector<TupleTypeElt, 4> tupleTypeElts;
      tupleTypeElts.reserve(n);
      for (unsigned i = 0; i != n; ++i) {
        auto tv = CS.createTypeVariable(expr);
        tupleTypeElts.push_back(tv);
        varDecls[i]->setType(tv);
      }
      ASTContext &Context = CS.getASTContext();
      auto inputTy = TupleType::get(tupleTypeElts, Context);
      auto outputTy = CS.createTypeVariable(expr);
      return FunctionType::get(inputTy, outputTy, Context);
    }

    Type visitImplicitClosureExpr(ImplicitClosureExpr *expr) {
      llvm_unreachable("Already type-checked");
    }

    Type visitModuleExpr(ModuleExpr *expr) {
      // Module expressions always have a fixed type.
      return expr->getType();
    }

    Type visitAddressOfExpr(AddressOfExpr *expr) {
      // The address-of operator produces an explicit lvalue
      // [byref(settable)] T from a (potentially implicit) settable lvalue S.
      // We model this with the constraint
      //
      //     S < [byref(implicit, settable)] T
      //
      // where T is a fresh type variable.
      auto tv = CS.createTypeVariable(expr);
      auto bound = LValueType::get(tv,
                                   LValueType::Qual::DefaultForType|
                                   LValueType::Qual::Implicit,
                                   CS.getASTContext());
      auto result = LValueType::get(tv,
                                    LValueType::Qual::DefaultForType,
                                    CS.getASTContext());

      CS.addConstraint(ConstraintKind::Subtype,
                       expr->getSubExpr()->getType(), bound,
                       expr);
      return result;
    }

    Type visitNewArrayExpr(NewArrayExpr *expr) {
      // Open up the element type.
      auto resultTy = CS.openType(expr->getElementTypeLoc().getType());
      auto &tc = CS.getTypeChecker();
      for (unsigned i = expr->getBounds().size(); i != 1; --i) {
        auto &bound = expr->getBounds()[i-1];
        if (!bound.Value) {
          resultTy = tc.getArraySliceType(bound.Brackets.Start, resultTy);
          continue;
        }

        // FIXME: When we get a constant expression evaluator, we'll have
        // to use it here.
        auto literal = cast<IntegerLiteralExpr>(
                         bound.Value->getSemanticsProvidingExpr());
        resultTy = ArrayType::get(resultTy, literal->getValue().getZExtValue(),
                                  tc.Context);
      }

      auto &outerBound = expr->getBounds()[0];
      return tc.getArraySliceType(outerBound.Brackets.Start, resultTy);
    }

    Type visitNewReferenceExpr(NewReferenceExpr *expr) {
      // Create a new object of some reference type.
      auto instanceTy = CS.openType(expr->getElementTypeLoc().getType());

      // One must be able to construct an object from the provided argument,
      // or () if no argument was provided.
      Type argTy;
      if (auto arg = expr->getArg())
        argTy = arg->getType();
      else
        argTy = TupleType::getEmpty(CS.getASTContext());

      auto &context = CS.getASTContext();
      // FIXME: lame name
      auto name = context.getIdentifier("constructor");
      auto tv = CS.createTypeVariable();

      // The constructors will have type T -> instanceTy, for some fresh type
      // variable T.
      // FIXME: When we add support for class clusters, we'll need a new type
      // variable for the return type that is a subtype of instanceTy.
      CS.addValueMemberConstraint(instanceTy, name,
                                  FunctionType::get(tv, instanceTy, context),
                                  expr);

      // The first type must be convertible to the constructor's argument type.
      CS.addConstraint(ConstraintKind::Conversion, argTy, tv, expr);
      return instanceTy;
    }

    Type visitMetatypeExpr(MetatypeExpr *expr) {
      auto base = expr->getBase();

      // If this is an artificial MetatypeExpr, it's fully type-checked.
      if (!base) return expr->getType();

      auto tv = CS.createTypeVariable();
      CS.addConstraint(ConstraintKind::EqualRvalue, tv,
                       base->getType(), expr);

      return MetaTypeType::get(tv, CS.getASTContext());
    }

    Type visitOpaqueValueExpr(OpaqueValueExpr *expr) {
      llvm_unreachable("Already type-checked");
    }

    Type visitApplyExpr(ApplyExpr *expr) {
      ASTContext &Context = CS.getASTContext();

      // If the function subexpression has metatype type, this is a type
      // coercion or construction.
      // Note that matching the metatype type here, within constraint
      // generation, naturally restricts the use of metatypes to whatever
      // the constraint generator can eagerly evaluate.
      // FIXME: Specify this as a syntactic restriction on the form that one
      // can use for a coercion or type construction.
      if (isa<CallExpr>(expr)) {
        auto fnTy = CS.simplifyType(expr->getFn()->getType());
        if (auto metaTy = fnTy->getAs<MetaTypeType>()) {
          auto instanceTy = metaTy->getInstanceType();
          CS.addConstraint(ConstraintKind::Construction,
                           expr->getArg()->getType(), instanceTy, expr);
          return instanceTy;
        }
      }

      // The function subexpression has some rvalue type T1 -> T2 for fresh
      // variables T1 and T2.
      auto inputTy = CS.createTypeVariable(expr);
      auto outputTy = CS.createTypeVariable(expr);
      auto funcTy = FunctionType::get(inputTy, outputTy, Context);
      CS.addConstraint(ConstraintKind::EqualRvalue, funcTy,
                       expr->getFn()->getType(), expr);

      // The argument type must be convertible to the input type.
      CS.addConstraint(ConstraintKind::Conversion, expr->getArg()->getType(),
                       inputTy, expr);

      return outputTy;
    }
    
    Type visitSuperConstructorRefCallExpr(SuperConstructorRefCallExpr *expr) {
      // If the constructor ref was already resolved, handle as a normal
      // ApplyExpr.
      if (expr->getFn())
        return visitApplyExpr(expr);
      
      // If the 'this' argument was not assigned, the form was deemed invalid
      // at parse time.
      if (!expr->getArg() || expr->getArg()->getType()->is<ErrorType>())
        return Type();
      
      // Look up the base class type.
      ValueDecl *thisDecl = cast<DeclRefExpr>(expr->getArg())->getDecl();
      DeclContext *typeContext = thisDecl->getDeclContext()->getParent();
      assert(typeContext && "constructor without parent context?!");
      ClassDecl *classDecl = dyn_cast<ClassDecl>(typeContext);
      if (!classDecl) {
        CS.TC.diagnose(expr->getLoc(),
                       diag::super_constructor_not_in_class_constructor);
        return Type();
      }
      if (!classDecl->hasBaseClass()) {
        CS.TC.diagnose(expr->getLoc(),
                       diag::super_constructor_with_no_base_class);
        return Type();
      }
      Type superTy = classDecl->getBaseClass();
      
      // Add a member constraint for constructors on the base class.
      ASTContext &C = CS.getASTContext();
      auto argsTy = CS.createTypeVariable(expr);
      auto methodTy = FunctionType::get(argsTy, superTy, C);
      CS.addValueMemberConstraint(superTy,
                                  C.getIdentifier("constructor"),
                                  methodTy, expr);
      // The result of the expression is the partial application of the
      // super constructor to 'this'.
      return methodTy;
    }

    Type visitImplicitConversionExpr(ImplicitConversionExpr *expr) {
      llvm_unreachable("Already type-checked");
    }

    Type visitCoerceExpr(CoerceExpr *expr) {
      llvm_unreachable("Already type-checked");
    }

    Type visitDowncastExpr(DowncastExpr *expr) {
      llvm_unreachable("Already type-checked");
    }

    Type visitSuperToArchetypeExpr(SuperToArchetypeExpr *expr) {
      llvm_unreachable("Already type-checked");
    }
  };

  /// \brief AST walker that "sanitizes" an expression for the
  /// constraint-based type checker.
  ///
  /// This is only necessary because Sema fills in too much type information
  /// before the type-checker runs, causing redundant work.
  class SanitizeExpr : public ASTWalker {
    TypeChecker &TC;
  public:
    SanitizeExpr(TypeChecker &tc) : TC(tc) { }

    virtual bool walkToExprPre(Expr *expr) {
      // Don't recurse into array-new expressions.
      return !isa<NewArrayExpr>(expr);
    }

    virtual Expr *walkToExprPost(Expr *expr) {
      if (auto implicit = dyn_cast<ImplicitConversionExpr>(expr)) {
        // Skip implicit conversions completely.
        return implicit->getSubExpr();
      }

      if (auto dotCall = dyn_cast<DotSyntaxCallExpr>(expr)) {
        // A DotSyntaxCallExpr is a member reference that has already been
        // type-checked down to a call; turn it back into an overloaded
        // member reference expression.
        SourceLoc memberLoc;
        if (auto member = findReferencedDecl(dotCall->getFn(), memberLoc)) {
          auto base = skipImplicitConversions(dotCall->getArg());
          auto members
            = TC.Context.AllocateCopy(ArrayRef<ValueDecl *>(&member, 1));
          return new (TC.Context) OverloadedMemberRefExpr(base,
                                   dotCall->getDotLoc(), members, memberLoc,
                                   UnstructuredUnresolvedType::get(TC.Context));
        }
      }

      if (auto dotIgnored = dyn_cast<DotSyntaxBaseIgnoredExpr>(expr)) {
        // A DotSyntaxCallExpr is a member reference that has already been
        // type-checked down to a call where the argument doesn't actually
        // matter; turn it back into an overloaded member reference expression.
        SourceLoc memberLoc;
        if (auto member = findReferencedDecl(dotIgnored->getRHS(), memberLoc)) {
          auto base = skipImplicitConversions(dotIgnored->getLHS());
          auto members
            = TC.Context.AllocateCopy(ArrayRef<ValueDecl *>(&member, 1));
          return new (TC.Context) OverloadedMemberRefExpr(base,
                                    dotIgnored->getDotLoc(), members, memberLoc,
                                    UnstructuredUnresolvedType::get(TC.Context));
        }
      }
      
      return expr;
    }
  };

  class ConstraintWalker : public ASTWalker {
    ConstraintGenerator &CG;

  public:
    ConstraintWalker(ConstraintGenerator &CG) : CG(CG) { }

    virtual bool walkToExprPre(Expr *expr) {
      // For closures, we visit the closure first to assign a type with
      // appropriate type variables.
      // Don't walk into the bodies of function expressions.
      if (isa<ClosureExpr>(expr)) {
        auto type = CG.visit(expr);
        expr->setType(type);

        // For explicit closures, we visit the body, which participates in
        // type checking. The bodies of func expressions, on the other hand,
        // do not participate in type checking.
        return isa<ExplicitClosureExpr>(expr);
      }

      // For new array expressions, we visit the node but not any of its
      // children.
      // FIXME: If new array expressions gain an initializer, we'll need to
      // visit that first.
      if (auto newArray = dyn_cast<NewArrayExpr>(expr)) {
        auto type = CG.visitNewArrayExpr(newArray);
        expr->setType(type);
        return false;
      }

      return true;
    }

    /// \brief Once we've visited the children of the given expression,
    /// generate constraints from the expression.
    virtual Expr *walkToExprPost(Expr *expr) {
      if (auto explicitClosure = dyn_cast<ExplicitClosureExpr>(expr)) {
        // For explicit closures, we've already assigned a type to the
        // closure expression. All that remains is to convert the type of the
        // body expression to the type of the closure.
        CG.getConstraintSystem()
          .addConstraint(ConstraintKind::Conversion,
                         explicitClosure->getBody()->getType(),
                         explicitClosure->getType()->castTo<FunctionType>()
                           ->getResult(),
                         expr);
        return expr;
      }

      if (auto type = CG.visit(expr)) {
        expr->setType(type);
        return expr;
      }

      return nullptr;
    }

    /// \brief Ignore statements.
    virtual bool walkToStmtPre(Stmt *stmt) { return false; }

    /// \brief Ignore declarations.
    virtual bool walkToDeclPre(Decl *decl) { return false; }
  };

  // Remove implicit conversions from the expression.
  expr = expr->walk(SanitizeExpr(getTypeChecker()));

  // Walk the expression, generating constraints.
  ConstraintGenerator CG(*this);
  ConstraintWalker CW(CG);
  return expr->walk(CW) == nullptr;
}

SmallVector<Type, 4> ConstraintSystem::enumerateDirectSupertypes(Type type) {
  SmallVector<Type, 4> result;

  if (auto tupleTy = type->getAs<TupleType>()) {
    // A one-element tuple can be viewed as a scalar by dropping the label.
    // FIXME: There is a way more general property here, where we can drop
    // one label from the tuple, maintaining the rest.
    if (tupleTy->getFields().size() == 1) {
      auto &elt = tupleTy->getFields()[0];
      if (elt.isVararg()) // FIXME: Should we keep the name?
        result.push_back(elt.getVarargBaseTy());
      else if (!elt.getName().empty())
        result.push_back(elt.getType());
    }
  }

  if (auto functionTy = type->getAs<FunctionType>()) {
    // FIXME: Can weaken input type, but we really don't want to get in the
    // business of strengthening the result type.

    // An [auto_closure] function type can be viewed as scalar of the result
    // type.
    if (functionTy->isAutoClosure())
      result.push_back(functionTy->getResult());
  }

  if (type->mayHaveSuperclass()) {
    // FIXME: Can also weaken to the set of protocol constraints, but only
    // if there are any protocols that the type conforms to but the superclass
    // does not.

    // If there is a superclass, it is a direct supertype.
    if (auto superclass = TC.getSuperClassOf(type))
      result.push_back(superclass);
  }

  // FIXME: lots of other cases to consider!
  return result;
}

//===--------------------------------------------------------------------===//
// Constraint simplification
//===--------------------------------------------------------------------===//
#pragma mark Constraint simplification

ConstraintSystem::SolutionKind
ConstraintSystem::matchTupleTypes(TupleType *tuple1, TupleType *tuple2,
                                  TypeMatchKind kind, unsigned flags,
                                  bool &trivial) {
  unsigned subFlags = flags | TMF_GenerateConstraints;

  // Equality and subtyping have fairly strict requirements on tuple matching,
  // requiring element names to either match up or be disjoint.
  if (kind < TypeMatchKind::Conversion) {
    if (tuple1->getFields().size() != tuple2->getFields().size())
      return SolutionKind::Error;

    SolutionKind result = SolutionKind::TriviallySolved;
    for (unsigned i = 0, n = tuple1->getFields().size(); i != n; ++i) {
      const auto &elt1 = tuple1->getFields()[i];
      const auto &elt2 = tuple2->getFields()[i];

      // If the names don't match, we may have a conflict.
      if (elt1.getName() != elt2.getName()) {
        // Same-type requirements require exact name matches.
        if (kind == TypeMatchKind::SameType)
          return SolutionKind::Error;

        // For subtyping constraints, just make sure that this name isn't
        // used at some other position.
        if (!elt2.getName().empty()) {
          int matched = tuple1->getNamedElementId(elt2.getName());
          if (matched != -1)
            return SolutionKind::Error;
        }
      }

      // Variadic bit must match.
      if (elt1.isVararg() != elt2.isVararg())
        return SolutionKind::Error;

      // Compare the element types.
      switch (matchTypes(elt1.getType(), elt2.getType(), kind, subFlags,
                         trivial)) {
      case SolutionKind::Error:
        return SolutionKind::Error;

      case SolutionKind::TriviallySolved:
        break;

      case SolutionKind::Solved:
        result = SolutionKind::Solved;
        break;

      case SolutionKind::Unsolved:
        result = SolutionKind::Unsolved;
        break;
      }
    }
    return result;
  }

  assert(kind == TypeMatchKind::Conversion ||
         kind == TypeMatchKind::Construction);

  // Compute the element shuffles for conversions.
  enum {
    unassigned = -1,
    defaultValue = -2,
    variadic = -3
  };
  SmallVector<int, 16> elementSources(tuple2->getFields().size(), unassigned);
  SmallVector<bool, 16> consumed(tuple1->getFields().size(), false);
  SmallVector<unsigned, 4> variadicArguments;
  
  // Match up any named elements.
  for (unsigned i = 0, n = tuple2->getFields().size(); i != n; ++i) {
    const auto &elt2 = tuple2->getFields()[i];

    // Skip unnamed elements.
    if (elt2.getName().empty())
      continue;

    // Find the corresponding named element.
    int matched = -1;
    {
      int index = 0;
      for (auto field : tuple1->getFields()) {
        if (field.getName() == elt2.getName() && !consumed[index]) {
          matched = index;
          break;
        }
        ++index;
      }
    }
    if (matched == -1)
      continue;

    // If this is not a conversion constraint, shuffles are not permitted.
    if (i != (unsigned)matched && kind < TypeMatchKind::Conversion) {
      return SolutionKind::Error;
    }

    // Record this match.
    elementSources[i] = matched;
    consumed[matched] = true;
  }

  // Resolve any unmatched elements.
  unsigned next1 = 0, last1 = tuple1->getFields().size();
  auto skipToNextUnnamedInput = [&] {
    while (next1 != last1 &&
           (consumed[next1] || !tuple1->getFields()[next1].getName().empty()))
      ++next1;
  };
  skipToNextUnnamedInput();

  for (unsigned i = 0, n = tuple2->getFields().size(); i != n; ++i) {
    // Check whether we already found a value for this element.
    if (elementSources[i] != unassigned)
      continue;

    const auto &elt2 = tuple2->getFields()[i];

    // Variadic tuple elements match the rest of the input elements.
    if (elt2.isVararg()) {
      // Collect the remaining (unnamed) inputs.
      while (next1 != last1) {
        variadicArguments.push_back(next1);
        consumed[next1] = true;
        skipToNextUnnamedInput();
      }
      elementSources[i] = variadic;
      break;
    }

    // If there aren't any more inputs, we can use a default argument.
    if (next1 == last1) {
      if (elt2.hasInit()) {
        elementSources[i] = defaultValue;
        continue;
      }

      return SolutionKind::Error;
    }

    elementSources[i] = next1;
    consumed[next1] = true;
    skipToNextUnnamedInput();
  }

  // Check whether there were any unused input values.
  // FIXME: Could short-circuit this check, above, by not skipping named
  // input values.
  if (std::find(consumed.begin(), consumed.end(), false) != consumed.end())
    return SolutionKind::Error;

  // Check conversion constraints on the individual elements.
  SolutionKind result = SolutionKind::TriviallySolved;
  for (unsigned i = 0, n = tuple2->getFields().size(); i != n; ++i) {
    int source = elementSources[i];
    const auto &elt2 = tuple2->getFields()[i];

    assert(source != unassigned && "Cannot have unassigned source here");

    // Default values always work.
    if (source == defaultValue) {
      assert(elt2.hasInit() && "Bogus default value source");
      continue;
    }

    // For variadic elements, check the list of variadic arguments.
    if (source == variadic) {
      assert(elt2.isVararg() && "Bogus variadic argument source");
      Type variadicBaseType = elt2.getVarargBaseTy();
      for (unsigned variadicSrc : variadicArguments) {
        switch (matchTypes(tuple1->getElementType(variadicSrc),
                           variadicBaseType, kind, subFlags, trivial)) {
        case SolutionKind::Error:
          return SolutionKind::Error;

        case SolutionKind::TriviallySolved:
          break;

        case SolutionKind::Solved:
          result = SolutionKind::Solved;
          break;

        case SolutionKind::Unsolved:
          result = SolutionKind::Unsolved;
          break;
        }
      }
      continue;
    }

    const auto &elt1 = tuple1->getFields()[source];

    switch (matchTypes(elt1.getType(), elt2.getType(),
                       TypeMatchKind::Conversion, subFlags, trivial)) {
    case SolutionKind::Error:
      return SolutionKind::Error;

    case SolutionKind::TriviallySolved:
      break;

    case SolutionKind::Solved:
      result = SolutionKind::Solved;
      break;

    case SolutionKind::Unsolved:
      result = SolutionKind::Unsolved;
      break;
    }
  }

  return result;
}

ConstraintSystem::SolutionKind
ConstraintSystem::matchFunctionTypes(FunctionType *func1, FunctionType *func2,
                                     TypeMatchKind kind, unsigned flags,
                                     bool &trivial) {
  // An [auto_closure] function type can be a subtype of a
  // non-[auto_closure] function type.
  if (func1->isAutoClosure() != func2->isAutoClosure()) {
    if (func2->isAutoClosure())
      return SolutionKind::Error;

    if (kind < TypeMatchKind::TrivialSubtype)
      return SolutionKind::Error;
  }

  // Determine how we match up the input/result types.
  TypeMatchKind subKind;
  switch (kind) {
  case TypeMatchKind::BindType:
  case TypeMatchKind::SameType:
  case TypeMatchKind::SameTypeRvalue:
  case TypeMatchKind::TrivialSubtype:
    subKind = kind;
    break;

  case TypeMatchKind::Subtype:
    subKind = TypeMatchKind::TrivialSubtype;
    break;

  case TypeMatchKind::Conversion:
    subKind = TypeMatchKind::Subtype;
    break;

  case TypeMatchKind::Construction:
    subKind = TypeMatchKind::Conversion;
    break;
  }

  unsigned subFlags = flags | TMF_GenerateConstraints;
  // Input types can be contravariant (or equal).
  SolutionKind result = matchTypes(func2->getInput(), func1->getInput(),
                                   subKind, subFlags, trivial);
  if (result == SolutionKind::Error)
    return SolutionKind::Error;

  // Result type can be covariant (or equal).
  switch (matchTypes(func1->getResult(), func2->getResult(), subKind,
                     subFlags, trivial)) {
  case SolutionKind::Error:
    return SolutionKind::Error;

  case SolutionKind::TriviallySolved:
    break;

  case SolutionKind::Solved:
    result = SolutionKind::Solved;
    break;

  case SolutionKind::Unsolved:
    result = SolutionKind::Unsolved;
    break;
  }

  return result;
}

/// \brief Map a type-matching kind to a constraint kind.
static ConstraintKind getConstraintKind(TypeMatchKind kind) {
  switch (kind) {
  case TypeMatchKind::BindType:
    return ConstraintKind::Bind;

  case TypeMatchKind::SameType:
    return ConstraintKind::Equal;

  case TypeMatchKind::SameTypeRvalue:
    return ConstraintKind::EqualRvalue;

  case TypeMatchKind::TrivialSubtype:
    return ConstraintKind::TrivialSubtype;

  case TypeMatchKind::Subtype:
    return ConstraintKind::Subtype;

  case TypeMatchKind::Conversion:
    return ConstraintKind::Conversion;

  case TypeMatchKind::Construction:
    return ConstraintKind::Construction;
  }

  llvm_unreachable("unhandled type matching kind");
}

ConstraintSystem::SolutionKind
ConstraintSystem::matchTypes(Type type1, Type type2, TypeMatchKind kind,
                             unsigned flags, bool &trivial) {
  // Desugar both types.
  auto desugar1 = type1->getDesugaredType();
  auto desugar2 = type2->getDesugaredType();

  // If we have type variables that have been bound to fixed types, look through
  // to the fixed type.
  auto typeVar1 = dyn_cast<TypeVariableType>(desugar1);
  if (typeVar1) {
    if (auto fixed = getFixedType(typeVar1)) {
      type1 = fixed;
      desugar1 = fixed->getDesugaredType();
      typeVar1 = nullptr;
    }
  }

  auto typeVar2 = dyn_cast<TypeVariableType>(desugar2);
  if (typeVar2) {
    if (auto fixed = getFixedType(typeVar2)) {
      type2 = fixed;
      desugar2 = fixed->getDesugaredType();
      typeVar2 = nullptr;
    }
  }

  // If we have a same-type-as-rvalue constraint, and the right-hand side
  // has a form that is either definitely an lvalue or definitely an rvalue,
  // force the right-hand side to be an rvalue and
  if (kind == TypeMatchKind::SameTypeRvalue) {
    if (isa<LValueType>(desugar2)) {
      // The right-hand side is an lvalue type. Strip off the lvalue and
      // call this a normal 'same-type' constraint.
      type2 = type2->castTo<LValueType>()->getObjectType();
      desugar2 = type2->getDesugaredType();
      kind = TypeMatchKind::SameType;
      flags |= TMF_GenerateConstraints;
    } else if (!type2->is<TypeVariableType>()) {
      // The right-hand side is guaranteed to be an rvalue type. Call this
      // a normal same-type constraint.
      kind = TypeMatchKind::SameType;
      flags |= TMF_GenerateConstraints;
    }

    if (auto desugarFunc2 = dyn_cast<FunctionType>(desugar2)) {
      // The right-hand side is a function type, which is guaranteed to be
      // an rvalue type. Call this a normal same-type constraint, and
      // strip off the [auto_closure], which is not part of the type.
      if (desugarFunc2->isAutoClosure()) {
        auto func2 = type2->castTo<FunctionType>();
        type2 = FunctionType::get(func2->getInput(), func2->getResult(),
                                  TC.Context);
        desugar2 = type2.getPointer();
      }
      kind = TypeMatchKind::SameType;
      flags |= TMF_GenerateConstraints;
    }
  }

  // If the types are obviously equivalent, we're done.
  if (desugar1 == desugar2)
    return SolutionKind::TriviallySolved;

  // If either (or both) types are type variables, unify the type variables.
  if (typeVar1 || typeVar2) {
    switch (kind) {
    case TypeMatchKind::BindType:
    case TypeMatchKind::SameType: {
      if (typeVar1 && typeVar2) {
        auto rep1 = getRepresentative(typeVar1);
        auto rep2 = getRepresentative(typeVar2);
        if (rep1 == rep2) {
          // We already merged these two types, so this constraint is
          // trivially solved.
          return SolutionKind::TriviallySolved;
        }

        // Merge the equivalence classes corresponding to these two variables.
        mergeEquivalenceClasses(rep1, rep2);
        return SolutionKind::Solved;
      }

      // Provide a fixed type for the type variable.
      bool wantRvalue = kind == TypeMatchKind::SameType;
      if (typeVar1)
        assignFixedType(typeVar1, wantRvalue ? type2->getRValueType() : type2);
      else
        assignFixedType(typeVar2, wantRvalue ? type1->getRValueType() : type1);
      return SolutionKind::Solved;
    }

    case TypeMatchKind::SameTypeRvalue:
    case TypeMatchKind::TrivialSubtype:
    case TypeMatchKind::Subtype:
    case TypeMatchKind::Conversion:
    case TypeMatchKind::Construction:
      if (flags & TMF_GenerateConstraints) {
        // Add a new constraint between these types. We consider the current
        // type-matching problem to the "solved" by this addition, because
        // this new constraint will be solved at a later point.
        // Obviously, this must not happen at the top level, or the algorithm
        // would not terminate.
        addConstraint(getConstraintKind(kind), type1, type2);
        return SolutionKind::Solved;
      }

      // We couldn't solve this constraint. If only one of the types is a type
      // variable, perhaps we can do something with it below.
      if (typeVar1 && typeVar2)
        return typeVar1 == typeVar2? SolutionKind::TriviallySolved
                                   : SolutionKind::Unsolved;
        
      break;
    }
  }

  // Decompose parallel structure.
  unsigned subFlags = flags | TMF_GenerateConstraints;
  if (desugar1->getKind() == desugar2->getKind()) {
    switch (desugar1->getKind()) {
#define SUGARED_TYPE(id, parent) case TypeKind::id:
#define TYPE(id, parent)
#include "swift/AST/TypeNodes.def"
      llvm_unreachable("Type has not been desugared completely");

#define ALWAYS_CANONICAL_TYPE(id, parent) case TypeKind::id:
#define TYPE(id, parent)
#include "swift/AST/TypeNodes.def"
        return desugar1 == desugar2
                 ? SolutionKind::TriviallySolved
                 : SolutionKind::Error;

    case TypeKind::Error:
      return SolutionKind::Error;

    case TypeKind::UnstructuredUnresolved:
      llvm_unreachable("Unstructured unresolved type");

    case TypeKind::TypeVariable:
      llvm_unreachable("Type variables handled above");

    case TypeKind::Tuple: {
      auto tuple1 = cast<TupleType>(desugar1);
      auto tuple2 = cast<TupleType>(desugar2);
      return matchTupleTypes(tuple1, tuple2, kind, flags, trivial);
    }

    case TypeKind::OneOf:
    case TypeKind::Struct:
    case TypeKind::Class: {
      auto nominal1 = cast<NominalType>(desugar1);
      auto nominal2 = cast<NominalType>(desugar2);
      if (nominal1->getDecl() == nominal2->getDecl()) {
        assert((bool)nominal1->getParent() == (bool)nominal2->getParent() &&
               "Mismatched parents of nominal types");

        if (!nominal1->getParent())
          return SolutionKind::TriviallySolved;

        // Match up the parents, exactly.
        return matchTypes(nominal1->getParent(), nominal2->getParent(),
                          TypeMatchKind::SameType, subFlags, trivial);
      }
      break;
    }

    case TypeKind::MetaType: {
      auto meta1 = cast<MetaTypeType>(desugar1);
      auto meta2 = cast<MetaTypeType>(desugar2);

      // metatype<B> < metatype<A> if A < B and both A and B are classes.
      TypeMatchKind subKind = TypeMatchKind::SameType;
      if (kind != TypeMatchKind::SameType &&
          (meta1->getInstanceType()->mayHaveSuperclass() ||
           meta2->getInstanceType()->getClassOrBoundGenericClass()))
        subKind = std::min(kind, TypeMatchKind::Subtype);
      
      return matchTypes(meta1->getInstanceType(), meta2->getInstanceType(),
                        subKind, subFlags, trivial);
    }

    case TypeKind::Function: {
      auto func1 = cast<FunctionType>(desugar1);
      auto func2 = cast<FunctionType>(desugar2);
      return matchFunctionTypes(func1, func2, kind, flags, trivial);
    }

    case TypeKind::PolymorphicFunction:
      llvm_unreachable("Polymorphic function type should have been opened");

    case TypeKind::Array: {
      auto array1 = cast<ArrayType>(desugar1);
      auto array2 = cast<ArrayType>(desugar2);
      return matchTypes(array1->getBaseType(), array2->getBaseType(),
                        TypeMatchKind::SameType, subFlags, trivial);
    }

    case TypeKind::ProtocolComposition:
      // Existential types handled below.
      break;

    case TypeKind::LValue: {
      auto lvalue1 = cast<LValueType>(desugar1);
      auto lvalue2 = cast<LValueType>(desugar2);
      if (lvalue1->getQualifiers() != lvalue2->getQualifiers() &&
          !(kind >= TypeMatchKind::TrivialSubtype &&
            lvalue1->getQualifiers() < lvalue2->getQualifiers()))
        return SolutionKind::Error;

      return matchTypes(lvalue1->getObjectType(), lvalue2->getObjectType(),
                        TypeMatchKind::SameType, subFlags, trivial);
    }

    case TypeKind::UnboundGeneric:
      llvm_unreachable("Unbound generic type should have been opened");

    case TypeKind::BoundGenericClass:
    case TypeKind::BoundGenericOneOf:
    case TypeKind::BoundGenericStruct: {
      auto bound1 = cast<BoundGenericType>(desugar1);
      auto bound2 = cast<BoundGenericType>(desugar2);
      
      if (bound1->getDecl() == bound2->getDecl()) {
        // Match up the parents, exactly, if there are parents.
        SolutionKind result = SolutionKind::TriviallySolved;
        assert((bool)bound1->getParent() == (bool)bound2->getParent() &&
               "Mismatched parents of bound generics");
        if (bound1->getParent()) {
          switch (matchTypes(bound1->getParent(), bound2->getParent(),
                             TypeMatchKind::SameType, subFlags, trivial)) {
          case SolutionKind::Error:
            // There may still be a Conversion or Construction we can satisfy
            // the constraint with.
            // FIXME: The recursive match may have introduced new equality
            // constraints that are now invalid. rdar://problem/13140447
            if (kind >= TypeMatchKind::Conversion)
              break;
              
            return SolutionKind::Error;

          case SolutionKind::TriviallySolved:
            break;

          case SolutionKind::Solved:
            result = SolutionKind::Solved;
            break;

          case SolutionKind::Unsolved:
            result = SolutionKind::Unsolved;
            break;
          }
        }

        // Match up the generic arguments, exactly.
        auto args1 = bound1->getGenericArgs();
        auto args2 = bound2->getGenericArgs();
        assert(args1.size() == args2.size() && "Mismatched generic args");
        for (unsigned i = 0, n = args1.size(); i != n; ++i) {
          switch (matchTypes(args1[i], args2[i], TypeMatchKind::SameType,
                             subFlags, trivial)) {
          case SolutionKind::Error:
            // There may still be a Conversion or Construction we can satisfy
            // the constraint with.
            // FIXME: The recursive match may have introduced new equality
            // constraints that are now invalid. rdar://problem/13140447
            if (kind >= TypeMatchKind::Conversion)
              break;

            return SolutionKind::Error;

          case SolutionKind::TriviallySolved:
            break;

          case SolutionKind::Solved:
            result = SolutionKind::Solved;
            break;

          case SolutionKind::Unsolved:
            result = SolutionKind::Unsolved;
            break;
          }
        }

        return result;
      }
      break;
    }
    }
  }

  // FIXME: Materialization

  bool concrete = !typeVar1 && !typeVar2;
  if (concrete && kind >= TypeMatchKind::TrivialSubtype) {
    if (auto tuple2 = type2->getAs<TupleType>()) {
      // A scalar type is a trivial subtype of a one-element, non-variadic tuple
      // containing a single element if the scalar type is a subtype of
      // the type of that tuple's element.
      if (tuple2->getFields().size() == 1 &&
          !tuple2->getFields()[0].isVararg()) {
        return matchTypes(type1, tuple2->getElementType(0), kind, subFlags,
                          trivial);
      }

      // A scalar type can be converted to a tuple so long as there is at
      // most one non-defaulted element.
      if (kind >= TypeMatchKind::Conversion) {
        int scalarFieldIdx = tuple2->getFieldForScalarInit();
        if (scalarFieldIdx >= 0) {
          const auto &elt = tuple2->getFields()[scalarFieldIdx];
          auto scalarFieldTy = elt.isVararg()? elt.getVarargBaseTy()
                                             : elt.getType();
          return matchTypes(type1, scalarFieldTy, kind, subFlags, trivial);
        }
      }
    }

    if (type1->mayHaveSuperclass() && type2->mayHaveSuperclass()) {
      // Determines whether the first type is derived from the second.
      auto solveDerivedFrom =
        [&](Type type1, Type type2) -> Optional<SolutionKind> {
          if (auto archetype2 = type2->getAs<ArchetypeType>()) {
            type2 = archetype2->getSuperclass();
          }
          auto classDecl2 = type2->getClassOrBoundGenericClass();

          for (auto super1 = TC.getSuperClassOf(type1); super1;
               super1 = TC.getSuperClassOf(super1)) {
            if (super1->getClassOrBoundGenericClass() != classDecl2)
              continue;

            switch (auto result = matchTypes(super1, type2, TypeMatchKind::SameType,
                                             subFlags, trivial)) {
              case SolutionKind::Error:
                continue;

              case SolutionKind::Solved:
              case SolutionKind::TriviallySolved:
              case SolutionKind::Unsolved:
                return result;
            }
          }

          return Nothing;
        };

      // A class (or bound generic class) is a subtype of another class
      // (or bound generic class) if it is derived from that class.
      if (auto upcastResult = solveDerivedFrom(type1, type2))
        return *upcastResult;

      // A class can be downcast to its subclass as part of a 'construction'
      // constraint.
      if (kind >= TypeMatchKind::Construction) {
        if (auto downcastResult = solveDerivedFrom(type2, type1))
          return *downcastResult;
      }
    }
  }

  if (concrete && kind >= TypeMatchKind::Conversion) {
    // An lvalue of type T1 can be converted to a value of type T2 so long as
    // T1 is convertible to T2 (by loading the value).
    if (auto lvalue1 = type1->getAs<LValueType>()) {
      return matchTypes(lvalue1->getObjectType(), type2, kind, subFlags,
                        trivial);
    }

    // An expression can be converted to an auto-closure function type, creating
    // an implicit closure.
   if (auto function2 = type2->getAs<FunctionType>()) {
      if (function2->isAutoClosure()) {
        trivial = false;
        return matchTypes(type1, function2->getResult(), kind, subFlags,
                          trivial);
      }
    }
  }

  // For a subtyping relation involving two existential types, or a conversion
  // from any type, check whether the first type conforms to each of
  if (concrete &&
      (kind >= TypeMatchKind::Conversion ||
       (kind == TypeMatchKind::Subtype && type1->isExistentialType()))) {
    SmallVector<ProtocolDecl *, 4> protocols;
    if (!type1->hasTypeVariable() && type2->isExistentialType(protocols)) {
      for (auto proto : protocols) {
        if (!TC.conformsToProtocol(type1, proto))
          return SolutionKind::Error;
      }

      trivial = false;
      return SolutionKind::Solved;
    }
  }
  
  // A type can be constructed by passing an argument to one of its
  // constructors. This construction only applies to oneof and struct types
  // (or generic versions of oneof or struct types).
  if (kind == TypeMatchKind::Construction && isConstructibleType(type2)) {
    ConstructorLookup constructors(type2, TC.TU);
    if (constructors.isSuccess()) {
      auto &context = getASTContext();
      // FIXME: lame name
      auto name = context.getIdentifier("constructor");
      auto tv = createTypeVariable();

      // The constructor will have function type T -> T2, for a fresh type
      // variable T. Note that these constraints specifically require a
      // match on the result type because the constructors for oneofs and struct
      // types always return a value of exactly that type.
      addValueMemberConstraint(type2, name,
                               FunctionType::get(tv, type2, context));

      // The first type must be convertible to the constructor's argument type.
      addConstraint(ConstraintKind::Conversion, type1, tv);
      
      // FIXME: Do we want to consider conversion functions simultaneously with
      // constructors? Right now, we prefer constructors if they exist.
      return SolutionKind::Solved;
    }
  }

  // A nominal type can be converted to another type via a user-defined
  // conversion function.
  if (concrete && kind >= TypeMatchKind::Conversion &&
      type1->getNominalOrBoundGenericNominal()) {
    auto &context = getASTContext();
    auto name = context.getIdentifier("__conversion");
    MemberLookup &lookup = lookupMember(type1, name);
    if (lookup.isSuccess()) {
      auto inputTV = createTypeVariable();
      auto outputTV = createTypeVariable();

      // The conversion function will have function type TI -> TO, for fresh
      // type variables TI and TO.
      // FIXME: lame name!
      addValueMemberConstraint(type1, name,
                               FunctionType::get(inputTV, outputTV, context));

      // A conversion function must accept an empty parameter list ().
      addConstraint(ConstraintKind::Conversion, TupleType::getEmpty(context),
                    inputTV);

      // The output of the conversion function must be a subtype of the
      // type we're trying to convert to. The use of subtyping here eliminates
      // multiple-step user-defined conversions, which also eliminates concerns
      // about cyclic conversions causing infinite loops in the constraint
      // solver.
      addConstraint(ConstraintKind::Subtype, outputTV, type2);
      return SolutionKind::Solved;
    }
  }

  // If one of the types is a type variable, we leave this unsolved.
  if (typeVar1 || typeVar2)
    return SolutionKind::Unsolved;

  return SolutionKind::Error;
}

/// \brief Retrieve the fully-materialized form of the given type.
static Type getMateralizedType(Type type, ASTContext &context) {
  if (auto lvalue = type->getAs<LValueType>()) {
    return lvalue->getObjectType();
  }

  if (auto tuple = type->getAs<TupleType>()) {
    bool anyChanged = false;
    SmallVector<TupleTypeElt, 4> elements;
    for (unsigned i = 0, n = tuple->getFields().size(); i != n; ++i) {
      auto elt = tuple->getFields()[i];
      auto eltType = getMateralizedType(elt.getType(), context);
      if (anyChanged) {
        elements.push_back(elt.getWithType(eltType));
        continue;
      }

      if (eltType.getPointer() != elt.getType().getPointer()) {
        elements.append(tuple->getFields().begin(),
                        tuple->getFields().begin() + i);
        elements.push_back(elt.getWithType(eltType));
        anyChanged = true;
      }
    }

    if (anyChanged) {
      return TupleType::get(elements, context);
    }
  }

  return type;
}

void ConstraintSystem::resolveOverload(OverloadSet *ovl, unsigned idx) {
  // Determie the type to which we'll bind the overload set's type.
  auto &choice = ovl->getChoices()[idx];
  Type refType;
  switch (choice.getKind()) {
  case OverloadChoiceKind::Decl: {
    // Retrieve the type of a reference to the specific declaration choice.
    if (choice.getBaseType())
      refType = getTypeOfMemberReference(choice.getBaseType(),
                                         choice.getDecl(),
                                         /*FIXME:*/false);
    else
      refType = getTypeOfReference(choice.getDecl());

    bool isAssignment = choice.getDecl()->getAttrs().isAssignment();
    refType = adjustLValueForReference(refType, isAssignment,
                                       getASTContext());
    break;
  }

  case OverloadChoiceKind::BaseType:
    refType = choice.getBaseType();
    break;

  case OverloadChoiceKind::FunctionReturningBaseType:
    refType = FunctionType::get(createTypeVariable(),
                                choice.getBaseType(),
                                getASTContext());
    break;
  case OverloadChoiceKind::IdentityFunction:
    refType = FunctionType::get(choice.getBaseType(), choice.getBaseType(),
                                getASTContext());
    break;

  case OverloadChoiceKind::TupleIndex: {
    if (auto lvalueTy = choice.getBaseType()->getAs<LValueType>()) {
      // When the base of a tuple lvalue, the member is always an lvalue.
      auto tuple = lvalueTy->getObjectType()->castTo<TupleType>();
      refType = tuple->getElementType(choice.getTupleIndex())->getRValueType();
      refType = LValueType::get(refType, lvalueTy->getQualifiers(),
                                getASTContext());
    } else {
      // When the base is a tuple rvalue, the member is always an rvalue.
      // FIXME: Do we have to strip several levels here? Possible.
      auto tuple = choice.getBaseType()->castTo<TupleType>();
      refType =getMateralizedType(tuple->getElementType(choice.getTupleIndex()),
                                  getASTContext());
    }
    break;
  }
  }

  // Add the type binding constraint.
  addConstraint(ConstraintKind::Bind, ovl->getBoundType(), refType);

  // Note that we have resolved this overload.
  ResolvedOverloads[ovl] = std::make_pair(idx, refType);
}

Type ConstraintSystem::simplifyType(Type type,
       llvm::SmallPtrSet<TypeVariableType *, 16> &substituting) {
  return TC.transformType(type,
                          [&](Type type) -> Type {
            if (auto tvt = dyn_cast<TypeVariableType>(type.getPointer())) {
              tvt = getRepresentative(tvt);
              if (auto fixed = getFixedType(tvt)) {
                if (substituting.insert(tvt)) {
                  auto result = simplifyType(fixed, substituting);
                  substituting.erase(tvt);
                  return result;
                }
              }

              return tvt;
            }
                            
            return type;
         });
}

ConstraintSystem::SolutionKind
ConstraintSystem::simplifyLiteralConstraint(Type type, LiteralKind kind) {
  if (auto tv = dyn_cast<TypeVariableType>(type.getPointer())) {
    auto fixed = getFixedType(tv);
    if (!fixed)
      return SolutionKind::Unsolved;

    // Continue with the fixed type.
    type = fixed;
  }

  // Have we already checked whether this type is literal compatible?
  auto typePtr = type->getCanonicalType().getPointer();
  auto known = SharedState->LiteralChecks.find({typePtr, kind});
  if (known != SharedState->LiteralChecks.end())
    return known->second? SolutionKind::TriviallySolved : SolutionKind::Error;

  // We have not yet checked this type; check it now, and cache the result.
  // FIXME: We should do this caching in the translation unit.
  bool &result = SharedState->LiteralChecks[{typePtr, kind}];
  result = TC.isLiteralCompatibleType(type, SourceLoc(), kind,
                                      /*Complain=*/false).first;

  return result? SolutionKind::TriviallySolved : SolutionKind::Error;
}

ConstraintSystem::SolutionKind
ConstraintSystem::simplifyMemberConstraint(const Constraint &constraint) {
  // Resolve the base type, if we can. If we can't resolve the base type,
  // then we can't solve this constraint.
  Type baseTy = simplifyType(constraint.getFirstType());
  Type baseObjTy = baseTy->getRValueType();
  
  if (baseObjTy->is<TypeVariableType>())
    return SolutionKind::Unsolved;
  
  // If the base type is a tuple type, look for the named or indexed member
  // of the tuple.
  Identifier name = constraint.getMember();
  Type memberTy = constraint.getSecondType();
  if (auto baseTuple = baseObjTy->getAs<TupleType>()) {
    StringRef nameStr = name.str();
    int fieldIdx = -1;
    if (nameStr[0] == '$') {
      // Resolve a number reference into the tuple type.
      unsigned Value = 0;
      if (!nameStr.substr(1).getAsInteger(10, Value) &&
          Value < baseTuple->getFields().size())
        fieldIdx = Value;
    } else {
      fieldIdx = baseTuple->getNamedElementId(name);
    }

    if (fieldIdx == -1)
      return SolutionKind::Error;

    // Add an overload set that selects this field.
    OverloadChoice choice(constraint.getFirstType(), fieldIdx);
    addOverloadSet(OverloadSet::getNew(*this, memberTy, &constraint,
                                       { &choice, 1 }));
    return SolutionKind::Solved;
  }

  // FIXME: If the base type still involves type variables, we want this
  // constraint to be unsolved. This effectively requires us to solve the
  // left-hand side of a dot expression before we look for members.

  if (name.str() == "constructor") {
    // Constructors have their own approach to name lookup.
    ConstructorLookup constructors(baseObjTy, TC.TU);
    if (!constructors.isSuccess()) {
      return SolutionKind::Error;
    }

    // Check whether we have an 'identity' constructor.
    bool needIdentityConstructor = true;
    if (baseObjTy->getClassOrBoundGenericClass()) {
      // When we are constructing a class type, there is no coercion case
      // to consider.
      needIdentityConstructor = false;
    } else {
      for (auto constructor : constructors.Results) {
        if (auto funcTy = constructor->getType()->getAs<FunctionType>()) {
          if ((funcTy = funcTy->getResult()->getAs<FunctionType>())) {
            // Dig out the input type.
            auto inputTy = funcTy->getInput();
            if (auto inputTupleTy = inputTy->getAs<TupleType>()) {
              int scalarIdx = inputTupleTy->getFieldForScalarInit();
              if (scalarIdx >= 0) {
                inputTy = inputTupleTy->getElementType(scalarIdx);
              }
            }

            if (inputTy->isEqual(baseObjTy)) {
              needIdentityConstructor = false;
              break;
            }
          }
        }
      }
    }
    
    // Introduce a new overload set.
    SmallVector<OverloadChoice, 4> choices;
    for (auto constructor : constructors.Results) {
      choices.push_back(OverloadChoice(baseTy, constructor));
    }

    // If we need an "identity" constructor, then add an entry in the
    // overload set for T -> T, where T is the base type. This entry acts as a
    // stand-in for conversion of the argument to T.
    if (needIdentityConstructor) {
      choices.push_back(OverloadChoice(baseTy,
                                       OverloadChoiceKind::IdentityFunction));
    }
    addOverloadSet(OverloadSet::getNew(*this, memberTy, &constraint, choices));
    return SolutionKind::Solved;
  }

  // Look for members within the base.
  MemberLookup &lookup = lookupMember(baseObjTy, name);
  if (!lookup.isSuccess()) {
    return SolutionKind::Error;
  }

  // Can't refer to a property without an object instance.
  // FIXME: Subscripts have a similar restriction. Check it here.
  bool isMetatypeBase = baseObjTy->getRValueType()->is<MetaTypeType>();
  if (isMetatypeBase && lookup.Results.size() == 1 &&
      lookup.Results[0].Kind == MemberLookupResult::MemberProperty) {
    return SolutionKind::Error;
  }

  // FIXME: If we expect a type member, make sure we actually got type members.

  // Introduce a new overload set to capture the choices.
  SmallVector<OverloadChoice, 4> choices;
  for (auto &result : lookup.Results) {
    choices.push_back(OverloadChoice(baseTy, result.D));
  }
  addOverloadSet(OverloadSet::getNew(*this, memberTy, &constraint, choices));
  return SolutionKind::Solved;
}

ConstraintSystem::SolutionKind
ConstraintSystem::simplifyArchetypeConstraint(const Constraint &constraint) {
  // Resolve the base type, if we can. If we can't resolve the base type,
  // then we can't solve this constraint.
  Type baseTy = constraint.getFirstType()->getRValueType();
  if (auto tv = dyn_cast<TypeVariableType>(baseTy.getPointer())) {
    auto fixed = getFixedType(tv);
    if (!fixed)
      return SolutionKind::Unsolved;

    // Continue with the fixed type.
    baseTy = fixed->getRValueType();
  }

  return baseTy->is<ArchetypeType>()? SolutionKind::TriviallySolved
                                    : SolutionKind::Error;
}

/// \brief Retrieve the type-matching kind corresponding to the given
/// constraint kind.
static TypeMatchKind getTypeMatchKind(ConstraintKind kind) {
  switch (kind) {
  case ConstraintKind::Bind: return TypeMatchKind::BindType;
  case ConstraintKind::Equal: return TypeMatchKind::SameType;
  case ConstraintKind::EqualRvalue: return TypeMatchKind::SameTypeRvalue;
  case ConstraintKind::TrivialSubtype: return TypeMatchKind::TrivialSubtype;
  case ConstraintKind::Subtype: return TypeMatchKind::Subtype;
  case ConstraintKind::Conversion: return TypeMatchKind::Conversion;
  case ConstraintKind::Construction: return TypeMatchKind::Construction;

  case ConstraintKind::Literal:
    llvm_unreachable("Literals don't involve type matches");

  case ConstraintKind::ValueMember:
  case ConstraintKind::TypeMember:
    llvm_unreachable("Member constraints don't involve type matches");

  case ConstraintKind::Archetype:
    llvm_unreachable("Archetype constraints don't involve type matches");
  }
}

ConstraintSystem::SolutionKind
ConstraintSystem::simplifyConstraint(const Constraint &constraint) {
  switch (constraint.getKind()) {
  case ConstraintKind::Bind:
  case ConstraintKind::Equal:
  case ConstraintKind::EqualRvalue:
  case ConstraintKind::TrivialSubtype:
  case ConstraintKind::Subtype:
  case ConstraintKind::Conversion:
  case ConstraintKind::Construction: {
    // For relational constraints, match up the types.
    bool trivial = true;
    return matchTypes(constraint.getFirstType(), constraint.getSecondType(),
                      getTypeMatchKind(constraint.getKind()), TMF_None,
                      trivial);
  }

  case ConstraintKind::Literal:
    return simplifyLiteralConstraint(constraint.getFirstType(),
                                     constraint.getLiteralKind());

  case ConstraintKind::ValueMember:
  case ConstraintKind::TypeMember:
    return simplifyMemberConstraint(constraint);

  case ConstraintKind::Archetype:
    return simplifyArchetypeConstraint(constraint);
  }
}

void ConstraintSystem::collectConstraintsForTypeVariables(
       SmallVectorImpl<TypeVariableConstraints> &typeVarConstraints) {
  typeVarConstraints.clear();

  // Provide a mapping from type variable to its constraints. The getTVC
  // function ties togeter the SmallVector and the DenseMap.
  llvm::DenseMap<TypeVariableType *, unsigned> typeVarConstraintsMap;
  auto getTVC = [&](TypeVariableType *tv) -> TypeVariableConstraints& {
    tv = getRepresentative(tv);
    unsigned& constraintsIdx = typeVarConstraintsMap[tv];
    if (!constraintsIdx) {
      typeVarConstraints.push_back(TypeVariableConstraints(tv));
      constraintsIdx = typeVarConstraints.size();
    }
    return typeVarConstraints[constraintsIdx - 1];
  };

  // First, collect all of the constraints that relate directly to a
  // type variable.
  llvm::SetVector<TypeVariableType *> referencedTypeVars;
  for (auto constraint : Constraints) {
    auto first = simplifyType(constraint->getFirstType());
    switch (constraint->getClassification()) {
    case ConstraintClassification::Relational:
      // Handle this interesting case below.
      break;

    case ConstraintClassification::Archetype:
    case ConstraintClassification::Literal:
      if (auto firstTV = dyn_cast<TypeVariableType>(first.getPointer())) {
        // Record this constraint on the type variable.
        getTVC(firstTV).KindConstraints.push_back(constraint);
      } else {
        // Simply mark any type variables in the type as referenced.
        first->hasTypeVariable(referencedTypeVars);
      }
      continue;

    case ConstraintClassification::Member:
      // Mark the referenced type variables for both sides.
      first->hasTypeVariable(referencedTypeVars);
      simplifyType(constraint->getSecondType())
        ->hasTypeVariable(referencedTypeVars);
      continue;
    }

    auto second = simplifyType(constraint->getSecondType());

    auto firstTV = first->getAs<TypeVariableType>();
    if (firstTV) {
      // Record the constraint.
      getTVC(firstTV).Above.push_back(std::make_pair(constraint, second));
    } else {
      // Collect any type variables represented in the first type.
      first->hasTypeVariable(referencedTypeVars);
    }

    auto secondTV = second->getAs<TypeVariableType>();
    if (secondTV) {
      // Record the constraint.
      getTVC(secondTV).Below.push_back(std::make_pair(constraint, first));
    } else {
      // Collect any type variables represented in the second type.
      second->hasTypeVariable(referencedTypeVars);
    }

    // If both types are type variables, mark both as referenced.
    if (firstTV && secondTV) {
      referencedTypeVars.insert(firstTV);
      referencedTypeVars.insert(secondTV);
    }
  }

  // Mark any type variables that specify the result of an unresolved overload
  // set as having non-concrete constraints.
  for (auto ovl : UnresolvedOverloadSets) {
    ovl->getBoundType()->hasTypeVariable(referencedTypeVars);
  }

  // Mark any referenced type variables as having non-concrete constraints.
  for (auto tv : referencedTypeVars) {
    tv = getRepresentative(tv);
    auto known = typeVarConstraintsMap.find(tv);
    if (known == typeVarConstraintsMap.end())
      continue;

    typeVarConstraints[known->second-1].HasNonConcreteConstraints = true;
  }
}

bool ConstraintSystem::simplify() {
  bool solvedAny;
  do {
    // Loop through all of the thus-far-unsolved constraints, attempting to
    // simplify each one.
    SmallVector<Constraint *, 16> existingConstraints;
    existingConstraints.swap(Constraints);
    solvedAny = false;
    for (auto constraint : existingConstraints) {
      if (ExternallySolved.count(constraint))
        continue;

      if (addConstraint(constraint)) {
        solvedAny = true;

        if (TC.getLangOpts().DebugConstraintSolver)
          SolvedConstraints.push_back(constraint);        
      }

      if (failedConstraint) {
        return true;
      }
    }

    ExternallySolved.clear();
    ++NumSimplifyIterations;
  } while (solvedAny);

  // We've simplified all of the constraints we can.
  return false;
}

//===--------------------------------------------------------------------===//
// Constraint solving
//===--------------------------------------------------------------------===//
#pragma mark Constraint solving

namespace {
  /// \brief Describes the kind of step taking toward a potential
  /// solution.
  enum class SolutionStepKind : char {
    /// \brief Simplify the system again.
    Simplify,
    /// \brief Resolves an overload set by exploring each option.
    Overload, 
    /// \brief Binds a type variable to a given type.
    TypeBinding,
    /// \brief Explore potential bindings.
    ExploreBindings
  };

  /// \brief A step in the search for a solution, which may resolve a
  /// type variale or resolve an overload.
  class SolutionStep {
    /// \brief The kind of solution step this is.
    SolutionStepKind Kind;

    /// \brief Whether this solution step is "definitive".
    bool IsDefinitive;

    union {
      /// \brief The index of the overload set to be evaluated next.
      unsigned OvlSetIdx;

      /// \brief The type binding to perform. 
      struct {
        /// \brief The type variable to bind.
        TypeVariableType *TypeVar;
        /// \brief The type to which the type variable will be bound.
        TypeBase *BoundType;
      } TypeBinding; 
    };

  public:
    /// \brief Construct a solution step for a simple kind, which needs no
    /// extra data.
    SolutionStep(SolutionStepKind kind) : Kind(kind) {
      assert(kind == SolutionStepKind::ExploreBindings ||
             kind == SolutionStepKind::Simplify);
    }

    /// \brief Construct a solution step that resolves an overload.
    ///
    /// Overload solution steps are always definitive.
    explicit SolutionStep(unsigned ovlSetIdx)
      : Kind(SolutionStepKind::Overload), IsDefinitive(true),
        OvlSetIdx(ovlSetIdx) { }

    /// \brief Construct a solution step that binds a type variable.
    SolutionStep(TypeVariableType *typeVar, Type boundType,
                 bool isDefinitive)
      : Kind(SolutionStepKind::TypeBinding), IsDefinitive(isDefinitive),
        TypeBinding{typeVar, boundType.getPointer()} { }

    /// \brief Determine the kind of solution step to take.
    SolutionStepKind getKind() const { return Kind; }

    /// \brief Determine whether this solution step is definitive.
    ///
    /// A definitive solution step is a solution step that is guaranteed to
    /// be part of the solution, if a solution exists.
    bool isDefinitive() const { return IsDefinitive; }

    /// \brief Retrieve the overload set index.
    ///
    /// Only valid for an overload solution step.
    unsigned getOverloadSetIdx() const {
      assert(getKind() == SolutionStepKind::Overload);
      return OvlSetIdx;
    }

    /// \brief Retrieve the type variable and binding.
    ///
    /// Only valid for a type-binding solution step.
    std::pair<TypeVariableType *, Type> getTypeBinding() const {
      assert(getKind() == SolutionStepKind::TypeBinding);
      return { TypeBinding.TypeVar, Type(TypeBinding.BoundType) };
    }
  };

  /// \brief Describes the kind of child system that we should build to take
  /// the next solution step.
  enum class ChildKind : char {
    /// \brief Don't build a child system at all; we're looking at the parent
    /// system.
    None,
    /// \brief Select a particular overload choice.
    OverloadChoice,
    /// \brief Bind a given type variable to a given type.
    TypeBinding
  };

  /// \brief Describes the child system that we should be building.
  class ChildDescription {
    ChildKind Kind;

    union {
      struct {
        unsigned SetIdx;
        unsigned ChoiceIdx;
      } Overload;

      struct {
        TypeVariableType *TypeVar;
        TypeBase *Binding;
      } TypeBinding;
    };

  public:
    ChildDescription() : Kind(ChildKind::None) { }
    ChildDescription(unsigned ovlSetIdx, unsigned ovlChoiceIdx)
      : Kind(ChildKind::OverloadChoice), Overload{ovlSetIdx, ovlChoiceIdx} { }
    ChildDescription(TypeVariableType *typeVar, Type binding)
      : Kind(ChildKind::TypeBinding), TypeBinding{typeVar, binding.getPointer()}
    {
    }

    ChildKind getKind() const { return Kind; }

    std::pair<unsigned, unsigned> getOverloadChoice() const {
      assert(getKind() == ChildKind::OverloadChoice);
      return {Overload.SetIdx, Overload.ChoiceIdx};
    }

    std::pair<TypeVariableType *, Type> getTypeBinding() const {
      assert(getKind() == ChildKind::TypeBinding);
      return {TypeBinding.TypeVar, Type(TypeBinding.Binding)};
    }
  };

  /// \brief Stack used to store the constraint systems to visit.
  typedef SmallVector<std::pair<ConstraintSystem *, ChildDescription>, 16>
    SolutionStack;
}

/// \brief Resolve an overload set in the given constraint system by
/// producing a set of child constraint systems, each of which picks a specific
/// overload from that set. Those child constraint systems that do not fail
/// during simplification will be added to the stack of constraint systems
/// being considered.
static void resolveOverloadSet(ConstraintSystem &cs,
                               unsigned ovlSetIdx,
                               SolutionStack &stack) {
  OverloadSet *ovl = cs.getUnresolvedOverloadSet(ovlSetIdx);
  auto choices = ovl->getChoices();
  for (unsigned i = 0, n = choices.size(); i != n; ++i) {
    stack.push_back({&cs, ChildDescription(ovlSetIdx, n-i-1)});
  }
}

/// \brief Find the lower and upper bounds on a type variable.
static std::pair<Type, Type>
findTypeVariableBounds(ConstraintSystem &cs, TypeVariableConstraints &tvc,
                       bool &isDefinitive) {
  isDefinitive = !tvc.HasNonConcreteConstraints && tvc.KindConstraints.empty();

  std::pair<Type, Type> bounds;
  for (auto arg : tvc.Below) {
    if (arg.second->hasTypeVariable())
      continue;

    if (bounds.first) {
      // FIXME: Compute the meet of the types. We'll miss
      // potential solutions with the current approach.
      isDefinitive = false;
      ++NumLameNonDefinitive;
      break;
    }

    bounds.first = arg.second;
  }

  for (auto arg : tvc.Above) {
    if (arg.second->hasTypeVariable())
      continue;

    if (bounds.second) {
      // FIXME: Compute the join of the types. We'll miss
      // potential solutions with the current approach.
      isDefinitive = false;
      ++NumLameNonDefinitive;
      break;
    }

    bounds.second = arg.second;
  }

  return bounds;
}

/// \brief Given a set of constraint/type pairs, find the unique concrete type.
///
/// \returns a pair (T, Conflict), where T is the unique concrete type
/// found (or null if there is no such type) and Conflict is a bool
/// indicating whether there was some kind of conflict that makes the
/// type variable we're solving for not have an obvious solution.
static std::pair<Type, bool> 
findUniqueConcreteType(ConstraintSystem &cs,
                       ArrayRef<std::pair<Constraint *, Type>> values) {
  Type result;
  for (const auto &value : values) {
    if (value.second->hasTypeVariable()) {
      return { Type(), true };
    }

    if (result && !result->isEqual(value.second)) {
      return { Type(), true };
    }


    result = value.second;
  }

  return { result, false };
}

/// \brief Given a set of 'kind' constraints, e.g., literal and
/// archetype constraints, find the unique type that satisfies these
/// constraints, if any.
///
/// \returns a pair (T, Conflict), where T is the unique concrete type
/// found (or null if there is no such type) and Conflict is a bool
/// indicating whether there was some kind of conflict that makes the
/// type variable we're solving for not have an obvious solution.
static std::pair<Type, bool> 
findUniqueKindType(ConstraintSystem &cs,
                   ArrayRef<Constraint *> constraints)  {
  auto &tc = cs.getTypeChecker();
  Type result;
  for (auto constraint : constraints) {
    if (constraint->getKind() != ConstraintKind::Literal) {
      return { Type(), true };
    }

    auto literalType = tc.getDefaultLiteralType(constraint->getLiteralKind());
    if (!literalType || (result && !result->isEqual(literalType))) {
      return { Type(), true };
    }

    result = literalType;
  }

  return { result, false };
}

/// \brief Identify type variables for which can we determine a
/// concrete binding, and immediately produce constraints describing
/// that binding.
///
/// Any binding produced is either part of all solutions to this
/// constraint system, or their are no solutions to this constraint
/// system.
///
/// \return true if any type variables were bound.
static bool bindDefinitiveTypeVariables(
              ConstraintSystem &cs,
              SmallVectorImpl<TypeVariableConstraints> &typeVarConstraints) {
  bool foundAny = false;
  for (auto &tvc : typeVarConstraints) {
    if (tvc.HasNonConcreteConstraints)
      continue;

    // Find unique concrete type below.
    auto below = findUniqueConcreteType(cs, tvc.Below);
    if (below.second)
      continue;

    auto type = below.first;
    if (!type) {
      // Find unique concrete type above.
      auto above = findUniqueConcreteType(cs, tvc.Above);
      if (above.second)
        continue;

      type = above.first;
    }

    // Find unique kind type.
    auto kind = findUniqueKindType(cs, tvc.KindConstraints);
    if (kind.second)
      continue;

    if (kind.first) {
      if (type && !type->isEqual(kind.first))
        continue;

      type = kind.first;
    } else if (!type) {
      continue;
    }
    assert(type && "missing type?");

    // We found a type. Use it.
    cs.addConstraint(ConstraintKind::Equal, tvc.TypeVar, type);
    foundAny = true;

    // Directly solve the constraints associated with this type variable.
    for (const auto &below : tvc.Below) {
      cs.addConstraint(below.first, /*isExternallySolved=*/true);
    }
    for (const auto &above : tvc.Above) {
      cs.addConstraint(above.first, /*isExternallySolved=*/true);
    }
    for (auto constraint : tvc.KindConstraints) {
      cs.addConstraint(constraint, /*isExternallySolved=*/true);
    }
  }

  return foundAny;
}

/// \brief Given the direct constraints placed on the type variables within
/// a given constraint system, create potential bindings that resolve the
/// constraints for a type variable.
static Optional<std::tuple<TypeVariableType *, Type, bool>>
resolveTypeVariable(
  ConstraintSystem &cs,
  SmallVectorImpl<TypeVariableConstraints> &typeVarConstraints,
  bool onlyDefinitive)
{

  // Find a type variable with a lower bound, because those are the most
  // interesting.
  TypeVariableConstraints *tvcWithUpper = nullptr;
  Type tvcUpper;
  bool tvcWithUpperIsDefinitive;
  for (auto &tvc : typeVarConstraints) {
    // If we already explored type bindings for this type variable, skip it.
    if (cs.haveExploredTypeVar(tvc.TypeVar))
      continue;

    // If we're only allowed to look at type variables with concrete
    // constraints, and this type variable has non-concrete constraints,
    // skip it.
    if (onlyDefinitive && tvc.HasNonConcreteConstraints)
      continue;

    bool isDefinitive;
    auto bounds = findTypeVariableBounds(cs, tvc, isDefinitive);

    if (onlyDefinitive && !isDefinitive)
      continue;

    // If we have a lower bound, introduce a child constraint system binding
    // this type variable to that lower bound.
    if (bounds.first) {
      return std::make_tuple(tvc.TypeVar, bounds.first, isDefinitive);
    }

    // If there is an upper bound, record that (but don't use it yet).
    if (bounds.second && !tvcWithUpper) {
      tvcWithUpper = &tvc;
      tvcUpper = bounds.second;
      tvcWithUpperIsDefinitive = isDefinitive;
    }
  }

  // If we had an upper bound, introduce a child constraint system binding
  // this type variable to that upper bound.
  // FIXME: This type may be too specific, but we'd really rather not
  // go on a random search for subtypes that might work.
  if (tvcWithUpper) {
    return std::make_tuple(tvcWithUpper->TypeVar, tvcUpper,
                           tvcWithUpperIsDefinitive);
  }

  // If we were only looking for concrete bindings, don't consider literal
  // bindings.
  if (onlyDefinitive)
    return Nothing;

  // If there are any literal constraints, add the default literal values as
  // potential bindings.
  auto &tc = cs.getTypeChecker();
  for (auto &tvc : typeVarConstraints) {
    // If we already explored type bindings for this type variable, skip it.
    if (cs.haveExploredTypeVar(tvc.TypeVar))
      continue;

    for (auto &constraint : tvc.KindConstraints) {
      if (constraint->getClassification() != ConstraintClassification::Literal)
        continue;

      if (auto type = tc.getDefaultLiteralType(constraint->getLiteralKind())) {
        return std::make_tuple(tvc.TypeVar, type,
                               tvc.KindConstraints.size() == 1 &&
                               tvc.Above.empty() && tvc.Below.empty());
      }
    }
  }

  // We're out of ideas.
  return Nothing;
}

/// \brief Determine the next step to take toward finding a solution
/// based on the given constraint system.
///
/// \returns The next solution step, or an empty \c Optional if we've
/// run out of ideas.
static Optional<SolutionStep> getNextSolutionStep(ConstraintSystem &cs) {
  // If this constraint system has a failed constraint, there's nothing to do.
  if (cs.getFailedConstraint())
    return Nothing;

  // If this constraint system resolved overloads in a child system, there is
  // nothing more we can do with it.
  if (cs.hasResolvedOverloadsInChildSystems())
    return Nothing;

  // If there are any potential bindings to explore, do it now.
  if (cs.hasPotentialBindings()) {
    return SolutionStep(SolutionStepKind::ExploreBindings);
  }
  
  SmallVector<TypeVariableConstraints, 16> typeVarConstraints;
  cs.collectConstraintsForTypeVariables(typeVarConstraints);

  // If there are any type variables that we can definitively solve,
  // do so now.
  if (bindDefinitiveTypeVariables(cs, typeVarConstraints)) {
    return SolutionStep(SolutionStepKind::Simplify);
  }

  // If there are any unresolved overload sets, resolve one now.
  // FIXME: This is terrible for performance.
  if (cs.getNumUnresolvedOverloadSets() > 0) {
    // Find the overload set with the minimum number of overloads.
    unsigned minSize = cs.getUnresolvedOverloadSet(0)->getChoices().size();
    unsigned minIdx = 0;
    if (minSize > 2) {
      for (unsigned i = 1, n = cs.getNumUnresolvedOverloadSets(); i < n; ++i) {
        unsigned newSize = cs.getUnresolvedOverloadSet(i)->getChoices().size();
        if (newSize < minSize) {
          minSize = newSize;
          minIdx = i;

          if (minSize == 2)
            break;
        }
      }
    }

    // Resolve the unresolved overload set with the minimum number of overloads.
    return SolutionStep(minIdx);
  }

  // Try to determine a binding for a type variable.
  if (auto binding = resolveTypeVariable(cs, typeVarConstraints,
                                         /*onlyDefinitive=*/false)) {
    using std::get;
    assert(!get<2>(*binding) && "Missing definitive result above?");
    return SolutionStep(get<0>(*binding), get<1>(*binding), get<2>(*binding));
  }

  // We're out of ideas.
  return Nothing;
}

bool ConstraintSystem::solve(SmallVectorImpl<ConstraintSystem *> &viable) {
  assert(!Parent &&"Can only solve at the top level");

  // Simplify this constraint system.
  if (TC.getLangOpts().DebugConstraintSolver) {
    llvm::errs() << "---Simplified constraints---\n";
  }
  bool error = simplify();
  if (TC.getLangOpts().DebugConstraintSolver) {
    dump();
  }
  if (error)
    return true;

  // Seed the constraint system stack with ourselves.
  SolutionStack stack;
  stack.push_back({this, ChildDescription()});

  // While there are still constraint systems to search, do so.
  while (!stack.empty()) {
    auto csAndChildDesc = stack.back();
    auto cs = csAndChildDesc.first;
    auto childDesc = csAndChildDesc.second;
    stack.pop_back();

    // If we're supposed to build a child system, do so now (and simplify it).
    switch (childDesc.getKind()) {
    case ChildKind::None:
      break;

    case ChildKind::OverloadChoice: {
      auto ovl = childDesc.getOverloadChoice();
      if (auto childCS = cs->createDerivedConstraintSystem(ovl.first,
                                                           ovl.second)) {
        cs = childCS;
        break;
      }

      continue;
    }

    case ChildKind::TypeBinding: {
      auto binding = childDesc.getTypeBinding();
      if (auto childCS = cs->createDerivedConstraintSystem(binding.first,
                                                           binding.second)) {
        cs = childCS;
        break;
      }
      continue;
    }
    }
    
    // If there are any children of this system that are still active,
    // then we found a potential solution. There is no need to explore
    // alternatives based on this constraint system.
    if (cs->hasActiveChildren()) {
      cs->finalize();
      continue;
    }

    // If there are no unsolved constraints and no unresolved overload sets,
    // this system is either a solution or it is underconstrained.
    if (cs->Constraints.empty() && cs->UnresolvedOverloadSets.empty()) {
      if (!cs->finalize())
        viable.push_back(cs);
      continue;
    }

    // While we have something interesting to do with this constraint
    // system, do it.
    bool done = false;
    while (auto step = getNextSolutionStep(*cs)) {
      switch (step->getKind()) {
      case SolutionStepKind::Simplify:
        // Simplify the system again.
        if (cs->simplify()) {
          cs->finalize();
          done = true;
        }
        break;

      case SolutionStepKind::Overload: {
        // Resolve the overload set.
        assert(step->isDefinitive() && "Overload solutions are definitive");
        cs->ResolvedOverloadsInChildSystems = true;
        stack.push_back({cs, ChildDescription()});
        resolveOverloadSet(*cs, step->getOverloadSetIdx(), stack);
        done = true;
        break;
      }

      case SolutionStepKind::TypeBinding: {
        // We have a type binding.
        auto binding = step->getTypeBinding();

        // If we have a definitive type binding, apply it immediately;
        // there's no point in creating a child system.
        if (step->isDefinitive()) {
          cs->addConstraint(ConstraintKind::Equal,
                            binding.first, binding.second);
          if (cs->simplify()) {
            cs->finalize();
            done = true;
          }
          break;
        }
        
        // Add this type binding as a potential binding; we'll
        // explore potential bindings below.
        cs->addPotentialBinding(binding.first, binding.second);

        // Fall though to explore this binding.
      }
          
      case SolutionStepKind::ExploreBindings: {
        // If there are no potential bindings, we have nothing else we can
        // explore. Consider this system dead.
        if (cs->PotentialBindings.empty()) {
          cs->markUnsolvable();
          cs->finalize();
          done = true;
          break;
        }

        // Push this constraint system back onto the stack to be reconsidered if
        // none of the child systems created below succeed.
        stack.push_back({cs, ChildDescription()});

        // Create child systems for each of the potential bindings.
        auto potentialBindings = std::move(cs->PotentialBindings);
        cs->PotentialBindings.clear();
        for (auto binding : potentialBindings) {
          if (cs->exploreBinding(binding.first, binding.second)) {
            stack.push_back({cs,
                             ChildDescription(binding.first, binding.second)});
          }
        }
        done = true;
        break;
      }
      }

      if (done)
        break;

      if (cs->Constraints.empty() && cs->UnresolvedOverloadSets.empty()) {
        if (!cs->finalize())
          viable.push_back(cs);
        done = true;
        break;
      }
    }

    if (!done) {
      // If there are no unsolved constraints and no unresolved overload sets,
      // this system is either a solution or it is underconstrained.
      if (cs->Constraints.empty() && cs->UnresolvedOverloadSets.empty()) {
        if (!cs->finalize())
          viable.push_back(cs);
        continue;
      }

      // We couldn't do anything with the system, so it's unsolvable.
      cs->markUnsolvable();
      cs->finalize();
    }
  }


  // If there is more than one viable system, attempt to pick the best solution.
  if (viable.size() > 1) {
    if (auto best = findBestSolution(viable)) {
      if (TC.getLangOpts().DebugConstraintSolver) {
        unsigned idx = 0;
        for (auto cs : viable) {
          SmallVector<TypeVariableType *, 4> freeVariables;
          llvm::errs() << "---Child system #" << ++idx;
          if (cs == best) {
            llvm::errs() << " (best)";
          }
          llvm::errs() << "---\n";
          cs->dump();
        }
      }

      viable.clear();
      viable.push_back(best);
    }
  }

  return viable.size() != 1;
}

//===--------------------------------------------------------------------===//
// Ranking solutions
//===--------------------------------------------------------------------===//
#pragma mark Ranking solutions

bool ConstraintSystem::typeMatchesDefaultLiteralConstraint(TypeVariableType *tv,
                                                           Type type) {
  tv = getRepresentative(tv);

  // FIXME:
  for (auto constraint : Constraints) {
    // We only care about literal constraints...
    if (constraint->getClassification() != ConstraintClassification::Literal)
      continue;

    // on type variables...
    auto constraintTV
      = dyn_cast<TypeVariableType>(constraint->getFirstType().getPointer());
    if (!constraintTV)
      continue;

    // that have the same representative as the type we care about.
    if (getRepresentative(constraintTV) != tv)
      continue;

    // If the type we were given matches the default literal type for this
    // constraint, we found what we're looking for.
    if (type->isEqual(TC.getDefaultLiteralType(constraint->getLiteralKind())))
      return true;
  }

  return false;
}

/// \brief Updates a solution comparison result based on whether a given metric
/// considers one solution set to be "at least as good as" the other.
///
/// \returns true if the caller should return the result immediately, because no
/// additional information would change it.
static bool updateSolutionCompareResult(SolutionCompareResult &result,
                                        bool firstAsGoodAsSecond,
                                        bool secondAsGoodAsFirst) {
  if (firstAsGoodAsSecond && secondAsGoodAsFirst) {
    result = SolutionCompareResult::Incomparable;
    return true;
  }

  if (firstAsGoodAsSecond != secondAsGoodAsFirst) {
    switch (result) {
    case SolutionCompareResult::Incomparable:
      return false;

    case SolutionCompareResult::Identical:
      result = firstAsGoodAsSecond? SolutionCompareResult::Better
                                  : SolutionCompareResult::Worse;
      break;

    case SolutionCompareResult::Better:
      if (secondAsGoodAsFirst)
        result = SolutionCompareResult::Incomparable;
      break;

    case SolutionCompareResult::Worse:
      if (firstAsGoodAsSecond)
        result =  SolutionCompareResult::Incomparable;
      break;
    }
  }

  return false;
}

/// \brief Remove the initializers from any tuple types within the
/// given type.
static Type stripInitializers(TypeChecker &tc, Type origType) {
  return tc.transformType(origType, 
           [&](Type type) -> Type {
             if (auto tupleTy = type->getAs<TupleType>()) {
               SmallVector<TupleTypeElt, 4> fields;
               for (const auto &field : tupleTy->getFields()) {
                 fields.push_back(TupleTypeElt(field.getType(),
                                               field.getName(),
                                               nullptr,
                                               field.getVarargBaseTy()));
                                               
               }
               return TupleType::get(fields, tc.Context);
             }
             return type;
           });
}

SolutionCompareResult ConstraintSystem::compareSolutions(ConstraintSystem &cs1,
                                                         ConstraintSystem &cs2){
  // FIXME: Find least common ancestor. We don't need to look further than
  // than for a decision.

  // Compare the sets of bound type variables in the two systems.
  // FIXME: This would be a good place to stop at the LCA.
  auto compareTypeVariables =
    [&](ConstraintSystem *cs1, ConstraintSystem *cs2) -> SolutionCompareResult {
      SolutionCompareResult result = SolutionCompareResult::Identical;

      // Collect all of the fixed types in CS1.
      llvm::MapVector<TypeVariableType *, Type> cs1FixedTypes;
      for (auto walkCS1 = cs1; walkCS1; walkCS1 = walkCS1->Parent) {
        for (const auto &fixed : walkCS1->TypeVariableInfo)
          if (auto type = fixed.second.dyn_cast<TypeBase *>())
            cs1FixedTypes[fixed.first] = type;
      }

      auto &topSystem = cs1->getTopConstraintSystem();
      for (const auto &fixedTV1 : cs1FixedTypes) {
        if (!fixedTV1.first)
          continue;

        auto boundTV1 = fixedTV1.first;

        // Find the fixed type in the second constraint system.
        Type type2;
        auto known2 = cs2->TypeVariableInfo.find(boundTV1);
        if (known2 != cs2->TypeVariableInfo.end() &&
            known2->second.is<TypeBase *>())
          type2 = known2->second.get<TypeBase *>();
        if (type2) {
          auto type1 = fixedTV1.second;

          // Strip any initializers from tuples in the type; they aren't
          // to be compared.
          type1 = stripInitializers(cs1->getTypeChecker(), type1);
          type2 = stripInitializers(cs1->getTypeChecker(), type2);

          // If the types are equivalent, there's nothing more to do.
          if (type1->isEqual(type2))
            continue;

          // If either of the types still contains type variables, we can't
          // compare them.
          // FIXME: This is really unfortunate. More type variable sharing
          // (when it's sane) would help us do much better here.
          if (type1->hasTypeVariable() || type2->hasTypeVariable())
            return SolutionCompareResult::Incomparable;

          // If one type is a subtype of the other, but not vice-verse,
          // we prefer the system with the more-constrained type.
          bool trivial = false;
          bool type1Better = cs1->matchTypes(type1, type2,
                                             TypeMatchKind::Subtype,
                                             TMF_None, trivial)
                               == SolutionKind::TriviallySolved;
          bool type2Better = cs2->matchTypes(type2, type1,
                                             TypeMatchKind::Subtype,
                                             TMF_None, trivial)
                               == SolutionKind::TriviallySolved;
          if (updateSolutionCompareResult(result, type1Better, type2Better))
            return result;
          if (type1Better || type2Better)
            continue;

          // If the type variable was bound by a literal constraint, and the
          // type it is bound to happens to match the default literal
          // constraint in one system but not the other, we prefer the one
          // that matches the default.
          // Note that the constraint will be available in the parent of
          // the constraint system that assumed a value for the type variable
          // (or, of course, in the original system, since these constraints
          // are generated directly from the expression).
          // FIXME: Make it efficient to find these constraints. This is
          // silly.
          bool defaultLit1
            = topSystem.typeMatchesDefaultLiteralConstraint(boundTV1, type1);
          bool defaultLit2
            = topSystem.typeMatchesDefaultLiteralConstraint(boundTV1, type2);
          if (updateSolutionCompareResult(result, defaultLit1, defaultLit2))
            return result;
          
          continue;
        }
      }

      return result;
    };

  // Compare the type variables bound in the first system to the type variables
  // bound in the second type system.
  SolutionCompareResult result = compareTypeVariables(&cs1, &cs2);
  
  // FIXME: There might be variables bound in the second type system but not
  // the first, but for now we don't really care.

  // FIXME: Compare overload sets.

  switch (result) {
  case SolutionCompareResult::Incomparable:
  case SolutionCompareResult::Better:
  case SolutionCompareResult::Worse:
    return result;

  case SolutionCompareResult::Identical:
    // FIXME: We haven't checked enough to conclude that two solutions are
    // identical.
    return SolutionCompareResult::Incomparable;
  }
}

ConstraintSystem *
ConstraintSystem::findBestSolution(SmallVectorImpl<ConstraintSystem *> &viable){
  if (viable.empty())
    return nullptr;
  if (viable.size() == 1)
    return viable[0];

  // Find a potential best. 
  ConstraintSystem *best = nullptr;
  for (auto cs : viable) {
    if (!best) {
      // Found the first solved system.
      best = cs;
      continue;
    }

    switch (compareSolutions(*cs, *best)) {
    case SolutionCompareResult::Identical:
      // FIXME: Might want to warn about this in debug builds, so we can
      // find a way to eliminate the redundancy in the search space.
    case SolutionCompareResult::Incomparable:
    case SolutionCompareResult::Worse:
      break;

    case SolutionCompareResult::Better:
      best = cs;
      break;
    }
  }

  if (!best)
    return nullptr;

  // Make sure that our current best is better than all of the solved systems.
  for (auto cs : viable) {
    if (best == cs)
      continue;

    switch (compareSolutions(*best, *cs)) {
    case SolutionCompareResult::Identical:
      // FIXME: Might want to warn about this in debug builds, so we can
      // find a way to eliminate the redundancy in the search space.
      // Fall through
    case SolutionCompareResult::Better:
      break;

    case SolutionCompareResult::Incomparable:
    case SolutionCompareResult::Worse:
      return nullptr;
    }
  }

  // FIXME: If we lost our best, we should minimize the set of viable
  // solutions.

  return best;
}

//===--------------------------------------------------------------------===//
// Applying a solution to an expression
//===--------------------------------------------------------------------===//
#pragma mark Applying a solution to an expression.

Expr *ConstraintSystem::convertToType(Expr *expr, Type toType,
                                      bool isAssignment) {
  // FIXME: Temporary hack that uses the existing coercion logic.
  return TC.coerceToType(expr, toType,
                         isAssignment? CoercionKind::Assignment
                                     : CoercionKind::Normal);
}

Expr *ConstraintSystem::convertObjectArgumentToType(Expr *expr, Type toType) {
  // FIXME: Temporary hack that uses the existing coercion logic.
  return TC.coerceObjectArgument(expr, toType);
}

/// \brief Determine whether the given expression refers to an assignment
/// function.
static bool isAssignmentFn(Expr *expr) {
  expr = expr->getSemanticsProvidingExpr();
  if (auto dre = dyn_cast<DeclRefExpr>(expr))
    return dre->getDecl()->getAttrs().isAssignment();
  if (auto dotCall = dyn_cast<DotSyntaxCallExpr>(expr))
    return isAssignmentFn(dotCall->getFn());
  if (auto emr = dyn_cast<ExistentialMemberRefExpr>(expr))
    return emr->getDecl()->getAttrs().isAssignment();
  if (auto amr = dyn_cast<ArchetypeMemberRefExpr>(expr))
    return amr->getDecl()->getAttrs().isAssignment();
  if (auto gmr = dyn_cast<GenericMemberRefExpr>(expr))
    return gmr->getDecl()->getAttrs().isAssignment();
  return false;
}

Expr *ConstraintSystem::applySolution(Expr *expr) {
  class ExprRewriter : public ExprVisitor<ExprRewriter, Expr *> {
    ConstraintSystem &CS;

    Expr *specialize(Expr *expr, PolymorphicFunctionType *polyFn,
                     Type openedType) {
      // Gather the substitutions from archetypes to concrete types, found
      // by identifying all of the type variables in the original type
      auto &tc = CS.getTypeChecker();
      TypeSubstitutionMap substitutions;
      auto type
        = tc.transformType(openedType,
            [&](Type type) -> Type {
              if (auto tv = dyn_cast<TypeVariableType>(type.getPointer())) {
                auto archetype = tv->getImpl().getArchetype();
                auto simplified = CS.simplifyType(tv);
                substitutions[archetype] = simplified;

                return SubstitutedType::get(archetype, simplified,
                                           tc.Context);
              }

              return type;
            });

      // Check that the substitutions we've produced actually work.
      // FIXME: We'd like the type checker to ensure that this always
      // succeeds.
      ConformanceMap conformances;
      if (tc.checkSubstitutions(substitutions, conformances, expr->getLoc(),
                                &substitutions))
        return nullptr;

      // Build the specialization expression.
      auto encodedSubs = tc.encodeSubstitutions(&polyFn->getGenericParams(),
                                                substitutions, conformances,
                                                /*ArchetypesAreOpen=*/false,
                                                /*OnlyInnermostParams=*/true);
      return new (tc.Context) SpecializeExpr(expr, type, encodedSubs);
    }

    /// \brief Build a new member reference with the given base and member.
    Expr *buildMemberRef(Expr *base, SourceLoc dotLoc, ValueDecl *member,
                         SourceLoc memberLoc, Type openedType) {
      // FIXME: Falls back to the old type checker.

      auto &tc = CS.getTypeChecker();
      auto result = tc.buildMemberRefExpr(base, dotLoc, { &member, 1 },
                                          memberLoc);
      if (!result)
        return nullptr;

      result = tc.recheckTypes(result);
      if (!result)
        return nullptr;

      if (auto polyFn = result->getType()->getAs<PolymorphicFunctionType>()) {
        return specialize(result, polyFn, openedType);
      }

      return result;
    }

    /// \brief Build a reference to an operator within a protocol.
    Expr *buildProtocolOperatorRef(ProtocolDecl *proto, ValueDecl *value,
                                   SourceLoc nameLoc, Type openedType) {
      assert(isa<FuncDecl>(value) && "Only functions allowed");
      assert(cast<FuncDecl>(value)->isOperator() && "Only operators allowed");

      // Figure out the base type, which we do by finding the type variable
      // in the open type that corresponds to the 'This' archetype, which
      // we opened.
      // FIXME: This is both inefficient and suspicious. We should probably
      // find a place to cache the type variable, rather than searching for it
      // again.
      Type baseTy;
      auto thisArchetype
        = proto->getThis()->getDeclaredType()->castTo<ArchetypeType>();
      CS.getTypeChecker().transformType(openedType, [&](Type type) -> Type {
        if (auto typeVar = dyn_cast<TypeVariableType>(type.getPointer())) {
          if (typeVar->getImpl().getArchetype() == thisArchetype) {
            baseTy = CS.simplifyType(typeVar);
            return nullptr;
          }
        }
        
        return type;
      });
      assert(baseTy && "Unable to find base type for protocol operator ref");
      // FIXME: Check whether baseTy is an archetype?

      auto &ctx = CS.getASTContext();
      auto base = new (ctx) MetatypeExpr(nullptr, nameLoc,
                                         MetaTypeType::get(baseTy, ctx));
      return buildMemberRef(base, SourceLoc(), value, nameLoc, openedType);
    }

  public:
    ExprRewriter(ConstraintSystem &CS) : CS(CS) { }

    ConstraintSystem &getConstraintSystem() const { return CS; }

    /// \brief Simplify the expression type and return the expression.
    ///
    /// This routine is used for 'simple' expressions that only need their
    /// types simplified, with no further computation.
    Expr *simplifyExprType(Expr *expr) {
      auto toType = CS.simplifyType(expr->getType());
      expr->setType(toType);
      return expr;
    }

    Expr *visitErrorExpr(ErrorExpr *expr) {
      // Do nothing with error expressions.
      return expr;
    }

    Expr *visitLiteralExpr(LiteralExpr *expr) {
      // FIXME: The existing literal coercion code should move here.
      auto type = CS.simplifyType(expr->getType());
      expr->setType(UnstructuredUnresolvedType::get(CS.getASTContext()));
      return CS.convertToType(expr, type);
    }
    
    // FIXME: Add specific entries for the various literal expressions.

    Expr *visitDeclRefExpr(DeclRefExpr *expr) {
      auto fromType = expr->getType();

      if (auto proto
            = dyn_cast<ProtocolDecl>(expr->getDecl()->getDeclContext())) {
        // If this a member of a protocol, build an appropriate operator
        // reference.
        return buildProtocolOperatorRef(proto, expr->getDecl(), expr->getLoc(),
                                        fromType);
      }

      // Set the type of this expression to the actual type of the reference.
      expr->setType(expr->getDecl()->getTypeOfReference());

      // If there is no type variable in the original expression type, we're
      // done.
      if (!fromType->hasTypeVariable())
        return expr;

      // Check whether this is a polymorphic function type, which needs to
      // be specialized.
      auto polyFn = expr->getType()->getAs<PolymorphicFunctionType>();
      if (!polyFn) {
        // No polymorphic function; this a reference to a declaration with a
        // deduced type, such as $0.
        return expr;
      }

      return specialize(expr, polyFn, fromType);
    }

    Expr *visitDotSyntaxBaseIgnoredExpr(DotSyntaxBaseIgnoredExpr *expr) {
      llvm_unreachable("Already type-checked");
    }

    Expr *visitOverloadedDeclRefExpr(OverloadedDeclRefExpr *expr) {
      // Determine the declaration selected for this overloaded reference.
      auto &context = CS.getASTContext();
      auto ovl = CS.getGeneratedOverloadSet(expr);
      assert(ovl && "No overload set associated with decl reference expr?");
      auto selected = CS.getSelectedOverloadFromSet(ovl).getValue();
      auto choice = selected.first;
      auto decl = choice.getDecl();

      if (auto proto = dyn_cast<ProtocolDecl>(decl->getDeclContext())) {
        // If this a member of a protocol, build an appropriate operator
        // reference.
        return buildProtocolOperatorRef(proto, decl, expr->getLoc(),
                                          selected.second);
      }

      // Normal path: build a declaration reference.
      auto result = new (context) DeclRefExpr(decl, expr->getLoc(),
                                              decl->getTypeOfReference());

      // For a polymorphic function type, we have to specialize our reference.
      if (auto polyFn = expr->getType()->getAs<PolymorphicFunctionType>()) {
        assert(false);
        return specialize(result, polyFn, selected.second);
      }

      return result;
    }

    Expr *visitOverloadedMemberRefExpr(OverloadedMemberRefExpr *expr) {
      auto ovl = CS.getGeneratedOverloadSet(expr);
      assert(ovl && "No overload set associated with decl reference expr?");
      auto selected = CS.getSelectedOverloadFromSet(ovl).getValue();
      return buildMemberRef(expr->getBase(), expr->getDotLoc(),
                            selected.first.getDecl(), expr->getMemberLoc(),
                            selected.second);
    }
    
    Expr *visitOverloadedSuperMemberRefExpr(OverloadedSuperMemberRefExpr *e) {
      llvm_unreachable("not implemented");
    }

    Expr *visitOverloadedSuperConstructorRefExpr(
                                         OverloadedSuperConstructorRefExpr *e) {
      llvm_unreachable("Already type-checked");
    }
    
    Expr *visitUnresolvedDeclRefExpr(UnresolvedDeclRefExpr *expr) {
      // FIXME: We should have generated an overload set from this, in which
      // case we can emit a typo-correction error here but recover well.
      return nullptr;
    }

    Expr *visitMemberRefExpr(MemberRefExpr *expr) {
      // FIXME: Falls back to the old type checker.
      return CS.getTypeChecker().recheckTypes(expr);
    }
    
    Expr *visitSuperMemberRefExpr(SuperMemberRefExpr *expr) {
      llvm_unreachable("not implemented");
    }

    Expr *visitExistentialMemberRefExpr(ExistentialMemberRefExpr *expr) {
      llvm_unreachable("Already type-checked");
    }

    Expr *visitArchetypeMemberRefExpr(ArchetypeMemberRefExpr *expr) {
      llvm_unreachable("Already type-checked");
    }

    Expr *visitGenericMemberRefExpr(GenericMemberRefExpr *expr) {
      auto ovl = CS.getGeneratedOverloadSet(expr);
      assert(ovl && "No overload set associated with decl reference expr?");
      auto selected = CS.getSelectedOverloadFromSet(ovl).getValue();
      return buildMemberRef(expr->getBase(), expr->getDotLoc(),
                            selected.first.getDecl(), expr->getNameLoc(),
                            selected.second);
    }

    Expr *visitUnresolvedMemberExpr(UnresolvedMemberExpr *expr) {
      // Dig out the type of the 'oneof', which will either be the result
      // type of this expression (for unit OneOfElements) or the result of
      // the function type of this expression (for non-unit OneOfElements).
      Type oneofTy = CS.simplifyType(expr->getType());
      if (auto funcTy = oneofTy->getAs<FunctionType>())
        oneofTy = funcTy->getResult();

      // The base expression is simply the metatype of a oneof type.
      auto &tc = CS.getTypeChecker();
      auto base = new (tc.Context) MetatypeExpr(nullptr,
                                                expr->getDotLoc(),
                                                MetaTypeType::get(oneofTy,
                                                                  tc.Context));

      // Find the member and build a reference to it.
      // FIXME: Redundant member lookup.
      MemberLookup &lookup = CS.lookupMember(oneofTy, expr->getName());
      assert(lookup.isSuccess() && "Failed lookup?");
      auto member = lookup.Results[0].D;
      auto result = tc.buildMemberRefExpr(base, expr->getDotLoc(),
                                          ArrayRef<ValueDecl *>(&member, 1),
                                          expr->getNameLoc());
      return tc.recheckTypes(result);
    }

    Expr *visitUnresolvedDotExpr(UnresolvedDotExpr *expr) {
      // Determine the declaration selected for this overloaded reference.
      auto ovl = CS.getGeneratedOverloadSet(expr);
      assert(ovl && "No overload set associated with decl reference expr?");
      auto selected = CS.getSelectedOverloadFromSet(ovl).getValue();

      switch (selected.first.getKind()) {
      case OverloadChoiceKind::Decl:
        return buildMemberRef(expr->getBase(), expr->getDotLoc(),
                              selected.first.getDecl(), expr->getNameLoc(),
                              selected.second);

      case OverloadChoiceKind::TupleIndex: {
        auto base = expr->getBase();
        // If the base expression is not an lvalue, make everything inside it
        // materializable.
        if (!base->getType()->is<LValueType>()) {
          base = CS.getTypeChecker().convertToMaterializable(base);
          if (!base)
            return nullptr;
        }

        return new (CS.getASTContext()) TupleElementExpr(
                                          base,
                                          expr->getDotLoc(),
                                          selected.first.getTupleIndex(),
                                          expr->getNameLoc(),
                                          CS.simplifyType(expr->getType()));
      }

      case OverloadChoiceKind::BaseType:
      case OverloadChoiceKind::FunctionReturningBaseType:
      case OverloadChoiceKind::IdentityFunction:
        llvm_unreachable("Nonsensical overload choice");
      }
    }
    
    Expr *visitUnresolvedSuperMemberExpr(UnresolvedSuperMemberExpr *expr) {
      llvm_unreachable("not implemented");
    }

    Expr *visitSequenceExpr(SequenceExpr *expr) {
      llvm_unreachable("Expression wasn't parsed?");
    }

    Expr *visitParenExpr(ParenExpr *expr) {
      return simplifyExprType(expr);
    }

    Expr *visitTupleExpr(TupleExpr *expr) {
      return simplifyExprType(expr);
    }

    Expr *visitSubscriptExpr(SubscriptExpr *expr) {
      // Determine the declaration selected for this subscript operation.
      auto ovl = CS.getGeneratedOverloadSet(expr);
      assert(ovl && "No overload set associated with subscript expr?");
      auto choice = CS.getSelectedOverloadFromSet(ovl).getValue().first;
      auto subscript = cast<SubscriptDecl>(choice.getDecl());

      // FIXME: Falls back to existing type checker to actually populate
      // these nodes.
      auto &tc = CS.getTypeChecker();
      auto baseTy = expr->getBase()->getType()->getRValueType();

      // Subscripting an existential type.
      if (baseTy->isExistentialType()) {
        auto result
          = new (tc.Context) ExistentialSubscriptExpr(expr->getBase(),
                                                      expr->getLBracketLoc(),
                                                      expr->getIndex(),
                                                      expr->getRBracketLoc(),
                                                      subscript);
        return tc.semaSubscriptExpr(result);
      }

      // Subscripting an archetype.
      if (baseTy->is<ArchetypeType>()) {
        auto result
          = new (tc.Context) ArchetypeSubscriptExpr(expr->getBase(),
                                                    expr->getLBracketLoc(),
                                                    expr->getIndex(),
                                                    expr->getRBracketLoc(),
                                                    subscript);
        return tc.semaSubscriptExpr(result);
      }

      // Subscripting a specialization of a generic type.
      if (baseTy->isSpecialized()) {
        auto result
          = new (tc.Context) GenericSubscriptExpr(expr->getBase(),
                                                  expr->getLBracketLoc(),
                                                  expr->getIndex(),
                                                  expr->getRBracketLoc(),
                                                  subscript);
        return tc.semaSubscriptExpr(result);
      }

      // Subscripting a normal, nominal type.
      expr->setDecl(subscript);
      return tc.semaSubscriptExpr(expr);
    }
    
    Expr *visitArrayExpr(ArrayExpr *expr) {
      simplifyExprType(expr);
      Type arrayTy = expr->getType();
      
      ProtocolDecl *arrayProto = CS.TC.getArrayLiteralProtocol();
      assert(arrayProto && "type-checked array literal w/o protocol?!");
      
      /* FIXME: Ideally we'd use the protocol conformance as below, but
       * generic types (an in particular, T[]) can't be checked for conformance
       * yet <rdar://problem/13153805>
       *
       
      // Get the ArrayLiteralConvertible conformance.
      ProtocolConformance *conformance = nullptr;
      if (!CS.TC.conformsToProtocol(arrayTy, arrayProto, &conformance,
                                    expr->getLoc())) {
        // FIXME: This shouldn't happen at all when we solve the system using
        // a proper protocol constraint.
        CS.TC.diagnose(arrayProto->getLoc(), diag::array_protocol_broken);
        return nullptr;
      }
      
      // Find the convertFromArrayLiteral witness.
      FuncDecl *converterDecl;
      for (auto member : arrayProto->getMembers()) {
        auto func = dyn_cast<FuncDecl>(member);
        if (!func) continue;
        
        if (func->getName().str().equals("convertFromArrayLiteral")) {
          converterDecl = cast<FuncDecl>(conformance->Mapping[func]);
          break;
        }
      }
      
      if (!converterDecl) {
        CS.TC.diagnose(arrayProto->getLoc(), diag::array_protocol_broken);
        return nullptr;
      }
      
      if (!converterDecl->isStatic()) {
        CS.TC.diagnose(arrayProto->getLoc(), diag::array_protocol_broken);
        return nullptr;
      }
       
       *
       * For the time being, use a value member constraint to find the
       * appropriate convertFromArrayLiteral call.
       */
      auto ovl = CS.getGeneratedOverloadSet(expr);
      assert(ovl && "No overload set associated with subscript expr?");
      auto choice = CS.getSelectedOverloadFromSet(ovl).getValue().first;
      auto converterDecl = cast<FuncDecl>(choice.getDecl());
      
      // Construct the semantic expr as a convertFromArrayLiteral application.
      ASTContext &C = CS.getASTContext();
      Expr *converterRef = new (C) DeclRefExpr(converterDecl, expr->getLoc(),
                                           converterDecl->getTypeOfReference());
      Expr *typeRef = new (C) MetatypeExpr(nullptr,
                                           expr->getLoc(),
                                           MetaTypeType::get(arrayTy, C));
      
      Expr *semanticExpr = new (C) DotSyntaxCallExpr(converterRef,
                                                     expr->getLoc(),
                                                     typeRef);
      semanticExpr = CS.TC.recheckTypes(semanticExpr);
      
      semanticExpr = new (C) CallExpr(semanticExpr,
                                      expr->getSubExpr());

      semanticExpr = CS.TC.recheckTypes(semanticExpr);
      
      if (!semanticExpr) {
        CS.TC.diagnose(arrayProto->getLoc(), diag::array_protocol_broken);
        return nullptr;
      }
      
      expr->setSemanticExpr(semanticExpr);
      return expr;
    }
    
    Expr *visitSuperSubscriptExpr(SuperSubscriptExpr *expr) {
      llvm_unreachable("super subscript not implemented");
    }

    Expr *visitOverloadedSubscriptExpr(OverloadedSubscriptExpr *expr) {
      llvm_unreachable("Already type-checked");
    }
    
    Expr *visitOverloadedSuperSubscriptExpr(OverloadedSuperSubscriptExpr *expr) {
      llvm_unreachable("not implemented");
    }

    Expr *visitExistentialSubscriptExpr(ExistentialSubscriptExpr *expr) {
      llvm_unreachable("Already type-checked");
    }

    Expr *visitArchetypeSubscriptExpr(ArchetypeSubscriptExpr *expr) {
      llvm_unreachable("Already type-checked");
    }

    Expr *visitGenericSubscriptExpr(GenericSubscriptExpr *expr) {
      llvm_unreachable("Already type-checked");
    }

    Expr *visitTupleElementExpr(TupleElementExpr *expr) {
      llvm_unreachable("Already type-checked");
    }

    void simplifyPatternTypes(Pattern *pattern) {
      switch (pattern->getKind()) {
      case PatternKind::Paren:
        // Parentheses don't affect the type.
        return simplifyPatternTypes(
                                cast<ParenPattern>(pattern)->getSubPattern());

      case PatternKind::Any:
      case PatternKind::Typed:
        return;

      case PatternKind::Named: {
        // Simplify the type of any variables.
        auto var = cast<NamedPattern>(pattern)->getDecl();
        var->overwriteType(CS.simplifyType(var->getType()));
        return;
      }

      case PatternKind::Tuple: {
        auto tuplePat = cast<TuplePattern>(pattern);
        for (auto tupleElt : tuplePat->getFields()) {
          simplifyPatternTypes(tupleElt.getPattern());
        }
        return;
      }
      }
      
      llvm_unreachable("Unhandled pattern kind");
    }

    Expr *visitFuncExpr(FuncExpr *expr) {
      // FIXME: Type-check the function now? Or queue for later?
      simplifyExprType(expr);

      // Coerce the FuncExpr's pattern, in case we resolved something.
      Type input = expr->getType()->castTo<FunctionType>()->getInput();
      if (CS.TC.coerceToType(expr->getArgParamPatterns()[0], input, false))
        return nullptr;
      if (CS.TC.coerceToType(expr->getBodyParamPatterns()[0], input, false))
        return nullptr;

      return expr;
    }

    Expr *visitExplicitClosureExpr(ExplicitClosureExpr *expr) {
      auto type = CS.simplifyType(expr->getType())->castTo<FunctionType>();

      // Count the number of arguments.
      unsigned numInputArgs = 1;
      TupleType *inputTT = type->getInput()->getAs<TupleType>();
      if (inputTT)
        numInputArgs = inputTT->getFields().size();

      // Build up the set of VarDecls (building more if necessary).
      std::vector<VarDecl*> argVars(expr->getParserVarDecls().begin(),
                                    expr->getParserVarDecls().end());
      Pattern *argPat;
      SourceLoc loc = expr->getLoc();
      expr->GenerateVarDecls(numInputArgs, argVars, CS.getASTContext());

      // Build the patterns and update the variable types.
      if (inputTT) {
        std::vector<TuplePatternElt> argElts;
        for (unsigned i = 0; i < numInputArgs; ++i) {
          argVars[i]->overwriteType(inputTT->getElementType(i));
          auto p = new (CS.getASTContext()) NamedPattern(argVars[i]);
          p->setType(inputTT->getElementType(i));
          argElts.emplace_back(p);
        }
        argPat = TuplePattern::create(CS.getASTContext(), loc, argElts, loc);
      } else {
        argVars[0]->overwriteType(type->getInput());
        argPat = new (CS.getASTContext()) NamedPattern(argVars[0]);
      }
      argPat->setType(type->getInput());
      expr->setPattern(argPat);

      // Convert the expression in the body to the result type of the explicit
      // closure.
      auto resultType = type->getResult();
      if (Expr *body = CS.convertToType(expr->getBody(), resultType))
        expr->setBody(body);
      expr->setType(type);

      // Compute the capture list, now that we have analyzed the expression.
      expr->computeCaptures(CS.getASTContext());

      return expr;
    }

    Expr *visitImplicitClosureExpr(ImplicitClosureExpr *expr) {
      llvm_unreachable("Already type-checked");
    }

    Expr *visitModuleExpr(ModuleExpr *expr) { return expr; }

    Expr *visitAddressOfExpr(AddressOfExpr *expr) {
      // Compute the type of the address-of expression.
      // FIXME: Do we really need to compute this, or is this just a hack
      // due to the presence of the 'nonheap' bit?
      auto lv = expr->getSubExpr()->getType()->getAs<LValueType>();
      assert(lv && "Subexpression is not an lvalue?");
      assert(lv->isSettable() &&
             "Solved an address-of constraint with a non-settable lvalue?!");
      
      auto destQuals = lv->getQualifiers() - LValueType::Qual::Implicit;
      expr->setType(LValueType::get(lv->getObjectType(), destQuals,
                                    CS.getASTContext()));
      return expr;
    }

    Expr *visitNewArrayExpr(NewArrayExpr *expr) {
      auto &tc = CS.getTypeChecker();

      // Dig out the element type of the new array expression.
      auto resultType = CS.simplifyType(expr->getType());
      auto elementType = resultType->castTo<BoundGenericType>()
                           ->getGenericArgs()[0];
      expr->setElementType(elementType);

      // Make sure that the result type is a slice type, even if
      // canonicalization mapped it down to Slice<T>.
      auto sliceType = dyn_cast<ArraySliceType>(resultType.getPointer());
      if (!sliceType) {
        sliceType = ArraySliceType::get(elementType, tc.Context);
        sliceType->setImplementationType(
                                       resultType->castTo<BoundGenericType>());
        resultType = sliceType;
      }
      expr->setType(resultType);

      // Find the appropriate injection function.
      Expr* injectionFn = tc.buildArrayInjectionFnRef(sliceType,
                            expr->getBounds()[0].Value->getType(),
                            expr->getNewLoc());
      if (!injectionFn)
        return nullptr;
      expr->setInjectionFunction(injectionFn);
      
      return expr;
    }

    Expr *visitMetatypeExpr(MetatypeExpr *expr) {
      auto &tc = CS.getTypeChecker();

      if (Expr *base = expr->getBase()) {
        base = tc.convertToRValue(base);
        if (!base) return nullptr;
        expr->setBase(base);
        expr->setType(MetaTypeType::get(base->getType(), tc.Context));
      }
      return expr;
    }

    Expr *visitOpaqueValueExpr(OpaqueValueExpr *expr) {
      llvm_unreachable("Already type-checked");
    }
    
    Expr *visitApplyExpr(ApplyExpr *expr) {
      auto &tc = CS.getTypeChecker();

      // The function is always an rvalue.
      auto fn = tc.convertToRValue(expr->getFn());
      if (!fn)
        return nullptr;
      expr->setFn(fn);
      
      // For function application, convert the argument to the input type of
      // the function.
      if (auto fnType = fn->getType()->getAs<FunctionType>()) {
        auto origArg = expr->getArg();
        Expr *arg = nullptr;
        if (isa<DotSyntaxCallExpr>(expr))
          arg = CS.convertObjectArgumentToType(origArg, fnType->getInput());
        else
          arg = CS.convertToType(origArg, fnType->getInput(),
                                 isAssignmentFn(fn));

        if (!arg) {
          tc.diagnose(fn->getLoc(), diag::while_converting_function_argument,
                      fnType->getInput())
            << origArg->getSourceRange();

          return nullptr;
        }

        auto origType = expr->getType();
        expr->setArg(arg);
        expr->setType(fnType->getResult());

        if (auto polyFn = expr->getType()->getAs<PolymorphicFunctionType>()) {
          return specialize(expr, polyFn, origType);
        }

        return expr;
      }

      // FIXME: Implement support for metatypes here.
      return tc.semaApplyExpr(expr);
    }
    
    Expr *visitSuperConstructorRefCallExpr(SuperConstructorRefCallExpr *expr) {
      // Resolve the callee to the constructor declaration selected.
      auto ovl = CS.getGeneratedOverloadSet(expr);
      assert(ovl && "No overload set associated with decl reference expr?");
      auto selected = CS.getSelectedOverloadFromSet(ovl).getValue();
      ConstructorDecl *ctor = cast<ConstructorDecl>(selected.first.getDecl());
      // Reference the initializer, not the allocator.
      DeclRefExpr *ctorRef = new (CS.TC.Context) DeclRefExpr(ctor,
                                                   SourceLoc(),
                                                   ctor->getInitializerType());
      expr->setFn(ctorRef);
      return CS.TC.semaApplyExpr(expr);
    }

    Expr *visitNewReferenceExpr(NewReferenceExpr *expr) {
      auto instanceTy = CS.simplifyType(expr->getType());
      expr->setType(instanceTy);

      // Find the constructor we selected for this expression.
      auto ovl = CS.getGeneratedOverloadSet(expr);
      assert(ovl && "No overload set associated with new reference expr?");
      auto choice = CS.getSelectedOverloadFromSet(ovl).getValue().first;
      auto constructor = cast<ConstructorDecl>(choice.getDecl());

      // Form the constructor call expression.
      auto &tc = CS.getTypeChecker();
      auto classMetaTy = MetaTypeType::get(instanceTy, tc.Context);
      Expr *typeBase = new (tc.Context) MetatypeExpr(nullptr, expr->getLoc(),
                                                     classMetaTy);
      Expr *ctorRef = new (tc.Context) DeclRefExpr(constructor, expr->getLoc(),
                                         constructor->getTypeOfReference());
      ctorRef = new (tc.Context) ConstructorRefCallExpr(ctorRef, typeBase);

      // Extract (or create) the argument;
      Expr *arg = expr->getArg();
      if (!arg) {
        arg = new (tc.Context) TupleExpr(expr->getLoc(),
                                         MutableArrayRef<Expr *>(), nullptr,
                                         expr->getLoc());
        arg->setType(TupleType::getEmpty(tc.Context));
      }

      // FIXME: We need to know what happened to the constraints on the function
      // and on the argument to perform the appropriate conversions,
      // specializations, etc. For now, fall back to the existing type checker.
      ctorRef = tc.recheckTypes(ctorRef);
      expr->setFn(ctorRef);
      expr->setArg(arg);
      return tc.semaApplyExpr(expr);
    }

    // FIXME: Other subclasses of ApplyExpr?

    Expr *visitImplicitConversionExpr(ImplicitConversionExpr *expr) {
      llvm_unreachable("Already type-checked");
    }

    Expr *visitCoerceExpr(CoerceExpr *expr) {
      llvm_unreachable("Already type-checked");
    }

    Expr *visitDowncastExpr(DowncastExpr *expr) {
      llvm_unreachable("Already type-checked");
    }

    Expr *visitSuperToArchetypeExpr(SuperToArchetypeExpr *expr) {
      llvm_unreachable("Already type-checked");
    }
  };

  class ExprWalker : public ASTWalker {
    ExprRewriter &Rewriter;

  public:
    ExprWalker(ExprRewriter &Rewriter) : Rewriter(Rewriter) { }

    virtual bool walkToExprPre(Expr *expr) {
      if (auto closure = dyn_cast<ExplicitClosureExpr>(expr)) {
        // Update the types of the $I variables with their simplified versions.
        // We do this before walking into the body of the closure expression,
        // so that the body will have proper types for its references to the
        // $I variables.
        auto &cs = Rewriter.getConstraintSystem();
        for (auto var : closure->getParserVarDecls())
          var->overwriteType(cs.simplifyType(var->getType()));

        return true;
      }

      // For an array, just walk the expression itself; its children have
      // already been type-checked.
      if (auto newArray = dyn_cast<NewArrayExpr>(expr)) {
        Rewriter.visitNewArrayExpr(newArray);
        return false;
      }

      return true;
    }

    virtual Expr *walkToExprPost(Expr *expr) {
      return Rewriter.visit(expr);
    }

    /// \brief Ignore statements.
    virtual bool walkToStmtPre(Stmt *stmt) { return false; }

    /// \brief Ignore declarations.
    virtual bool walkToDeclPre(Decl *decl) { return false; }
  };

  assert(isSolved() && "Solution is not solved!");

  // FIXME: Disable the constraint-based type checker here, because we depend
  // heavily on the existing type checker.
  llvm::SaveAndRestore<bool> savedUseCS(TC.getLangOpts().UseConstraintSolver,
                                        false);
  ExprRewriter rewriter(*this);
  ExprWalker walker(rewriter);
  return expr->walk(walker);
}

//===--------------------------------------------------------------------===//
// High-level entry points.
//===--------------------------------------------------------------------===//
namespace {
  class PreCheckExpression : public ASTWalker {
    TypeChecker &TC;

  public:
    PreCheckExpression(TypeChecker &tc) : TC(tc) { }

    bool walkToExprPre(Expr *expr) {
      // For FuncExprs, we just want to type-check the patterns as written,
      // but not walk into the body. The body will by type-checked separately.
      if (auto func = dyn_cast<FuncExpr>(expr)) {
        TC.semaFuncExpr(func, /*isFirstPass=*/false,
                        /*allowUnknownTypes=*/true);
        return false;
      }

      return true;
    }

    Expr *walkToExprPost(Expr *expr) {
      // Fold sequence expressions.
      if (auto seqExpr = dyn_cast<SequenceExpr>(expr)) {
        return TC.foldSequence(seqExpr);
      }

      // Type check the type in an array new expression.
      if (auto newArray = dyn_cast<NewArrayExpr>(expr)) {
        llvm::SaveAndRestore<bool> savedUseCS(
                                     TC.getLangOpts().UseConstraintSolver,
                                     false);

        // FIXME: Check that the element type has a default constructor.
        
        if (TC.validateType(newArray->getElementTypeLoc(),
                            /*isFirstPass=*/false))
          return nullptr;

        // Check array bounds. They are subproblems that don't interact with
        // the surrounding expression context.
        for (unsigned i = newArray->getBounds().size(); i != 1; --i) {
          auto &bound = newArray->getBounds()[i-1];
          if (!bound.Value)
            continue;

          // All inner bounds must be constant.
          if (TC.typeCheckArrayBound(bound.Value, /*requireConstant=*/true))
            return nullptr;
        }

        // The outermost bound does not need to be constant.
        auto boundExpr = newArray->getBounds()[0].Value;
        if (TC.typeCheckExpression(boundExpr))
          return nullptr;
        newArray->getBounds()[0].Value = boundExpr;
        if (TC.typeCheckArrayBound(newArray->getBounds()[0].Value,
                                   /*requireConstant=*/false))
          return nullptr;

        return expr;
      }

      // Type check the type in a new expression.
      if (auto newRef = dyn_cast<NewReferenceExpr>(expr)) {
        if (TC.validateType(newRef->getElementTypeLoc(),
                            /*isFirstPass=*/false))
          return nullptr;

        return expr;
      }

      return expr;
    }

    bool walkToStmtPre(Stmt *stmt) {
      // Never walk into statements.
      return false;
    }
  };
}

#pragma mark High-level entry points
Expr *TypeChecker::typeCheckExpressionConstraints(Expr *expr, Type convertType){
  // First, pre-check the expression, validating any types that occur in the
  // expression and folding sequence expressions.
  expr = expr->walk(PreCheckExpression(*this));
  if (!expr)
    return nullptr;

  llvm::raw_ostream &log = llvm::errs();

  // Construct a constraint system from this expression.
  ConstraintSystem cs(*this);
  if (cs.generateConstraints(expr))
    return nullptr;

  // If there is a type that we're expected to convert to, add the conversion
  // constraint.
  if (convertType) {
    cs.addConstraint(ConstraintKind::Conversion, expr->getType(), convertType);
  }

  if (getLangOpts().DebugConstraintSolver) {
    log << "---Initial constraints for the given expression---\n";
    expr->print(log);
    log << "\n";
    cs.dump();
  }

  // Attempt to solve the constraint system.
  SmallVector<ConstraintSystem *, 4> viable;
  if (cs.solve(viable)) {
    if (getLangOpts().DebugConstraintSolver) {
      log << "---Solved constraints---\n";
      cs.dump();

      if (!viable.empty()) {
        unsigned idx = 0;
        for (auto cs : viable) {
          log << "---Child system #" << ++idx << "---\n";
          cs->dump();
        }
      }

      if (viable.size() == 0)
        log << "No solution found.\n";
      else if (viable.size() == 1)
        log << "Unique solution found.\n";
      else {
        log << "Found " << viable.size() << " potential solutions.\n";
      }
    }

    // FIXME: Crappy diagnostic.
    diagnose(expr->getLoc(), diag::constraint_type_check_fail)
      << expr->getSourceRange();

    return nullptr;
  }

  auto solution = viable[0];
  if (getLangOpts().DebugConstraintSolver) {
    log << "---Solution---\n";
    solution->dump();
  }

  // Inject the permanent type bindings from this solution.
  ReinstateTypeVariableBindingsRAII reinstateBindings(*solution);

  // Apply the solution to the expression.
  auto result = solution->applySolution(expr);
  if (!result) {
    // Failure already diagnosed, above, as part of applying the solution.
   return nullptr;
  }

  // If we're supposed to convert the expression to some particular type,
  // do so now.
  if (convertType) {
    result = solution->convertToType(result, convertType);
    if (!result) {
      return nullptr;
    }
  }

  if (getLangOpts().DebugConstraintSolver) {
    log << "---Type-checked expression---\n";
    result->dump();
  }

  return result;
}

/// \brief Compute the rvalue type of the given expression, which is the
/// destination of an assignment statement.
static Type computeAssignDestType(ConstraintSystem &cs, Expr *dest,
                                  SourceLoc equalLoc) {
  if (TupleExpr *TE = dyn_cast<TupleExpr>(dest)) {
    auto &ctx = cs.getASTContext();
    SmallVector<TupleTypeElt, 4> destTupleTypes;
    for (unsigned i = 0; i != TE->getNumElements(); ++i) {
      Expr *subExpr = TE->getElement(i);
      Type elemTy = computeAssignDestType(cs, subExpr, equalLoc);
      if (!elemTy)
        return Type();
      destTupleTypes.push_back(TupleTypeElt(elemTy, TE->getElementName(i)));
    }

    return TupleType::get(destTupleTypes, ctx);
  }

  Type destTy = cs.simplifyType(dest->getType());
  if (LValueType *destLV = destTy->getAs<LValueType>()) {
    // If the destination is a settable lvalue, we're good; get its object type.
    if (!destLV->isSettable()) {
      cs.getTypeChecker().diagnose(equalLoc, diag::assignment_lhs_not_settable)
        << dest->getSourceRange();
    }
    destTy = destLV->getObjectType();
  } else if (auto typeVar = dyn_cast<TypeVariableType>(destTy.getPointer())) {
    // The destination is a type variable. This type variable must be an
    // lvalue type, which we enforce via a subtyping relationship with
    // [byref(implicit, settable)] T, where T is a fresh type variable that
    // will be the object type of this particular expression type.
    auto objectTv = cs.createTypeVariable(dest);
    auto refTv = LValueType::get(objectTv,
                                 LValueType::Qual::Implicit|
                                 LValueType::Qual::NonHeap,
                                 cs.getASTContext());
    cs.addConstraint(ConstraintKind::Subtype, typeVar, refTv);
    destTy = objectTv;
  } else {
    if (!destTy->is<ErrorType>())
      cs.getTypeChecker().diagnose(equalLoc, diag::assignment_lhs_not_lvalue)
        << dest->getSourceRange();

    return Type();
  }
  
  return destTy;
}

std::pair<Expr *, Expr *>
TypeChecker::typeCheckAssignmentConstraints(Expr *dest,
                                            SourceLoc equalLoc,
                                            Expr *src) {
  // First, pre-check the destination and source, validating any types that
  // occur in the expression and folding sequence expressions.

  // Pre-check the destination.
  dest = dest->walk(PreCheckExpression(*this));
  if (!dest)
    return std::make_pair(nullptr, nullptr);

  // Pre-check the source.
  src = src->walk(PreCheckExpression(*this));
  if (!src)
    return { nullptr, nullptr };

  llvm::raw_ostream &log = llvm::errs();

  // Construct a constraint system from the destination and source.
  ConstraintSystem cs(*this);
  if (cs.generateConstraints(dest) || cs.generateConstraints(src))
    return { nullptr, nullptr };

  // Compute the type to which the source must be converted to allow assignment
  // to the destination.
  auto destTy = computeAssignDestType(cs, dest, equalLoc);
  if (!destTy)
    return { nullptr, nullptr };

  // The source must be convertible to the destination.
  cs.addConstraint(ConstraintKind::Conversion, src->getType(), destTy);

  if (getLangOpts().DebugConstraintSolver) {
    log << "---Initial constraints for the given assignment---\n";
    log << "Destination expression:\n";
    dest->print(log);
    log << "\n";
    log << "Source expression:\n";
    src->print(log);
    log << "\n";
    cs.dump();
  }

  // Attempt to solve the constraint system.
  SmallVector<ConstraintSystem *, 4> viable;
  if (cs.solve(viable)) {
    if (getLangOpts().DebugConstraintSolver) {
      log << "---Solved constraints---\n";
      cs.dump();

      if (!viable.empty()) {
        unsigned idx = 0;
        for (auto cs : viable) {
          SmallVector<TypeVariableType *, 4> freeVariables;
          log << "---Child system #" << ++idx << "---\n";
          cs->dump();
        }
      }

      if (viable.size() == 0)
        log << "No solution found.\n";
      else if (viable.size() == 1)
        log << "Unique solution found.\n";
      else {
        log << "Found " << viable.size() << " potential solutions.\n";
      }
    }

    // FIXME: Crappy diagnostic.
    diagnose(equalLoc, diag::constraint_assign_type_check_fail)
      << dest->getSourceRange() << src->getSourceRange();

    return { nullptr, nullptr };
  }

  auto solution = viable[0];
  if (getLangOpts().DebugConstraintSolver) {
    log << "---Solution---\n";
    solution->dump();
  }

  // Inject the permanent type bindings from this solution.
  ReinstateTypeVariableBindingsRAII reinstateBindings(*solution);

  // Apply the solution to the destination.
  dest = solution->applySolution(dest);
  if (!dest) {
    // Failure already diagnosed, above, as part of applying the solution.
    return { nullptr, nullptr };
  }

  // Apply the solution to the source.
  src = solution->applySolution(src);
  if (!src) {
    // Failure already diagnosed, above, as part of applying the solution.
    return { nullptr, nullptr };
  }

  // Convert the source to the simplified destination type.
  src = solution->convertToType(src, solution->simplifyType(destTy));
  if (!src) {
    // Failure already diagnosed, above, as part of applying the solution.
    return { nullptr, nullptr };
  }

  if (getLangOpts().DebugConstraintSolver) {
    log << "---Type-checked expressions---\n";
    log << "Destination expression:\n";
    dest->print(log);
    log << "\n";
    log << "Source expression:\n";
    src->print(log);
    log << "\n";
  }

  return { dest, src };
}

//===--------------------------------------------------------------------===//
// Debugging
//===--------------------------------------------------------------------===//
#pragma mark Debugging

void ConstraintSystem::dump() {
  llvm::raw_ostream &out = llvm::errs();

  if (Parent) {
    SmallVector<ConstraintSystem *, 4> path;;
    for (auto cs = this; cs->Parent; cs = cs->Parent) {
      path.push_back(cs);
    }

    out << "Assumptions:\n";
    for (auto p = path.rbegin(), pe = path.rend(); p != pe; ++p) {
      auto cs = *p;
      out.indent(2);

      // If we assumed a type variable binding, report that.
      if (cs->assumedTypeVar) {
        out << "  assuming " << cs->assumedTypeVar->getString() << " == "
            << cs->getFixedType(cs->assumedTypeVar) << "\n";
        continue;
      }

      // Otherwise, report the resolved overloads.
      assert(!cs->ResolvedOverloads.empty());
      for (auto ovl : cs->ResolvedOverloads) {
        auto &choice = ovl.first->getChoices()[ovl.second.first];
        out << "  selected overload set #" << ovl.first->getID()
            << " choice #" << ovl.second.first << " for ";
        switch (choice.getKind()) {
        case OverloadChoiceKind::Decl:
          if (choice.getBaseType())
            out << choice.getBaseType()->getString() << ".";
          out << choice.getDecl()->getName().str() << ": "
            << ovl.first->getBoundType()->getString() << " == "
            << ovl.second.second->getString() << "\n";
          break;

        case OverloadChoiceKind::BaseType:
          out << "base type " << choice.getBaseType()->getString() << "\n";
          break;

        case OverloadChoiceKind::FunctionReturningBaseType:
          out << "function returning base type "
              << choice.getBaseType()->getString() << "\n";
          break;
        case OverloadChoiceKind::IdentityFunction:
          out << "identity " << choice.getBaseType()->getString() << " -> "
              << choice.getBaseType()->getString() << "\n";
          break;
        case OverloadChoiceKind::TupleIndex:
          out << "tuple " << choice.getBaseType()->getString() << " index "
              << choice.getTupleIndex() << "\n";
          break;
        }
      }
    }
    out << "\n";
  }

  out << "Type Variables:\n";
  for (auto cs = this; cs; cs = cs->Parent) {
    for (auto tv : cs->TypeVariables) {
      out.indent(2);
      tv->getImpl().print(out);
      auto rep = getRepresentative(tv);
      if (rep == tv) {
        if (auto fixed = getFixedType(tv)) {
          out << " as ";
          fixed->print(out);
        }
      } else {
        out << " equivalent to ";
        rep->print(out);
      }
      out << "\n";
    }
  }
  
  out << "\nUnsolved Constraints:\n";
  for (auto constraint : Constraints) {
    out.indent(2);
    constraint->print(out);
    out << "\n";
  }

  out << "\nSolved Constraints:\n";
  for (auto constraint : SolvedConstraints) {
    out.indent(2);
    constraint->print(out);
    out << "\n";
  }

  if (!UnresolvedOverloadSets.empty()) {
    out << "\nUnresolved overload sets:\n";
    for (auto overload : UnresolvedOverloadSets) {
      out.indent(2) << "set #" << overload->getID() << " binds "
        << overload->getBoundType()->getString() << ":\n";
      for (auto choice : overload->getChoices()) {
        out.indent(4);
        switch (choice.getKind()) {
        case OverloadChoiceKind::Decl:
          if (choice.getBaseType())
            out << choice.getBaseType()->getString() << ".";
          out << choice.getDecl()->getName().str() << ": ";
          out << choice.getDecl()->getTypeOfReference()->getString() << '\n';
          break;

        case OverloadChoiceKind::BaseType:
          out << "base type " << choice.getBaseType()->getString() << "\n";
          break;

        case OverloadChoiceKind::FunctionReturningBaseType:
          out << "function returning base type "
            << choice.getBaseType()->getString() << "\n";
          break;
        case OverloadChoiceKind::IdentityFunction:
          out << "identity " << choice.getBaseType()->getString() << " -> "
            << choice.getBaseType()->getString() << "\n";
          break;
        case OverloadChoiceKind::TupleIndex:
          out << "tuple " << choice.getBaseType()->getString() << " index "
            << choice.getTupleIndex() << "\n";
          break;
        }
      }
    }
  }

  if (failedConstraint) {
    out << "\nFailed constraint:\n";
    out.indent(2);
    failedConstraint->print(out);
    out << "\n";
  }

  if (isSolved()) {
    out << "SOLVED (completely)\n";
  } else {
    out << "UNSOLVED\n";
  }
  
}

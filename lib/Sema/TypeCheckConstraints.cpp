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
#include "llvm/ADT/DenseMap.h"
#include "llvm/ADT/SmallPtrSet.h"
#include "llvm/ADT/SmallVector.h"
#include "llvm/Support/Allocator.h"
#include "llvm/Support/raw_ostream.h"
#include <iterator>
#include <memory>
#include <utility>

using namespace swift;
using llvm::SmallPtrSet;

namespace {
  class ConstraintSystem;
}

void *operator new(size_t bytes, ConstraintSystem& cs,
                   size_t alignment = 8);

//===--------------------------------------------------------------------===//
// Type variable implementation.
//===--------------------------------------------------------------------===//
#pragma mark Type variable implementation

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

  /// \brief The expression that will be assigned this particular type, if
  /// any.
  Expr *TheExpr;

  /// \brief The archetype that this type variable describes.
  ArchetypeType *Archetype;

  /// \brief The parent of this type variable if it has been unified with
  /// another type variable, or null if it is the representative of its
  /// equivalence class.
  TypeVariableType *Parent;

public:
  explicit Implementation(unsigned ID)
    : ID(ID), TheExpr(nullptr), Archetype(nullptr), Parent(nullptr) { }

  explicit Implementation(unsigned ID, Expr *TheExpr)
    : ID(ID), TheExpr(TheExpr), Archetype(nullptr), Parent(nullptr) { }

  explicit Implementation(unsigned ID, ArchetypeType *Archetype)
    : ID(ID), TheExpr(nullptr), Archetype(Archetype), Parent(nullptr) { }

  /// \brief Retrieve the unique ID corresponding to this type variable.
  unsigned getID() const { return ID; }
  
  /// \brief Retrieve the expression that will be assigned this particular
  /// type, if any.
  Expr *getExpr() const { return TheExpr; }

  /// \brief Retrieve the archetype that this type variable replaced.
  ArchetypeType *getArchetype() const { return Archetype; }

  /// \brief Retrieve the type variable associated with this implementation.
  TypeVariableType *getTypeVariable() {
    return reinterpret_cast<TypeVariableType *>(this) - 1;
  }

  /// \brief Return the representative of the set of equivalent type variables.
  /// Two variables
  ///
  /// This routine performs path compression, so that future representive
  /// queries are more efficient.
  TypeVariableType *getRepresentative() {
    if (!Parent)
      return getTypeVariable();

    auto rep = Parent->getImpl().getRepresentative();
    Parent = rep;
    return rep;
  }

  /// \brief Set the representative for this equivalence set of type variables,
  /// merge the equivalence sets of the two type variables.
  ///
  /// Note that both \c this and \c newRep must be the representatives of their
  /// equivalence classes, and must be distinct.
  void mergeEquivalenceClasses(TypeVariableType *newRep) {
    assert(getTypeVariable() == getRepresentative()
           && "this is not the representative");
    assert(newRep == newRep->getImpl().getRepresentative()
           && "newRep is not the representative");
    assert(getTypeVariable() != newRep && "cannot merge type with itself");
    Parent = newRep;
  }

  void print(llvm::raw_ostream &Out) {
    Out << "$T" << ID;

    // If this type variable is not the representative for its equivalence
    // class, just print the representative and we're done.
    auto rep = getRepresentative();
    if (rep != getTypeVariable()) {
      Out << " equivalent to $T" << rep->getImpl().ID;
      return;
    }
  }
};

template<typename ...Args>
TypeVariableType *TypeVariableType::getNew(ASTContext &C, Args &&...args) {
  // FIXME: Use the constraint-system's local allocator!

  // Allocate memory
  void *mem = C.Allocate(sizeof(TypeVariableType) + sizeof(Implementation),
                         alignof(TypeVariableType));

  // Construct the type variable.
  auto *result = ::new (mem) TypeVariableType(C);

  // Construct the implementation object.
  new (result+1) TypeVariableType::Implementation(std::forward<Args>(args)...);
  
  return result;
}

void TypeVariableType::print(raw_ostream &OS) const {
  OS << "$T" << getImpl().getID();
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
    ///
    /// FIXME: This kind of constraint will eventually go away, when we can
    /// express the notion of 'converts to a particular literal kind' as a
    /// protocol, rather than a convention.
    Literal,
    /// \brief The first type has a member with the given name, and the
    /// type of that member, when referenced as a value, is the second type.
    ValueMember,
    /// \brief The first type has a type member with the given name, and the
    /// type of that member, when referenced as a type, is the second type.
    TypeMember
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
    Member
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

    /// \brief Constraints are always allocated within a given constraint
    /// system.
    void *operator new(size_t) = delete;

  public:
    Constraint(ConstraintKind Kind, Type First, Type Second, Identifier Member)
      : Kind(Kind), First(First), Second(Second), Member(Member)
    {
      switch (Kind) {
      case ConstraintKind::Bind:
      case ConstraintKind::Equal:
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
      }
    }

    Constraint(Type type, LiteralKind literal)
      : Kind(ConstraintKind::Literal), First(type), Literal(literal) { }

    // FIXME: Need some context information here.

    /// \brief Determine the kind of constraint.
    ConstraintKind getKind() const { return Kind; }

    /// \brief Determine the classification of this constraint, providing
    /// a broader categorization than \c getKind().
    ConstraintClassification getClassification() const {
      switch (Kind) {
      case ConstraintKind::Bind:
      case ConstraintKind::Equal:
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

    void print(llvm::raw_ostream &Out) {
      First->print(Out);
      switch (Kind) {
      case ConstraintKind::Bind: Out << " := "; break;
      case ConstraintKind::Equal: Out << " == "; break;
      case ConstraintKind::TrivialSubtype: Out << " <t "; break;
      case ConstraintKind::Subtype: Out << " < "; break;
      case ConstraintKind::Conversion: Out << " << "; break;
      case ConstraintKind::Construction: Out << " <<C "; break;
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
  enum class OverloadChoiceKind : char {
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
    IdentityFunction
  };

  /// \brief Describes a particular choice within an overload set.
  ///
  /// 
  class OverloadChoice {
    /// \brief The base type to be used when referencing the declaration.
    Type Base;

    /// \brief The kind of overload choice we're working with, along with
    /// the declaration being selected (for an overload choice that picks a
    /// declaration).
    llvm::PointerIntPair<ValueDecl *, 2> ValueAndKind;

    /// \brief Overload choices are always allocated within a given constraint
    /// system.
    void *operator new(size_t) = delete;

  public:
    OverloadChoice(Type base, ValueDecl *value)
      : Base(base), ValueAndKind(value, (unsigned)OverloadChoiceKind::Decl) { }

    OverloadChoice(Type base, OverloadChoiceKind kind)
      : Base(base), ValueAndKind(nullptr, (unsigned)kind)
    {
      assert(base && "Must have a base type for overload choice");
      assert(kind != OverloadChoiceKind::Decl && "wrong constructor for decl");
    }

    /// \brief Retrieve the base type used to refer to the declaration.
    Type getBaseType() const { return Base; }

    /// \brief Determines the kind of overload choice this is.
    OverloadChoiceKind getKind() const {
      return (OverloadChoiceKind)ValueAndKind.getInt();
    }

    /// \brief Retrieve the declaraton that corresponds to this overload choice.
    ValueDecl *getDecl() const {
      assert(getKind() == OverloadChoiceKind::Decl && "Not a declaration");
      return ValueAndKind.getPointer();
    }
  };

  /// \brief An overload set, which is a set of overloading choices from which
  /// only one can be selected.
  class OverloadSet {
    /// \brief ID number that uniquely identifies this overload set.
    unsigned ID;

    /// \brief The expression or constraint that introduced the overload choice.
    llvm::PointerUnion<Expr *, const Constraint *> ExprOrConstraint;

    /// \brief The number of choices in the overload set.
    unsigned NumChoices;

    /// \brief The type bound by this overload set.
    Type BoundType;
    
    /// \brief Overload sets are always allocated within a given constraint
    /// system.
    void *operator new(size_t) = delete;

    OverloadSet(unsigned ID,
                llvm::PointerUnion<Expr *, const Constraint *> from,
                Type boundType,
                ArrayRef<OverloadChoice> choices)
      : ID(ID), ExprOrConstraint(from), NumChoices(choices.size()),
        BoundType(boundType)
    {
      memmove(this+1, choices.data(), sizeof(OverloadChoice)*choices.size());
    }

  public:
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
    TypeVariableConstraints(TypeVariableType *typeVar) : TypeVar(typeVar) {}

    /// \brief The representative type variable.
    TypeVariableType *TypeVar;

    /// \brief The set of constraints directly applicable to the type
    /// variable T.
    SmallVector<Constraint *, 4> Constraints;
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

  /// \brief Describes a system of constraints on type variables, the
  /// solution of which assigns concrete types to each of the type variables.
  /// Constraint systems are typically generated given an (untyped) expression.
  class ConstraintSystem {
    TypeChecker &TC;
    ConstraintSystem *Parent = nullptr;
    Constraint *failedConstraint = nullptr;

    // ---Track the state space we've already explored---
    /// \brief The number of child systems that are still active, i.e., haven't
    /// been found to be unsolvable.
    unsigned NumActiveChildren = 0;

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

    // ---Only valid in the top-level constraint system---
    llvm::BumpPtrAllocator Allocator;
    unsigned TypeCounter = 0;
    unsigned OverloadSetCounter = 0;
    
    // ---Only valid in the top-level constraint system---
    OverloadSet *InOverloadSet = nullptr;
    unsigned OverloadChoiceIdx;

    /// \brief The type variable for which we are assuming a given particular
    /// type.
    TypeVariableType *assumedTypeVar = nullptr;

    // Valid everywhere
    SmallVector<TypeVariableType *, 16> TypeVariables;
    SmallVector<Constraint *, 16> Constraints;
    SmallVector<OverloadSet *, 4> UnresolvedOverloadSets;
    SmallVector<std::unique_ptr<ConstraintSystem>, 2> Children;
    llvm::DenseMap<TypeVariableType *, Type> FixedTypes;

    unsigned assignTypeVariableID() {
      return getTopConstraintSystem().TypeCounter++;
    }

    unsigned assignOverloadSetID() {
      return getTopConstraintSystem().OverloadSetCounter++;
    }
    friend class OverloadSet;

  public:
    ConstraintSystem(TypeChecker &TC)
      : TC(TC) {}

    /// \brief Creates a child constraint system, inheriting the constraints
    /// of its parent.
    ///
    /// \param parent The parent constraint system.
    ///
    /// \param overloadChoiceIdx The index into the parent's first unresolved
    /// overload set, which indices which overload choice this child system
    /// explores.
    ConstraintSystem(ConstraintSystem *parent, unsigned overloadChoiceIdx)
      : TC(parent->TC), Parent(parent),
        InOverloadSet(parent->UnresolvedOverloadSets.front()),
        OverloadChoiceIdx(overloadChoiceIdx),
        // FIXME: Lazily copy from parent system.
        Constraints(parent->Constraints),
        UnresolvedOverloadSets(parent->UnresolvedOverloadSets.begin()+1,
                               parent->UnresolvedOverloadSets.end())
    {
    }

    /// \brief Creates a child constraint system, inheriting the constraints
    /// of its parent.
    ///
    /// \param parent The parent constraint system.
    ///
    /// \param typeVar The type variable whose binding we will be exploring
    /// within this child system.
    ConstraintSystem(ConstraintSystem *parent, TypeVariableType *typeVar)
      : TC(parent->TC), Parent(parent), assumedTypeVar(typeVar),
        // FIXME: Lazily copy from parent system.
        Constraints(parent->Constraints),
        UnresolvedOverloadSets(parent->UnresolvedOverloadSets)
    {
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

    /// \brief Determine whether this constraint system has any active
    /// child constraint systems.
    bool hasActiveChildren() const { return NumActiveChildren > 0; }

    /// \brief Create a new constraint system that is derived from this
    /// constraint system, referencing the rules of the parent system but
    /// also introducing its own (likely dependent) constraints.
    template<typename ...Args>
    ConstraintSystem *createDerivedConstraintSystem(Args &&...args){
      auto result = new ConstraintSystem(this, std::forward<Args>(args)...);
      Children.push_back(std::unique_ptr<ConstraintSystem>(result));
      ++NumActiveChildren;
      return result;
    }

  private:
    /// \brief Indicates that the given child constraint system is inactive
    /// (i.e., unsolvable).
    void markChildInactive(ConstraintSystem *childCS);

    /// \brief Indicates that this constraint system is unsolvable.
    void markUnsolvable() {
      if (Parent)
        Parent->markChildInactive(this);
    }

  public:
    /// \brief Create a new type variable.
    template<typename ...Args>
    TypeVariableType *createTypeVariable(Args &&...args) {
      auto tv= TypeVariableType::getNew(TC.Context, assignTypeVariableID(),
                                        std::forward<Args>(args)...);
      TypeVariables.push_back(tv);
      return tv;
    }

    /// \brief Add a constraint to the constraint system.
    void addConstraint(ConstraintKind kind, Type first, Type second) {
      assert(first && "Missing first type");
      assert(second && "Missing first type");
      Constraints.push_back(new (*this) Constraint(kind, first, second,
                                                   Identifier()));
    }

    ///\ brief Add a literal constraint to the constraint system.
    void addLiteralConstraint(Type type, LiteralKind kind) {
      assert(type && "missing type for literal constraint");
      Constraints.push_back(new (*this) Constraint(type, kind));
    }

    /// \brief Add a value member constraint to the constraint system.
    void addValueMemberConstraint(Type baseTy, Identifier name, Type memberTy) {
      assert(baseTy);
      assert(memberTy);
      assert(!name.empty());
      Constraints.push_back(new (*this) Constraint(ConstraintKind::ValueMember,
                                                   baseTy, memberTy, name));
    }

    /// \brief Add a type member constraint to the constraint system.
    void addTypeMemberConstraint(Type baseTy, Identifier name, Type memberTy) {
      assert(baseTy);
      assert(memberTy);
      assert(!name.empty());
      Constraints.push_back(new (*this) Constraint(ConstraintKind::ValueMember,
                                                   baseTy, memberTy, name));
    }

    /// \brief Retrieve the fixed type corresponding to the given type variable,
    /// or a null type if there is no fixed type.
    Type getFixedType(TypeVariableType *typeVar) {
      typeVar = typeVar->getImpl().getRepresentative();
      auto known = FixedTypes.find(typeVar);
      if (known == FixedTypes.end()) {
        if (Parent)
          return Parent->getFixedType(typeVar);

        return Type();
      }

      return known->second;
    }

    /// \brief Assign a fixed type to the given type variable.
    void assignFixedType(TypeVariableType *typeVar, Type type) {
      typeVar = typeVar->getImpl().getRepresentative();
      assert(!FixedTypes[typeVar] && "Already have a type assignment!");
      FixedTypes[typeVar] = type;
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
      return openType(type, replacements);
    }

    /// \brief "Open" the given type by replacing any occurrences of archetypes
    /// (including those implicit in unbound generic types) with fresh type
    /// variables.
    ///
    /// \param type The type to open.
    ///
    /// \param replacements The mapping from opened archetypes to the type
    /// variables to which they were opened.
    ///
    /// \returns The opened type, or \c type if there are no archetypes in it.
    Type openType(Type type,
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
      UnresolvedOverloadSets.push_back(ovl);
    }

    /// \brief Retrieve the allocator used by this constraint system.
    llvm::BumpPtrAllocator &getAllocator() { return Allocator; }

    template <typename It>
    ArrayRef<typename std::iterator_traits<It>::value_type>
    allocateCopy(It start, It end) {
      typedef typename std::iterator_traits<It>::value_type T;
      T *result = (T*)Allocator.Allocate(sizeof(T)*(end-start), __alignof__(T));
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
    void generateConstraints(Expr *E);

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

    /// \brief Simplify the given constaint.
    SolutionKind simplifyConstraint(const Constraint &constraint);

    /// \brief Walks through the list of constraints, collecting the constraints
    /// that directly apply to each representative type variable.
    ///
    /// \param typeVarConstraints will be populated with a list of
    /// representative type variables and the constraints that apply directly
    /// to them.
    void collectConstraintsForTypeVariables(
           SmallVectorImpl<TypeVariableConstraints> &typeVarConstraints);

    /// \brief Simplify the constraints on the given type variable.
    ///
    /// \param tvc The type variable and its constraints.
    ///
    /// \param solved Set into which every fully-solved constraint will be
    /// added, allowing the caller to remove such constraints from the list
    /// later.
    ///
    /// \returns true if any new constraints were introduced when simplifying
    /// the type variable constraints.
    bool simplifyTypeVarConstraints(TypeVariableConstraints &tvc,
                                    SmallPtrSet<Constraint *, 4> &solved);

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

    /// \brief Determine whether this constraint system is fully solved, meaning
    /// that all type variables are either bound to fixed types or have only
    /// protocol constraints remaining on them.
    bool isSolved(SmallVectorImpl<TypeVariableType *> &freeVariables);

    void dump();
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
    for (auto supertype : supertypes) {
      if (explored.count(supertype->getCanonicalType()) > 0)
        continue;

      PotentialBindings.push_back( { typeVar, supertype } );
    }
  }
}

Type ConstraintSystem::openType(Type startingType,
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

  std::function<Type(Type)> replaceArchetypes;
  replaceArchetypes = [&](Type type) -> Type {
    // Replace archetypes with fresh type variables.
    if (auto archetype = type->getAs<ArchetypeType>()) {
      return getTypeVariable(archetype);
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
/// an implicit byref.
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
    return LValueType::get(lv->getObjectType(), quals, context);
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
                               funcTy->isAutoClosure(), context);
    }
  }

  return type;
}

Type ConstraintSystem::getTypeOfReference(ValueDecl *value) {
  // Determine the type of the value, opening up that type if necessary.
  return adjustLValueForReference(openType(value->getTypeOfReference()),
                                  value->getAttrs().isAssignment(),
                                  TC.Context);
}

Type ConstraintSystem::getTypeOfMemberReference(Type baseTy, ValueDecl *value,
                                                bool isTypeReference) {
  // Figure out the instance type used for the base.
  baseTy = baseTy->getRValueType();
  bool isInstance = true;
  if (auto baseMeta = baseTy->getAs<MetaTypeType>()) {
    baseTy = baseMeta->getInstanceType();
    isInstance = false;
  }

  // Figure out the type of the owner.
  auto owner = value->getDeclContext();
  Type ownerTy;
  if (auto nominalOwner = dyn_cast<NominalTypeDecl>(owner)) {
    ownerTy = nominalOwner->getDeclaredTypeInContext();
  } else {
    auto extensionOwner = cast<ExtensionDecl>(owner);
    auto extendedTy = extensionOwner->getExtendedType();
    if (auto nominal = extendedTy->getAs<NominalType>())
      ownerTy = nominal->getDecl()->getDeclaredTypeInContext();
    else if (auto unbound = extendedTy->getAs<UnboundGenericType>())
      ownerTy = unbound->getDecl()->getDeclaredTypeInContext();
    else
      llvm_unreachable("unknown owner for type member");
  }

  // The archetypes that have been opened up and replaced with type variables.
  llvm::DenseMap<ArchetypeType *, TypeVariableType *> replacements;

  // If the owner is specialized, we need to open up the types in the owner.
  if (ownerTy->isSpecialized()) {
    ownerTy = openType(ownerTy, replacements);
  }

  // The base type must be a subtype of the owner type.
  addConstraint(ConstraintKind::Subtype, baseTy, ownerTy);

  // Determine the type of the member.
  Type type;
  if (isTypeReference)
    type = cast<TypeDecl>(value)->getDeclaredType();
  else if (auto subscript = dyn_cast<SubscriptDecl>(value)) {
    auto resultTy = LValueType::get(subscript->getElementType(),
                                    LValueType::Qual::DefaultForMemberAccess|
                                    LValueType::Qual::Implicit,
                                    TC.Context);
    type = FunctionType::get(subscript->getIndices()->getType(), resultTy,
                             TC.Context);
  } else
    type = value->getTypeOfReference();
  type = openType(type, replacements);

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

void ConstraintSystem::generateConstraints(Expr *expr) {
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
      CS.addLiteralConstraint(tv, LiteralKind::Int);
      return tv;
    }

    Type visitFloatLiteralExpr(FloatLiteralExpr *expr) {
      auto tv = CS.createTypeVariable(expr);
      CS.addLiteralConstraint(tv, LiteralKind::Float);
      return tv;
    }

    Type visitCharacterLiteralExpr(CharacterLiteralExpr *expr) {
      auto tv = CS.createTypeVariable(expr);
      CS.addLiteralConstraint(tv, LiteralKind::Char);
      return tv;
    }

    Type visitStringLiteralExpr(StringLiteralExpr *expr) {
      auto tv = CS.createTypeVariable(expr);
      LiteralKind kind = LiteralKind::ASCIIString;
      if (std::find_if(expr->getValue().begin(), expr->getValue().end(),
                       [](char c) { return c > 127; })
            != expr->getValue().end())
        kind = LiteralKind::UTFString;
      CS.addLiteralConstraint(tv, kind);
      return tv;
    }

    Type
    visitInterpolatedStringLiteralExpr(InterpolatedStringLiteralExpr *expr) {
      auto tv = CS.createTypeVariable(expr);
      CS.addLiteralConstraint(tv, LiteralKind::UTFString);
      for (auto segment : expr->getSegments()) {
        CS.addConstraint(ConstraintKind::Construction, segment->getType(), tv);
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

    Type visitUnresolvedDeclRefExpr(UnresolvedDeclRefExpr *expr) {
      // This is an error case, where we're trying to use type inference
      // to help us determine which declaration the user meant to refer to.
      // FIXME: Do we need to note that we're doing some kind of recovery?
      return CS.createTypeVariable(expr);
    }

    Type visitMemberRefExpr(MemberRefExpr *expr) {
      llvm_unreachable("Already type-checked");
    }

    Type visitExistentialMemberRefExpr(ExistentialMemberRefExpr *expr) {
      llvm_unreachable("Already type-checked");
    }

    Type visitArchetypeMemberRefExpr(ArchetypeMemberRefExpr *expr) {
      llvm_unreachable("Already type-checked");
    }

    Type visitGenericMemberRefExpr(GenericMemberRefExpr *expr) {
      llvm_unreachable("Already type-checked");
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
      CS.addValueMemberConstraint(oneofTy, expr->getName(), memberTy);

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
      CS.addValueMemberConstraint(baseTy, expr->getName(), tv);
      return tv;
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
      // itself has type [byref] O.
      auto inputTv = CS.createTypeVariable(expr);
      auto outputTv = CS.createTypeVariable(expr);
      auto outputTy = LValueType::get(outputTv, 
                                      LValueType::Qual::DefaultForMemberAccess|
                                      LValueType::Qual::Implicit,
                                      Context);
      auto fnTy = FunctionType::get(inputTv, outputTy, Context);

      // Add the constraint for a subscript declaration.
      // FIXME: lame name!
      auto baseTy = expr->getBase()->getType();
      CS.addValueMemberConstraint(baseTy, Context.getIdentifier("__subscript"),
                                  fnTy);
      
      // Add the constraint that the index expression's type be convertible
      // to the input type of the subscript operator.
      CS.addConstraint(ConstraintKind::Conversion,
                       expr->getIndex()->getType(),
                       inputTv);
      return outputTy;
    }

    Type visitOverloadedSubscriptExpr(OverloadedSubscriptExpr *expr) {
      llvm_unreachable("Already type-checked");
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
      case PatternKind::Named:
        // For a pattern of unknown type, create a new type variable.
        return CS.createTypeVariable(expr);

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
      auto patterns = expr->getParamPatterns();
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
        tupleTypeElts.push_back(TupleTypeElt(tv, Identifier()));
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
      // The address-of operator produces an explicit lvalue [byref] T from a
      // (potentially implicit) lvalue S. We model this with the constraint
      //
      //     [byref] T < S
      //
      // where T is a fresh type variable.
      auto tv = CS.createTypeVariable(expr);
      auto result = LValueType::get(tv, LValueType::Qual::DefaultForType,
                                    CS.getASTContext());

      CS.addConstraint(ConstraintKind::Subtype,
                       result,
                       expr->getSubExpr()->getType());
      return result;
    }

    Type visitNewArrayExpr(NewArrayExpr *expr) {
      // FIXME: Eventually, we'll want to support unbound generic types in
      // the type of the array-new expression, which requires opening up the
      // unbound generic type here.
      return expr->getType();
    }

    Type visitTypeOfExpr(TypeOfExpr *expr) {
      llvm_unreachable("Already type-checked");
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
        if (auto metaTy = expr->getFn()->getType()->getAs<MetaTypeType>()) {
          auto instanceTy = metaTy->getInstanceType();
          CS.addConstraint(ConstraintKind::Construction,
                           expr->getArg()->getType(), instanceTy);
          return instanceTy;
        }
      }

      // The function subexpression has some type T1 -> T2 for fresh variables
      // T1 and T2.
      auto inputTy = CS.createTypeVariable(expr);
      auto outputTy = CS.createTypeVariable(expr);
      auto funcTy = FunctionType::get(inputTy, outputTy, Context);

      // FIXME: Allow conversions to function type? That seems completely
      // unnecessary... except perhaps for the metatype case mentioned above.
      CS.addConstraint(ConstraintKind::Equal, expr->getFn()->getType(), funcTy);

      // The argument type must be convertible to the input type.
      CS.addConstraint(ConstraintKind::Conversion, expr->getArg()->getType(),
                       inputTy);

      return outputTy;
    }

    Type visitImplicitConversionExpr(ImplicitConversionExpr *expr) {
      llvm_unreachable("Already type-checked");
    }

    Type visitCoerceExpr(CoerceExpr *expr) {
      llvm_unreachable("Already type-checked");
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
                           ->getResult());
        return expr;
      }

      auto type = CG.visit(expr);
      expr->setType(type);
      return expr;
    }

    /// \brief Ignore statements.
    virtual bool walkToStmtPre(Stmt *stmt) { return false; }

    /// \brief Ignore declarations.
    virtual bool walkToDeclPre(Decl *decl) { return false; }
  };

  // Walk the expression, generating constraints.
  ConstraintGenerator CG(*this);
  ConstraintWalker CW(CG);
  expr->walk(CW);
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

  if (type->getClassOrBoundGenericClass()) {
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
  // requiring
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
    int matched = tuple1->getNamedElementId(elt2.getName());
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
  // If we have type variables that have been bound to fixed types, look through
  // to the fixed type.
  auto typeVar1 = type1->getAs<TypeVariableType>();
  if (typeVar1) {
    if (auto fixed = getFixedType(typeVar1)) {
      type1 = fixed;
      typeVar1 = nullptr;
    }
  }

  auto typeVar2 = type2->getAs<TypeVariableType>();
  if (typeVar2) {
    if (auto fixed = getFixedType(typeVar2)) {
      type2 = fixed;
      typeVar2 = nullptr;
    }
  }

  // If the types are equivalent, we're done.
  // FIXME: This gets complicated when dealing with type variables, because
  // they can get unified via same-type requirements, breaking the canonical
  // type system in amusing and horrible ways.
  if (type1->isEqual(type2))
    return SolutionKind::TriviallySolved;

  // If either (or both) types are type variables, unify the type variables.
  if (typeVar1 || typeVar2) {
    switch (kind) {
    case TypeMatchKind::BindType:
    case TypeMatchKind::SameType: {
      if (typeVar1 && typeVar2) {
        auto rep1 = typeVar1->getImpl().getRepresentative();
        auto rep2 = typeVar2->getImpl().getRepresentative();
        if (rep1 == rep2) {
          // We already merged these two types, so this constraint is
          // trivially solved.
          return SolutionKind::TriviallySolved;
        }

        // Merge the equivalence classes corresponding to these two type
        // variables. Always merge 'up' the constraint stack, so that our
        // representative has the smallest ID within the equivalence class. This
        // ensures that the representative won't be further down the solution
        // stack than anything it is merged with.
        if (rep1->getImpl().getID() < rep2->getImpl().getID())
          rep1->getImpl().mergeEquivalenceClasses(rep2);
        else
          rep2->getImpl().mergeEquivalenceClasses(rep1);
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
        return SolutionKind::Unsolved;
        
      break;
    }
  }

  // Decompose parallel structure.
  auto canType1 = type1->getCanonicalType();
  auto canType2 = type2->getCanonicalType();
  unsigned subFlags = flags | TMF_GenerateConstraints;
  if (canType1->getKind() == canType2->getKind()) {
    switch (canType1->getKind()) {
#define SUGARED_TYPE(id, parent) case TypeKind::id:
#define TYPE(id, parent)
#include "swift/AST/TypeNodes.def"
      llvm_unreachable("Sugared type masquerading as canonical");

#define ALWAYS_CANONICAL_TYPE(id, parent) case TypeKind::id:
#define TYPE(id, parent)
#include "swift/AST/TypeNodes.def"
    case TypeKind::Error:
      return SolutionKind::Error;

    case TypeKind::UnstructuredUnresolved:
      llvm_unreachable("Unstructured unresolved type");

    case TypeKind::TypeVariable:
      llvm_unreachable("Type variables handled above");

    case TypeKind::Tuple: {
      auto tuple1 = cast<TupleType>(canType1);
      auto tuple2 = cast<TupleType>(canType2);
      return matchTupleTypes(tuple1, tuple2, kind, flags, trivial);
    }

    case TypeKind::OneOf:
    case TypeKind::Struct:
    case TypeKind::Class: {
      auto nominal1 = cast<NominalType>(canType1);
      auto nominal2 = cast<NominalType>(canType2);
      if (nominal1->getDecl() == nominal2->getDecl()) {
        return SolutionKind::Error;

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
      auto meta1 = cast<MetaTypeType>(canType1);
      auto meta2 = cast<MetaTypeType>(canType2);

      // metatype<B> < metatype<A> if A < B and both A and B are classes.
      TypeMatchKind subKind = TypeMatchKind::SameType;
      if (kind != TypeMatchKind::SameType &&
          meta1->getInstanceType()->getClassOrBoundGenericClass() &&
          meta2->getInstanceType()->getClassOrBoundGenericClass())
        subKind = std::max(kind, TypeMatchKind::Subtype);
      
      return matchTypes(meta1->getInstanceType(), meta2->getInstanceType(),
                        subKind, subFlags, trivial);
    }

    case TypeKind::Function: {
      auto func1 = cast<FunctionType>(canType1);
      auto func2 = cast<FunctionType>(canType2);
      return matchFunctionTypes(func1, func2, kind, flags, trivial);
    }

    case TypeKind::PolymorphicFunction:
      llvm_unreachable("Polymorphic function type should have been opened");

    case TypeKind::Array: {
      auto array1 = cast<ArrayType>(canType1);
      auto array2 = cast<ArrayType>(canType2);
      return matchTypes(array1->getBaseType(), array2->getBaseType(),
                        TypeMatchKind::SameType, subFlags, trivial);
    }

    case TypeKind::ProtocolComposition:
      // Existential types handled below.
      break;

    case TypeKind::LValue: {
      auto lvalue1 = cast<LValueType>(canType1);
      auto lvalue2 = cast<LValueType>(canType2);
      if (lvalue1->getQualifiers() != lvalue2->getQualifiers() &&
          !(kind >= TypeMatchKind::TrivialSubtype &&
            lvalue1->getQualifiers() < lvalue2->getQualifiers()))
        return SolutionKind::Error;

      return matchTypes(lvalue1->getObjectType(), lvalue2->getObjectType(),
                        TypeMatchKind::SameType, subFlags, trivial);
    }

    case TypeKind::UnboundGeneric:
      llvm_unreachable("Unbound generic type should have been opened");

    case TypeKind::BoundGeneric: {
      auto bound1 = cast<BoundGenericType>(canType1);
      auto bound2 = cast<BoundGenericType>(canType2);
      
      if (bound1->getDecl() == bound2->getDecl()) {
        // Match up the parents, exactly, if there are parents.
        SolutionKind result = SolutionKind::TriviallySolved;
        assert((bool)bound1->getParent() == (bool)bound2->getParent() &&
               "Mismatched parents of bound generics");
        if (bound1->getParent()) {
          switch (matchTypes(bound1->getParent(), bound2->getParent(),
                             TypeMatchKind::SameType, subFlags, trivial)) {
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

        // Match up the generic arguments, exactly.
        auto args1 = bound1->getGenericArgs();
        auto args2 = bound2->getGenericArgs();
        assert(args1.size() == args2.size() && "Mismatched generic args");
        for (unsigned i = 0, n = args1.size(); i != n; ++i) {
          switch (matchTypes(args1[i], args2[i], TypeMatchKind::SameType,
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

    // A class (or bound generic class) is a subtype of another class
    // (or bound generic class) if it is derived from that class.
    if (type1->getClassOrBoundGenericClass() &&
        type2->getClassOrBoundGenericClass()) {
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
        return matchTypes(type1, function2->getResult(), kind, flags, trivial);
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
    MemberLookup lookup(type1, name, TC.TU);
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

Type ConstraintSystem::simplifyType(Type type,
       llvm::SmallPtrSet<TypeVariableType *, 16> &substituting) {
  return TC.transformType(type,
                          [&](Type type) -> Type {
            if (auto tvt = type->getAs<TypeVariableType>()) {
              tvt = tvt->getImpl().getRepresentative();
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
  if (auto tv = type->getAs<TypeVariableType>()) {
    auto fixed = getFixedType(tv);
    if (!fixed)
      return SolutionKind::Unsolved;

    // Continue with the fixed type.
    type = fixed;
  }

  return TC.isLiteralCompatibleType(type, SourceLoc(), kind,
                                    /*Complain=*/false).first
           ? SolutionKind::TriviallySolved
           : SolutionKind::Error;
}

ConstraintSystem::SolutionKind
ConstraintSystem::simplifyMemberConstraint(const Constraint &constraint) {
  // Resolve the base type, if we can. If we can't resolve the base type,
  // then we can't solve this constraint.
  Type baseTy = constraint.getFirstType();
  if (auto tv = baseTy->getAs<TypeVariableType>()) {
    auto fixed = getFixedType(tv);
    if (!fixed)
      return SolutionKind::Unsolved;

    // Continue with the fixed type.
    baseTy = fixed;
  }

  // If the base type is a tuple type, look for the named or indexed member
  // of the tuple.
  Identifier name = constraint.getMember();
  Type memberTy = constraint.getSecondType();
  if (auto baseTuple = baseTy->getRValueType()->getAs<TupleType>()) {
    StringRef nameStr = name.str();
    int fieldIdx = -1;
    if (nameStr[0] == '$') {
      // Resolve a number reference into the tuple type.
      unsigned Value = 0;
      if (!nameStr.substr(1).getAsInteger(10, Value) &&
          Value < baseTuple->getFields().size())
        fieldIdx = Value;
    } else {
      baseTuple->getNamedElementId(name);
    }

    if (fieldIdx == -1)
      return SolutionKind::Error;

    // Determine the type of a reference to this tuple element.
    auto fieldType = baseTuple->getElementType(fieldIdx);
    // FIXME: What if we later find out that memberTy is an lvalue type?
    // Do we have to worry about lvalue types of lvalue types forming?
    if (!memberTy->is<LValueType>()) {
      if (auto baseLv = baseTy->getAs<LValueType>()) {
        fieldType = LValueType::get(fieldType, baseLv->getQualifiers(),
                                    TC.Context);
      }
    }

    addConstraint(ConstraintKind::Equal, fieldType, memberTy);
    return SolutionKind::Solved;
  }

  // FIXME: If the base type still involves type variables, we want this
  // constraint to be unsolved. This effectively requires us to solve the
  // left-hand side of a dot expression before we look for members.

  if (name.str() == "constructor") {
    // Constructors have their own approach to name lookup.
    ConstructorLookup constructors(baseTy, TC.TU);
    if (!constructors.isSuccess()) {
      return SolutionKind::Error;
    }

    // Check whether we have an 'identity' constructor.
    bool sawIdentityConstructor = false;
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

          if (inputTy->isEqual(baseTy)) {
            sawIdentityConstructor = true;
            break;
          }
        }
      }
    }

    // If there is only a single result, compute the type of a reference to
    // that constructor and equate it with the member type.
    if (constructors.Results.size() == 1 && sawIdentityConstructor) {
      auto refType = getTypeOfMemberReference(baseTy, constructors.Results[0],
                                              /*isTypeReference=*/false);
      addConstraint(ConstraintKind::Bind, memberTy, refType);
      return SolutionKind::Solved;
    }

    // We have multiple results. Introduce a new overload set.
    SmallVector<OverloadChoice, 4> choices;
    for (auto constructor : constructors.Results) {
      choices.push_back(OverloadChoice(baseTy, constructor));
    }

    // If we didn't see an "identity" constructor, then add an entry in the
    // overload set for T -> T, where T is the base type. This entry acts as a
    // stand-in for conversion of the argument to T.
    if (!sawIdentityConstructor) {
      choices.push_back(OverloadChoice(baseTy,
                                       OverloadChoiceKind::IdentityFunction));
    }
    addOverloadSet(OverloadSet::getNew(*this, memberTy, &constraint, choices));
    return SolutionKind::Solved;
  }

  // Look for members within the base.
  MemberLookup lookup(baseTy, name, TC.TU);
  if (!lookup.isSuccess()) {
    return SolutionKind::Error;
  }

  // Can't refer to a property without an object instance.
  // FIXME: Subscripts have a similar restriction. Check it here.
  bool isMetatypeBase = baseTy->getRValueType()->is<MetaTypeType>();
  if (isMetatypeBase && lookup.Results.size() == 1 &&
      lookup.Results[0].Kind == MemberLookupResult::MemberProperty) {
    return SolutionKind::Error;
  }

  // If we only have a single result, compute the type of a reference to
  // that declaration and equate it with the member type.
  bool isTypeMember = constraint.getKind() == ConstraintKind::TypeMember;
  if (lookup.Results.size() == 1) {
    if (isTypeMember && !isa<TypeDecl>(lookup.Results[0].D))
      return SolutionKind::Error;
    
    auto refType = getTypeOfMemberReference(baseTy, lookup.Results[0].D,
                                            isTypeMember);
    addConstraint(ConstraintKind::Bind, memberTy, refType);
    return SolutionKind::Solved;
  }

  // We have multiple results. Introduce a new overload set.
  SmallVector<OverloadChoice, 4> choices;
  for (auto &result : lookup.Results) {
    choices.push_back(OverloadChoice(baseTy, result.D));
  }
  addOverloadSet(OverloadSet::getNew(*this, memberTy, &constraint, choices));
  return SolutionKind::Solved;
}

/// \brief Retrieve the type-matching kind corresponding to the given
/// constraint kind.
static TypeMatchKind getTypeMatchKind(ConstraintKind kind) {
  switch (kind) {
  case ConstraintKind::Bind: return TypeMatchKind::BindType;
  case ConstraintKind::Equal: return TypeMatchKind::SameType;
  case ConstraintKind::TrivialSubtype: return TypeMatchKind::TrivialSubtype;
  case ConstraintKind::Subtype: return TypeMatchKind::Subtype;
  case ConstraintKind::Conversion: return TypeMatchKind::Conversion;
  case ConstraintKind::Construction: return TypeMatchKind::Construction;

  case ConstraintKind::Literal:
    llvm_unreachable("Literals don't involve type matches");

  case ConstraintKind::ValueMember:
  case ConstraintKind::TypeMember:
    llvm_unreachable("Member constraints don't involve type matches");
  }
}

ConstraintSystem::SolutionKind
ConstraintSystem::simplifyConstraint(const Constraint &constraint) {
  switch (constraint.getKind()) {
  case ConstraintKind::Bind:
  case ConstraintKind::Equal:
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
  }
}

namespace {
  /// \brief Provide a partial ordering between two constraints.
  ///
  /// The ordering depends on the the classification of constraints:
  ///   - For relational constraints, the constraints are ordered by strength,
  ///     e.g., == precedes < and < precedes <<.
  ///
  ///   - For literal constraints, we order by literal kind.
  ///
  ///   - For member constraints, we order by member name (lexicographically)
  ///     and, for equivalently-named constraints, with value members
  ///     preceding type members.
  class OrderConstraintForTypeVariable {
  public:
    bool operator()(Constraint *lhs, Constraint *rhs) const {
      // If the classifications differ, order by classification.
      auto lhsClass = lhs->getClassification();
      auto rhsClass = rhs->getClassification();
      if (lhsClass != rhsClass)
        return lhsClass < rhsClass;

      switch (lhsClass) {
      case ConstraintClassification::Relational: {
        // Order based on the relational constraint kind.
        return lhs->getKind() < rhs->getKind();
      }

      case ConstraintClassification::Literal:
        // For literals, order by literal kind.
        return lhs->getLiteralKind() < rhs->getLiteralKind();

      case ConstraintClassification::Member:
        // For members, order first by member name.
        if (lhs->getMember() != rhs->getMember()) {
          return lhs->getMember().str() < rhs->getMember().str();
        }

        // Order by type vs. value member. Value members come first.
        return lhs->getKind() < rhs->getKind();
      }
    }
  };
}

void ConstraintSystem::collectConstraintsForTypeVariables(
       SmallVectorImpl<TypeVariableConstraints> &typeVarConstraints) {
  typeVarConstraints.clear();

  // First, collect all of the constraints that relate directly to a
  // type variable.
  llvm::DenseMap<TypeVariableType *, unsigned> typeVarConstraintsMap;
  for (auto constraint : Constraints) {
    if (auto firstTV = constraint->getFirstType()->getAs<TypeVariableType>()) {
      // Retrieve the representative.
      firstTV = firstTV->getImpl().getRepresentative();

      // Find the entry in the constraints output vector that corresponds to
      // this type variable.
      unsigned& constraintsIdx = typeVarConstraintsMap[firstTV];
      if (!constraintsIdx) {
        typeVarConstraints.push_back(TypeVariableConstraints(firstTV));
        constraintsIdx = typeVarConstraints.size();
      }

      // Record the constraint.
      typeVarConstraints[constraintsIdx - 1].Constraints.push_back(constraint);
    }

    switch (constraint->getKind()) {
    case ConstraintKind::Bind:
    case ConstraintKind::Equal:
    case ConstraintKind::TrivialSubtype:
    case ConstraintKind::Subtype:
    case ConstraintKind::Conversion:
    case ConstraintKind::Construction:
      break;

    case ConstraintKind::Literal:
    case ConstraintKind::ValueMember:
    case ConstraintKind::TypeMember:
      // We don't care about the second type (or there isn't one).
      continue;
    }

    auto secondTV = constraint->getSecondType()->getAs<TypeVariableType>();
    if (!secondTV)
      continue;

    // Retrieve the representative.
    secondTV = secondTV->getImpl().getRepresentative();

    // Find the entry in the constraints output vector that corresponds to
    // this type variable.
    unsigned& constraintsIdx = typeVarConstraintsMap[secondTV];
    if (!constraintsIdx) {
      typeVarConstraints.push_back(TypeVariableConstraints(secondTV));
      constraintsIdx = typeVarConstraints.size();
    }

    // Record the constraint.
    typeVarConstraints[constraintsIdx - 1].Constraints.push_back(constraint);
  }

  // Sort the type variable constraints.
  for (auto &tvc : typeVarConstraints) {
    std::stable_sort(tvc.Constraints.begin(), tvc.Constraints.end(),
                     OrderConstraintForTypeVariable());
  }
}

namespace {
  /// \brief Maintains the set of type variable constraints
  struct RelationalTypeVarConstraints {
    RelationalTypeVarConstraints() : Below(), Above() { }
    
    Constraint *Below;
    Constraint *Above;
  };
}

/// \brief Collect the set of types that are bound "below" and "above" the
/// given type variable T.
///
/// A type X is above T if there is a constraint
/// T <t X, T < X, or T << X, whereas it is below T if there is a constraint
/// X <t T, X < T, X << T.
///
/// \param cs The constraint system in which these constraints occur.
///
/// \param tvc The type variable and its constraints. The constraints must
/// already have been sorted with the predicate
/// \c OrderConstraintForTypeVariable.
///
/// \param typesBelow Will be populated with a set of type/constraint pairs
/// describing the types that are "below" the type variable. The list of types
/// is uniqued, and the corresponding constraint will be the most strict
/// constraint for that particular type.
///
/// \param typesAbove Will be populated with a set of type/constraint pairs
/// describing the types that are "above" the type variable. The list of types
/// is uniqued, and the corresponding constraint will be the most strict
/// constraint for that particular type.
///
/// \param onRedundant An optional callback that will be invoked when we
/// uncover a redundant constraint.
///
/// \param onAboveAndBelow An optional callback that will be invoked when
/// a given type occurs both above and below the type variable.
///
/// \returns An iterator into the list of constraints on this type variable
/// that points just past the last relational constraint.
static SmallVectorImpl<Constraint *>::iterator
collectTypesAboveAndBelow(ConstraintSystem &cs,
  TypeVariableConstraints &tvc,
  SmallVectorImpl<std::pair<Type, Constraint *>> &typesBelow,
  SmallVectorImpl<std::pair<Type, Constraint *>> &typesAbove,
  std::function<void(Constraint *)> onRedundant,
  std::function<void(Type, Constraint *, Constraint *)> onAboveAndBelow)
{
  // Collect the relational constraints above and below each simplified type,
  // dropping any redundant constraints in the process.
  llvm::DenseMap<CanType, RelationalTypeVarConstraints> typeVarConstraints;
  auto curCons = tvc.Constraints.begin(),
  lastConstraint = tvc.Constraints.end();
  for (; curCons != lastConstraint &&
       (*curCons)->getClassification()  == ConstraintClassification::Relational;
       ++curCons) {
    // Determine whether we have a constraint below (vs. a constraint above)
    // this type variable.
    bool constraintIsBelow = false;
    if (auto secondTV = (*curCons)->getSecondType()
            ->getAs<TypeVariableType>()) {
      constraintIsBelow = (secondTV->getImpl().getRepresentative()
                            == tvc.TypeVar);
    }

    // Compute the type bound, and simplify it.
    Type bound = constraintIsBelow? (*curCons)->getFirstType()
                                  : (*curCons)->getSecondType();
    bound = cs.simplifyType(bound);

    Constraint *(RelationalTypeVarConstraints::*which)
      = constraintIsBelow? &RelationalTypeVarConstraints::Below
                         : &RelationalTypeVarConstraints::Above;
    auto &typeVarConstraint = typeVarConstraints[bound->getCanonicalType()];

    if (typeVarConstraint.*which) {
      // We already have a constraint that provides either the same relation
      // or a tighter relation, because the sort orders relational constraints
      // in order of nonincreasing strictness.
      // 
      // FIXME: If the type may end up being a tuple type, then a conversion
      // constraint is not redundant with a subtyping constraint.
      assert(((typeVarConstraint.*which)->getKind()
              <= (*curCons)->getKind()) &&
             "Improper ordering of constraints");
      if (onRedundant)
        onRedundant(*curCons);
      continue;
    }

    // Record this constraint.
    typeVarConstraint.*which = *curCons;
    if (constraintIsBelow)
      typesBelow.push_back(std::make_pair(bound, *curCons));
    else
      typesAbove.push_back(std::make_pair(bound, *curCons));

    // If a type variable T is bounded both above and below by a nominal type X
    // (or specialization of one), tell the caller.
    if (typeVarConstraint.Below && typeVarConstraint.Above &&
        onAboveAndBelow)
      onAboveAndBelow(bound, typeVarConstraint.Above, typeVarConstraint.Below);
  }

  return curCons;
}

bool
ConstraintSystem::simplifyTypeVarConstraints(TypeVariableConstraints &tvc,
                                         SmallPtrSet<Constraint *, 4> &solved) {
  bool addedConstraints = false;
  bool foundEquality = false;

  // Collect the relational constraints above and below each simplified type,
  // dropping any redundant constraints in the process.
  SmallVector<std::pair<Type, Constraint *>, 4> typesBelow;
  SmallVector<std::pair<Type, Constraint *>, 4> typesAbove;

  auto lastConstraint = tvc.Constraints.end();
  auto currentConstraint
    = collectTypesAboveAndBelow(*this, tvc, typesBelow, typesAbove,
                                [&](Constraint *redundant) {
                                  solved.insert(redundant);
                                },
        [&](Type bound,
            Constraint *above,
            Constraint *below) {
          // If a type variable T is bounded both above and below by a nominal
          // type X (or specialization of one), then we can conclude that
          // X == T, e.g.,
          //
          //   X < T and T < X => T == X
          //
          // Note that this rule does not apply in general because tuple types
          // allow one to add or drop labels essentially at will, so there are
          // multiple types T for which X < T and T < X. With nominal types,
          // we prohibit cyclic conversions and subtyping relationships.
          if (bound->is<NominalType>() || bound->is<BoundGenericType>()) {
            addConstraint(ConstraintKind::Equal, tvc.TypeVar, bound);
            foundEquality = true;
            addedConstraints = true;
          }
        });

  // If we determined an equality constraint, all of the relational constraints
  // are now solved.
  if (foundEquality) {
    for (auto at : typesAbove) {
        solved.insert(at.second);
    }
    for (auto bt : typesBelow) {
      solved.insert(bt.second);
    }
  }

  // Introduce transitive constraints, e.g.,
  //
  //   X < T and T << Y => X << Y
  //
  // When the two relation kinds differ, as above, the weaker constraint is
  // used for the resulting relation.
  // FIXME: Actually implement this, taking care to avoid introducing cycles
  // in simplify().

  // Remove redundant literal constraints.
  Constraint *prevLit = nullptr;
  for (; currentConstraint != lastConstraint &&
       (*currentConstraint)->getClassification()
         == ConstraintClassification::Literal;
       ++currentConstraint) {
    if (!prevLit) {
      prevLit = *currentConstraint;
      continue;
    }

    // We have a redundant literal constraint; remove the latter constraint.
    if (prevLit->getLiteralKind() == (*currentConstraint)->getLiteralKind()) {
      solved.insert(*currentConstraint);
    }
  }

  // FIXME: If we see multiple type-member constraints, we can equate the
  // right-hand types.
  // FIXME: If we see a value member constraint and a type-member constraint
  // with the same name, we can relate the first to the metatype of the
  // second.
  return addedConstraints;
}

bool ConstraintSystem::simplify() {
  bool firstLoop = true;
  bool solvedAny;
  SmallPtrSet<Constraint *, 4> typeVarResolved;
  do {
    // Loop through all of the thus-far-unsolved constraints, attempting to
    // simplify each one.
    SmallVector<Constraint *, 16> existingConstraints;
    existingConstraints.swap(Constraints);
    solvedAny = false;
    for (auto constraint : existingConstraints) {
      // If we have already solved this constraint externally, just silently
      // drop it.
      if (typeVarResolved.count(constraint))
        continue;

      switch (simplifyConstraint(*constraint)) {
      case SolutionKind::Error:
        failedConstraint = constraint;
        markUnsolvable();
        return true;

      case SolutionKind::TriviallySolved:
        // Implicitly drop this solved constraint.
        break;
          
      case SolutionKind::Solved:
        // Implicitly drop this solved constraint.
        solvedAny = true;
        break;

      case SolutionKind::Unsolved:
        // Since this constraint was not solved, add it to the list of
        // remaining constraints.
        Constraints.push_back(constraint);
        break;
      }
    }
    existingConstraints.clear();

    // If we didn't actually solve anything real and this isn't the first time
    // through the loop, bail out now so we don't do the work of checking
    // specific constraints on type variables.
    if (!solvedAny && !firstLoop) {
      break;
    }

    // From the remaining constraints, collect the constraints for each of
    // the representative (non-fixed) type variables.
    SmallVector<TypeVariableConstraints, 32> typeVarConstraints;
    typeVarResolved.clear();
    collectConstraintsForTypeVariables(typeVarConstraints);
    for (auto &tvc : typeVarConstraints) {
      if (simplifyTypeVarConstraints(tvc, typeVarResolved))
        solvedAny = true;
    }
    typeVarConstraints.clear();

    // If we solved any constraints via type variable constraints and we aren't
    // going to go through the constraint list again, just remove these
    // constraints from the list.
    if (!typeVarResolved.empty() && !solvedAny) {
      Constraints.erase(std::remove_if(Constraints.begin(),
                                       Constraints.end(),
                                       [&](Constraint *con) {
                                         return typeVarResolved.count(con) > 0;
                                       }),
                         Constraints.end());
    }

    firstLoop = false;
  } while (solvedAny);

  // We've simplified all of the constraints we can.
  return false;
}

//===--------------------------------------------------------------------===//
// Constraint solving
//===--------------------------------------------------------------------===//
#pragma mark Constraint solving

/// \brief Resolve an overload set in the given constraint system by
/// producing a set of child constraint systems, each of which picks a specific
/// overload from that set. Those child constraint systems that do not fail
/// during simplification will be added to the stack of constraint systems
/// being considered.
static void resolveOverloadSet(ConstraintSystem &cs,
                               OverloadSet *ovl,
                               SmallVectorImpl<ConstraintSystem *> &stack) {
  auto choices = ovl->getChoices();
  for (unsigned i = 0, n = choices.size(); i != n; ++i) {
    auto idx = n-i-1;
    auto &choice = ovl->getChoices()[idx];
    auto childCS = cs.createDerivedConstraintSystem(idx);

    // Determie the type to which we'll bind the overload set's type.
    Type refType;
    switch (choice.getKind()) {
    case OverloadChoiceKind::Decl: {
      // Retrieve the type of a reference to the specific declaration choice.
      if (choice.getBaseType())
        refType = childCS->getTypeOfMemberReference(choice.getBaseType(),
                                                    choice.getDecl(),
                                                    /*FIXME:*/false);
      else
        refType = childCS->getTypeOfReference(choice.getDecl());

      bool isAssignment = choice.getDecl()->getAttrs().isAssignment();
      refType = adjustLValueForReference(refType, isAssignment,
                                         cs.getASTContext());
      break;
    }

    case OverloadChoiceKind::BaseType:
      refType = choice.getBaseType();
      break;

    case OverloadChoiceKind::FunctionReturningBaseType:
      refType = FunctionType::get(childCS->createTypeVariable(),
                                  choice.getBaseType(),
                                  cs.getASTContext());
      break;
    case OverloadChoiceKind::IdentityFunction:
      refType = FunctionType::get(choice.getBaseType(), choice.getBaseType(),
                                  cs.getASTContext());
      break;
    }

    childCS->addConstraint(ConstraintKind::Bind, ovl->getBoundType(), refType);

    // Simplify the child system. Assuming it's still valid, add it to
    // the stack to be dealt with later.
    // FIXME: If it's not still valid, keep it around for diagnostics.
    if (!childCS->simplify())
      stack.push_back(childCS);
  }
}

/// \brief Find the lower and upper bounds on a type variable.
static std::pair<Type, Type>
findTypeVariableBounds(ConstraintSystem &cs, TypeVariableConstraints &tvc) {
  // Collect the relational constraints above and below the type variable.
  // FIXME: We perform this operation multiple times. It would be better to
  // keep this information online, generated either as constraints are
  // added or as part of simplification.
  SmallVector<std::pair<Type, Constraint *>, 4> typesBelow;
  SmallVector<std::pair<Type, Constraint *>, 4> typesAbove;
  collectTypesAboveAndBelow(cs, tvc, typesBelow, typesAbove, nullptr, nullptr);

  std::pair<Type, Type> bounds;

  // Only consider concrete above/below that don't involve type variables.
  // FIXME: Do we really need this restriction?
  auto pairHasTypeVariable = [](std::pair<Type, Constraint *> tc) {
    return tc.first->hasTypeVariable();
  };
  typesBelow.erase(std::remove_if(typesBelow.begin(), typesBelow.end(),
                                  pairHasTypeVariable),
                   typesBelow.end());
  typesAbove.erase(std::remove_if(typesAbove.begin(), typesAbove.end(),
                                  pairHasTypeVariable),
                   typesAbove.end());

  if (!typesBelow.empty()) {
    if (typesBelow.size() == 1) {
      bounds.first = typesBelow.front().first;
    } else {
      // FIXME: Compute the meet of the types in typesBelow. We'll miss
      // potential solutions with the current approach.
      bounds.first = typesBelow.front().first;
    }
  }

  if (!typesAbove.empty()) {
    if (typesAbove.size() == 1) {
      bounds.second = typesAbove.front().first;
    } else {
      // FIXME: Compute the join of the types in typesAbove. We'll miss
      // potential solutions with the current approach.
      bounds.second = typesAbove.front().first;
    }
  }

  return bounds;
}

/// \brief Given the direct constraints placed on the type variables within
/// a given constraint system, create potential bindings that resolve the
/// constraints for a type variable.
static void
resolveTypeVariable(ConstraintSystem &cs,
  SmallVectorImpl<TypeVariableConstraints> &typeVarConstraints) {

  // Find a type variable with a lower bound, because those are the most
  // interesting.
  TypeVariableConstraints *tvcWithUpper = nullptr;
  Type tvcUpper;
  for (auto &tvc : typeVarConstraints) {
    // If we already explored type bindings for this type variable, skip it.
    if (cs.haveExploredTypeVar(tvc.TypeVar))
      continue;

    auto bounds = findTypeVariableBounds(cs, tvc);

    // If we have a lower bound, introduce a child constraint system binding
    // this type variable to that lower bound.
    if (bounds.first) {
      cs.addPotentialBinding(tvc.TypeVar, bounds.first);
      return;
    }

    // If there is an upper bound, record that (but don't use it yet).
    if (bounds.second && !tvcWithUpper) {
      tvcWithUpper = &tvc;
      tvcUpper = bounds.second;
    }
  }

  // If we had an upper bound, introduce a child constraint system binding
  // this type variable to that upper bound.
  // FIXME: This type may be too specific, but we'd really rather not
  // go on a random search for subtypes that might work.
  if (tvcWithUpper) {
    cs.addPotentialBinding(tvcWithUpper->TypeVar, tvcUpper);
    return;
  }

  // FIXME: Attempt to resolve literal constraints by dropping in the default
  // literal types.
}

bool ConstraintSystem::solve(SmallVectorImpl<ConstraintSystem *> &viable) {
  assert(&getTopConstraintSystem() == this &&"Can only solve at the top level");

  // Seed the constraint system stack with ourselves.
  SmallVector<ConstraintSystem *, 16> stack;
  stack.push_back(this);

  // If there are any unresolved overload sets, create child systems in
  // which we resolve the next overload set.
  while (!stack.empty()) {
    auto cs = stack.back();
    stack.pop_back();

    // If there are any unresolved overload sets, create child systems in
    // which we resolve the overload set to each option.
    if (!cs->UnresolvedOverloadSets.empty()) {
      resolveOverloadSet(*cs, cs->UnresolvedOverloadSets.front(), stack);

      // When we've resolved an overload set, we are done with this constraint
      // system: everything else will be handled by .
      continue;
    }

    // If there are any children of this system that are still active,
    // then we found a potential solution. There is no need to explore
    // alternatives based on this constraint system.
    if (cs->hasActiveChildren())
      continue;

    // If we have not queued up potential bindings (or have exhausted all of
    // our potential bindings thus far), try to generate new bindings.
    if (cs->PotentialBindings.empty()) {
      // Collect the list of constraints that apply directly to each of the
      // type variables. This list will be used in subsequent solution attempts.
      // FIXME: Tons of redundant computation here. We need to keep this
      // data online.
      SmallVector<TypeVariableConstraints, 16> typeVarConstraints;
      cs->collectConstraintsForTypeVariables(typeVarConstraints);

      // If there are no remaining type constraints, this system is either
      // solved or underconstrained. 
      if (typeVarConstraints.empty()) {
        viable.push_back(cs);
        continue;
      }

      // Pick a type variable to resolve into a potential binding.
      resolveTypeVariable(*cs, typeVarConstraints);
    }

    // If there are no potential bindings, we have nothing else we can
    // explore. Consider this system dead.
    if (cs->PotentialBindings.empty()) {
      cs->markUnsolvable();
      continue;
    }

    // Push this constraint system back onto the stack to be reconsidered if
    // none of the child systems created below succeed.
    stack.push_back(cs);

    // Create child systems for each of the potential bindings.
    auto potentialBindings = std::move(cs->PotentialBindings);
    cs->PotentialBindings.clear();
    for (auto binding : potentialBindings) {
      if (cs->exploreBinding(binding.first, binding.second)) {
        auto childCS = cs->createDerivedConstraintSystem(binding.first);
        childCS->addConstraint(ConstraintKind::Equal, binding.first,
                               binding.second);
        if (!childCS->simplify())
          stack.push_back(childCS);
      }
    }
  }

  // FIXME: Having more than one child system left at the end means there
  // is an ambiguity. Diagnose it.
  return viable.size() == 0;
}

bool
ConstraintSystem::isSolved(SmallVectorImpl<TypeVariableType *> &freeVariables) {
  // Look for a failed constraint.
  if (failedConstraint)
    return false;
  
  // Look for any unresolved overload sets.
  if (!UnresolvedOverloadSets.empty())
    return false;

  // Look for any free type variables.
  for (auto tv : TypeVariables) {
    // We only care about the representatives.
    if (tv->getImpl().getRepresentative() != tv)
      continue;

    if (auto fixed = getFixedType(tv)) {
      if (simplifyType(fixed)->hasTypeVariable())
        return false;
      
      continue;
    }

    freeVariables.push_back(tv);
  }

  // Look for any remaining constraints.
  for (auto con : Constraints) {
    // First type must be a type variable...
    auto tv = con->getFirstType()->getAs<TypeVariableType>();
    if (!tv)
      return false;

    // ... that has not been bound to a fixed type.
    tv = tv->getImpl().getRepresentative();
    if (getFixedType(tv))
      return false;

    // This must be a subtyping or conversion constraint.
    if (con->getKind() != ConstraintKind::Subtype &&
        con->getKind() != ConstraintKind::Conversion)
      return false;

    // The second type must be existential.
    if (!con->getSecondType()->isExistentialType())
      return false;
  }

  assert(!freeVariables.empty() || Constraints.empty());
  return true;
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
      if (cs->InOverloadSet) {
        auto &choice = cs->InOverloadSet->getChoices()[cs->OverloadChoiceIdx];
        out << "  selected overload set #" << cs->InOverloadSet->getID()
            << " choice #" << cs->OverloadChoiceIdx << " for ";
        switch (choice.getKind()) {
        case OverloadChoiceKind::Decl:
          if (choice.getBaseType())
            out << choice.getBaseType()->getString() << ".";
          out << choice.getDecl()->getName().str() << ": "
            << cs->InOverloadSet->getBoundType()->getString() << " == "
            << choice.getDecl()->getTypeOfReference()->getString() << "\n";
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
        }

        continue;
      }

      out << "  assuming " << cs->assumedTypeVar->getString() << " == "
          << cs->getFixedType(cs->assumedTypeVar) << "\n";
    }
    out << "\n";
  }

  out << "Type Variables:\n";
  for (auto cs = this; cs; cs = cs->Parent) {
    for (auto tv : cs->TypeVariables) {
      out.indent(2);
      tv->getImpl().print(out);
      if (tv->getImpl().getRepresentative() == tv) {
        if (auto fixed = getFixedType(tv)) {
          out << " as ";
          fixed->print(out);
        }
      }
      out << "\n";
    }
  }
  
  out << "\nConstraints:\n";
  for (auto constraint : Constraints) {
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
        }
      }
    }
  }

  SmallVector<TypeVariableType *, 4> freeVariables;
  if (isSolved(freeVariables)) {
    if (freeVariables.empty()) {
      llvm::errs() << "SOLVED (completely)\n";
    } else {
      llvm::errs() << "SOLVED (with free variables):";
      for (auto fv : freeVariables) {
        llvm::errs() << ' ' << fv->getString();
      }
      llvm::errs() << '\n';
    }
  } else {
    llvm::errs() << "UNSOLVED\n";
  }
}

bool TypeChecker::dumpConstraints(Expr *expr) {
  ConstraintSystem CS(*this);
  llvm::errs() << "---Initial constraints for the given expression---\n";
  CS.generateConstraints(expr);
  expr->dump();
  llvm::errs() << "\n";
  CS.dump();
  llvm::errs() << "---Simplified constraints---\n";
  bool error = CS.simplify();
  CS.dump();

  SmallVector<TypeVariableType *, 4> freeVariables;
  unsigned numSolved = 0;
  if (CS.isSolved(freeVariables))
    ++numSolved;
  if (!error && numSolved == 0) {
    llvm::errs() << "---Solved constraints---\n";
    SmallVector<ConstraintSystem *, 4> viable;
    error = CS.solve(viable);
    CS.dump();

    if (!error) {
      llvm::errs() << "---Viable constraint systems---\n";
      unsigned idx = 0;
      for (auto cs : viable) {
        llvm::errs() << "---Child system #" << ++idx << "---\n";
        cs->dump();
        freeVariables.clear();
        if (cs->isSolved(freeVariables))
          ++numSolved;
      }
    }
  }

  bool solved = false;
  if (numSolved == 0)
    llvm::errs() << "No solution found.\n";
  else if (numSolved == 1) {
    llvm::errs() << "Unique solution found.\n";
    solved = true;
  } else
    llvm::errs()<< "Found " << numSolved << " potential solutions.\n";

  return !solved;
}


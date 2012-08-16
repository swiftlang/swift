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

namespace {
  /// \brief Retrieve the name of a literal kind.
  static StringRef getLiteralKindName(LiteralKind kind) {
    switch (kind) {
    case LiteralKind::Int: return "integer";
    case LiteralKind::Float: return "float";
    case LiteralKind::Char: return "character";
    case LiteralKind::UTFString: return "UTF-8 string";
    case LiteralKind::ASCIIString: return "ASCII string";
    }
  }

  class OverloadChoice;
}

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
    /// symmetric constraint.
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
      case ConstraintKind::Equal:
      case ConstraintKind::TrivialSubtype:
      case ConstraintKind::Subtype:
      case ConstraintKind::Conversion:
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
      case ConstraintKind::Equal:
      case ConstraintKind::TrivialSubtype:
      case ConstraintKind::Subtype:
      case ConstraintKind::Conversion:
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
      case ConstraintKind::Equal: Out << " == "; break;
      case ConstraintKind::TrivialSubtype: Out << " <t "; break;
      case ConstraintKind::Subtype: Out << " < "; break;
      case ConstraintKind::Conversion: Out << " << "; break;
      case ConstraintKind::Literal:
        Out << " is an " << getLiteralKindName(getLiteralKind()) << " literal";
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

  /// \brief Describes a particular choice of a declaration from within a
  /// reference to an overloaded name.
  class OverloadChoice {
    /// \brief The overloaded expression from which we're making a selection.
    OverloadSetRefExpr *E;

    /// \brief The index into the list of candidates, which specifies which
    /// overload candidate this choice represents.
    unsigned Index;

    /// \brief The constraint system in which this overload choice was selected.
    ConstraintSystem *Constraints;

    /// \brief Overload choices are always allocated within a given constraint
    /// system.
    void *operator new(size_t) = delete;

  public:
    OverloadChoice(OverloadSetRefExpr *expr, unsigned index,
                   ConstraintSystem *constraints)
      : E(expr), Index(index), Constraints(constraints) { }

    /// \brief Retrieve the overloaded expression that this choice concerns.
    OverloadSetRefExpr *getExpr() const { return E; }

    /// \brief Retrieve the index into the overloaded expression that this
    /// choice represents.
    unsigned getIndex() const { return Index; }

    /// \brief Retrieve the declaraton that corresponds to this overload choice.
    ValueDecl *getDecl() const { return E->getDecls()[Index]; }

    /// \brief Retrieve the constraint system implied by this overload choice.
    ConstraintSystem *getConstraints() const { return Constraints; }

    void *operator new(size_t bytes, ConstraintSystem& cs,
                       size_t alignment = alignof(Constraint)) {
      return ::operator new (bytes, cs, alignment);
    }

    inline void operator delete(void *, const ConstraintSystem &cs, size_t) {}
  };

  /// \brief An overload set, which is a set of overloading choices from which
  /// only one can be selected.
  class OverloadSet {
    /// \brief The number of choices in the overload set.
    unsigned NumChoices;
    
    /// \brief Overload sets are always allocated within a given constraint
    /// system.
    void *operator new(size_t) = delete;

    OverloadSet(ArrayRef<OverloadChoice *> choices)
      : NumChoices(choices.size())
    {
      memmove(this+1, choices.data(), sizeof(OverloadChoice *)*choices.size());
    }

  public:
    /// \brief Retrieve the set of choices provided by this overload set.
    ArrayRef<OverloadChoice *> getChoices() const {
      return { reinterpret_cast<OverloadChoice * const *>(this + 1),
               NumChoices };
    }

    /// \brief Create a new overload set, using (and copying) the given choices.
    static OverloadSet *getNew(ConstraintSystem &CS,
                               ArrayRef<OverloadChoice *> choices);
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
    /// \brief Require the types to match exactly.
    SameType,
    /// \brief Require the first type to be a "trivial" subtype of the second
    /// type or be an exact match.
    TrivialSubtype,
    /// \brief Require the first type to be a subtype of the second type
    /// (or be an exact match or trivial subtype).
    Subtype,
    /// \brief Requires the first type to be convertible to the second type,
    /// which includes exact matches and both forms of subtyping.
    Conversion
  };

  /// \brief Describes a system of constraints on type variables, the
  /// solution of which assigns concrete types to each of the type variables.
  /// Constraint systems are typically generated given an (untyped) expression.
  class ConstraintSystem {
    TypeChecker &TC;
    ConstraintSystem *Parent;
    llvm::BumpPtrAllocator Allocator;
    unsigned TypeCounter;
    SmallVector<TypeVariableType *, 16> TypeVariables;
    SmallVector<Constraint *, 16> Constraints;
    SmallVector<OverloadSet *, 4> UnresolvedOverloadSets;
    SmallVector<std::unique_ptr<ConstraintSystem>, 2> Children;
    llvm::DenseMap<TypeVariableType *, Type> FixedTypes;

    unsigned assignTypeVariableID() {
      ConstraintSystem *topCS = this;
      while (topCS->Parent)
        topCS = topCS->Parent;
      return topCS->TypeCounter++;
    }

  public:
    ConstraintSystem(TypeChecker &TC, ConstraintSystem *Parent = nullptr)
      : TC(TC), Parent(Parent), TypeCounter(0) { }

    /// \brief Retrieve the type checker assciated 
    TypeChecker &getTypeChecker() const { return TC; }

    /// \brief Retrieve the AST context.
    ASTContext &getASTContext() const { return TC.Context; }

    /// \brief Create a new constraint system that is derived from this
    /// constraint system, referencing the rules of the parent system but
    /// also introducing its own (likely dependent) constraints.
    ConstraintSystem *createDerivedConstraintSystem() {
      auto result = new ConstraintSystem(TC, this);
      Children.push_back(std::unique_ptr<ConstraintSystem>(result));
      return result;
    }

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

    /// \brief "Open" the given type by replacing any occurrences of archetypes
    /// (including those implicit in unbound generic types) with fresh type
    /// variables.
    ///
    /// \param type The type to open.
    /// \returns The opened type, or \c type if there are no archetypes in it.
    Type openType(Type type);

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

    /// \brief Simplify a type, by replacing type variables with either their
    /// fixed types (if available) or their representatives.
    ///
    /// The resulting types can be compared canonically, so long as additional
    /// type equivalence requirements aren't introduced between comparisons.
    Type simplifyType(Type type);

    /// \brief Attempt to simplify the given literal constraint.
    SolutionKind simplifyLiteralConstraint(Type type, LiteralKind kind);

    /// \brief Attempt to simplify the given member constraint.
    SolutionKind simplifyMemberConstraint(Type baseTy, Identifier name,
                                          bool isTypeMember, Type memberTy);

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

    void dump();
  };
}

void *operator new(size_t bytes, ConstraintSystem& cs,
                   size_t alignment) {
  return cs.getAllocator().Allocate(bytes, alignment);
}

OverloadSet *OverloadSet::getNew(ConstraintSystem &CS,
                                 ArrayRef<OverloadChoice *> choices) {
  unsigned size = sizeof(OverloadSet)
                + sizeof(OverloadChoice *) * choices.size();
  void *mem = CS.getAllocator().Allocate(size, alignof(OverloadSet));
  return ::new (mem) OverloadSet(choices);
}

Type ConstraintSystem::openType(Type startingType) {
  llvm::DenseMap<ArchetypeType *, TypeVariableType *> replacements;

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

      // The type variable must be a subtype of the composition of all of
      // its protocol conformance requirements.
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
        CS.addConstraint(ConstraintKind::Subtype, tv, composition);
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
    if (auto archetype = dyn_cast<ArchetypeType>(type)) {
      return getTypeVariable(archetype);
    }

    // Create type variables for all of the archetypes in a polymorphic
    // function type.
    if (auto polyFn = dyn_cast<PolymorphicFunctionType>(type)) {
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

    // FIXME: Unbound generic types.
    return type;
  };
  
  return TC.transformType(startingType, replaceArchetypes);
}

Type ConstraintSystem::getTypeOfReference(ValueDecl *value) {
  // Determine the type of the value, opening up that type if necessary.
  return openType(value->getTypeOfReference());
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

  if (ownerTy->isSpecialized()) {
    // If the owner is specialized, we need to open up the types in the owner.
    // FIXME: Implement this
    llvm_unreachable("opening of generic types unimplemented");
  }

  // The base type must be a subtype of the owner type.
  addConstraint(ConstraintKind::Subtype, baseTy, ownerTy);

  // Determine the type of the member.
  // FIXME: Subscript references are special. Deal with them.
  Type type;
  if (isTypeReference)
    type = cast<TypeDecl>(value)->getDeclaredType();
  else
    type = value->getTypeOfReference();
  type = openType(type);

  // Skip the 'this' argument if it's already been bound by the base.
  if (auto func = dyn_cast<FuncDecl>(value)) {
    if (func->isStatic() || isInstance)
      type = type->castTo<AnyFunctionType>()->getResult();
  }
  return type;
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

void ConstraintSystem::generateConstraints(Expr *expr) {
  class ConstraintGenerator : public ExprVisitor<ConstraintGenerator, Type> {
    ConstraintSystem &CS;

  public:
    ConstraintGenerator(ConstraintSystem &CS) : CS(CS) { }

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
        CS.addConstraint(ConstraintKind::Conversion, segment->getType(), tv);
      }
      return tv;
    }

    Type visitDeclRefExpr(DeclRefExpr *E) {
      // FIXME: If the decl is in error, we get no information from this.
      // We may, alternatively, want to use a type variable in that case,
      // and possibly infer the type of the variable that way.
      return CS.getTypeOfReference(E->getDecl());
    }

    Type visitDotSyntaxBaseIgnoredExpr(DotSyntaxBaseIgnoredExpr *E) {
      llvm_unreachable("Already type-checked");
    }

    Type visitOverloadedDeclRefExpr(OverloadedDeclRefExpr *E) {
      // For a reference to an overloaded declaration, we create a type variable
      // that will be equal to different types in different child constraint
      // systems, one per overloaded declaration.
      auto tv = CS.createTypeVariable(E);
      ArrayRef<ValueDecl*> decls = E->getDecls();
      SmallVector<OverloadChoice *, 4> choices;
      for (unsigned i = 0, n = decls.size(); i != n; ++i) {
        // Create a new derived constraint system that captures the
        // additional requirements introduced by selecting this particular
        // overload.
        auto derivedCS = CS.createDerivedConstraintSystem();
        auto ovl = new (CS) OverloadChoice(E, i, derivedCS);
        choices.push_back(ovl);

        // Bind the type variable to the type of a reference to this declaration
        // within the derived constraint system.
        auto refTy = derivedCS->getTypeOfReference(decls[i]);
        derivedCS->addConstraint(ConstraintKind::Equal, refTy, tv);
      }

      // Record this overload set.
      CS.addOverloadSet(OverloadSet::getNew(CS, choices));
      return tv;
    }

    Type visitOverloadedMemberRefExpr(OverloadedMemberRefExpr *E) {
      // For a reference to an overloaded declaration, we create a type variable
      // that will be equal to different types in different child constraint
      // systems, one per overloaded declaration.
      // FIXME: Will this ever even happen? We'd need to resolve the 
      // base expression to a type before we ever get an overlaoded
      // member reference expression.
      auto tv = CS.createTypeVariable(E);
      ArrayRef<ValueDecl*> decls = E->getDecls();
      SmallVector<OverloadChoice *, 4> choices;
      auto baseTy = E->getBase()->getType();
      for (unsigned i = 0, n = decls.size(); i != n; ++i) {
        // Create a new derived constraint system that captures the
        // additional requirements introduced by selecting this particular
        // overload.
        auto derivedCS = CS.createDerivedConstraintSystem();
        auto ovl = new (CS) OverloadChoice(E, i, derivedCS);
        choices.push_back(ovl);

        auto decl = decls[i];

        // Bind the type variable to the type of a reference to this member
        // within the derived constraint system.
        auto refTy = derivedCS->getTypeOfMemberReference(baseTy, decl, false);
        derivedCS->addConstraint(ConstraintKind::Equal, refTy, tv);
      }
      
      // Record this overload set.
      CS.addOverloadSet(OverloadSet::getNew(CS, choices));
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
      auto tv = CS.createTypeVariable(expr);

      // The type must be a oneof type that has an element of the given name.
      // That named member has the type 'tv'.
      CS.addValueMemberConstraint(tv, expr->getName(), tv);
      return tv;
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
                                      LValueType::Qual::DefaultForMemberAccess,
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
      // Explicit closure expressions have function type (T1, T2, ..., TN) -> T0,
      // for fresh type variables Ti, 0 <= i <= N, where N is the number of
      // the largest closure argument (e.g., N=5 in an explicit closure { $5 }).
      unsigned n = expr->getParserVarDecls().size();
      SmallVector<TupleTypeElt, 4> tupleTypeElts;
      tupleTypeElts.reserve(n);
      for (unsigned i = 0; i != n; ++i) {
        tupleTypeElts.push_back(TupleTypeElt(CS.createTypeVariable(expr),
                                             Identifier()));
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
      return expr->getSubExpr()->getType();
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
      
      // The function argument has some type T1 -> T2 for fresh variables
      // T1 and T2.
      // FIXME: What if the function argument is a metatype?
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
      // Don't walk into the bodies of any function/closure expressions.
      if (isa<CapturingExpr>(expr)) {
        auto type = CG.visit(expr);
        expr->setType(type);
        return false;
      }
      
      return true;
    }

    /// \brief Once we've visited the children of the given expression,
    /// generate constraints from the expression.
    virtual Expr *walkToExprPost(Expr *expr) {
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

//===--------------------------------------------------------------------===//
// Constraint simplification
//===--------------------------------------------------------------------===//
#pragma mark Constraint simplification

ConstraintSystem::SolutionKind
ConstraintSystem::matchTupleTypes(TupleType *tuple1, TupleType *tuple2,
                                  TypeMatchKind kind, unsigned flags,
                                  bool &trivial) {
  // FIXME: When we're allowed to perform conversions, we can end up
  // shuffling here. Compute that shuffle.
  if (tuple1->getFields().size() != tuple2->getFields().size())
    return SolutionKind::Error;

  SolutionKind result = SolutionKind::TriviallySolved;
  unsigned subFlags = flags | TMF_GenerateConstraints;
  for (unsigned i = 0, n = tuple1->getFields().size(); i != n; ++i) {
    const auto &elt1 = tuple1->getFields()[i];
    const auto &elt2 = tuple2->getFields()[i];

    // FIXME: Totally wrong for conversions. We're assuming that they act
    // like subtypes.
    if (elt1.getName() != elt2.getName() &&
        !(kind >= TypeMatchKind::TrivialSubtype &&
          (elt1.getName().empty() || elt2.getName().empty()))) {
      return SolutionKind::Error;
    }

    if (elt1.isVararg() != elt2.isVararg())
      return SolutionKind::Error;

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
  case TypeMatchKind::SameType:
    return ConstraintKind::Equal;

  case TypeMatchKind::TrivialSubtype:
    return ConstraintKind::TrivialSubtype;

  case TypeMatchKind::Subtype:
    return ConstraintKind::Subtype;

  case TypeMatchKind::Conversion:
    return ConstraintKind::Conversion;
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
    case TypeMatchKind::SameType:
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
      if (typeVar1)
        assignFixedType(typeVar1, type2->getRValueType());
      else
        assignFixedType(typeVar2, type1->getRValueType());
      return SolutionKind::Solved;

    case TypeMatchKind::TrivialSubtype:
    case TypeMatchKind::Subtype:
    case TypeMatchKind::Conversion:
      if (flags & TMF_GenerateConstraints) {
        // Add a new constraint between these types. We consider the current
        // type-matching problem to the "solved" by this addition, because
        // this new constraint will be solved at a later point.
        // Obviously, this must not happen at the top level, or the algorithm
        // would not terminate.
        addConstraint(getConstraintKind(kind), type1, type2);
        return SolutionKind::Solved;
      }

      // We couldn't solve this constraint.
      return SolutionKind::Unsolved;
    }

    llvm_unreachable("Unhandled type-variable matching");
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
      // FIXME: subtyping for classes!
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
      // FIXME: Subtyping among metatypes, for classes only?
      return matchTypes(meta1->getInstanceType(), meta2->getInstanceType(),
                        TypeMatchKind::SameType, subFlags, trivial);
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
      
      // FIXME: subtyping for generic classes!
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
        // FIXME: If this fails, do we have to look for a subtype?
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

  // FIXME: Auto-closure.
  // FIXME: Materialization

  if (kind >= TypeMatchKind::Conversion) {
    // A scalar value can be converted to a non-empty tuple with at most one
    // non-defaulted element.
    if (auto tuple2 = type2->getAs<TupleType>()) {
      int scalarFieldIdx = tuple2->getFieldForScalarInit();
      if (scalarFieldIdx >= 0) {
        // FIXME: Handle variadic fields here.
        auto scalarFieldTy = tuple2->getElementType((unsigned)scalarFieldIdx);
        return matchTypes(type1, scalarFieldTy, kind, subFlags, trivial);
      }
    }

    // An lvalue of type T1 can be converted to a value of type T2 so long as
    // T1 is convertible to T2 (by loading the value).
    if (auto lvalue1 = type1->getAs<LValueType>()) {
      return matchTypes(lvalue1->getObjectType(), type2, kind, subFlags,
                        trivial);
    }
  }

  // FIXME: Check existential types.
  // FIXME: Subtyping for class types.
  // FIXME: User-defined conversions.

  return SolutionKind::Error;
}

Type ConstraintSystem::simplifyType(Type type) {
  return TC.transformType(type,
                          [&](Type type) -> Type {
            if (auto tvt = type->getAs<TypeVariableType>()) {
              tvt = tvt->getImpl().getRepresentative();
              if (auto fixed = getFixedType(tvt))
                return fixed;

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
           ? SolutionKind::Solved // FIXME: trivially solved
           : SolutionKind::Error;
}

ConstraintSystem::SolutionKind
ConstraintSystem::simplifyMemberConstraint(Type baseTy, Identifier name,
                                           bool isTypeMember, Type memberTy) {
  // Resolve the base type, if we can. If we can't resolve the base type,
  // then we can't solve this constraint.
  if (auto tv = baseTy->getAs<TypeVariableType>()) {
    auto fixed = getFixedType(tv);
    if (!fixed)
      return SolutionKind::Unsolved;

    // Continue with the fixed type.
    baseTy = fixed;
  }

  // If the base type is a tuple type, look for the named or indexed member
  // of the tuple.
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
  if (lookup.Results.size() == 1) {
    if (isTypeMember && !isa<TypeDecl>(lookup.Results[0].D))
      return SolutionKind::Error;
    
    auto refType = getTypeOfMemberReference(baseTy, lookup.Results[0].D,
                                            isTypeMember);
    addConstraint(ConstraintKind::Equal, refType, memberTy);
    return SolutionKind::Solved;
  }

  // We have multiple results.
  // FIXME: Implement this.
  llvm_unreachable("Overloaded member lookup results");
}

/// \brief Retrieve the type-matching kind corresponding to the given
/// constraint kind.
static TypeMatchKind getTypeMatchKind(ConstraintKind kind) {
  switch (kind) {
  case ConstraintKind::Equal: return TypeMatchKind::SameType;
  case ConstraintKind::TrivialSubtype: return TypeMatchKind::TrivialSubtype;
  case ConstraintKind::Subtype: return TypeMatchKind::Subtype;
  case ConstraintKind::Conversion: return TypeMatchKind::Conversion;
      
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
  case ConstraintKind::Equal:
  case ConstraintKind::TrivialSubtype:
  case ConstraintKind::Subtype:
  case ConstraintKind::Conversion: {
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
  case ConstraintKind::TypeMember: {
    bool isTypeMember = constraint.getKind() == ConstraintKind::TypeMember;
    return simplifyMemberConstraint(constraint.getFirstType(),
                                    constraint.getMember(),
                                    isTypeMember,
                                    constraint.getSecondType());
  }
  }
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
    case ConstraintKind::Equal:
    case ConstraintKind::TrivialSubtype:
    case ConstraintKind::Subtype:
    case ConstraintKind::Conversion:
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

  /// \brief Maintains the set of type variable constraints
  struct RelationalTypeVarConstraints {
    RelationalTypeVarConstraints() : Below(), Above() { }
    
    Constraint *Below;
    Constraint *Above;
  };
}

bool
ConstraintSystem::simplifyTypeVarConstraints(TypeVariableConstraints &tvc,
                                         SmallPtrSet<Constraint *, 4> &solved) {
  bool addedConstraints = false;
  bool foundEquality = false;

  // First, sort the constraints so that we can visit the various kinds of
  // constraints in the order we want to.
  std::stable_sort(tvc.Constraints.begin(), tvc.Constraints.end(),
                   OrderConstraintForTypeVariable());

  // Collect the relational constraints above and below each simplified type,
  // dropping any redundant constraints in the process.
  llvm::DenseMap<CanType, RelationalTypeVarConstraints> typeVarConstraints;
  SmallVector<std::pair<Type, Constraint *>, 4> typesBelow;
  SmallVector<std::pair<Type, Constraint *>, 4> typesAbove;
  auto currentConstraint = tvc.Constraints.begin(),
          lastConstraint = tvc.Constraints.end();
  for (; currentConstraint != lastConstraint &&
         (*currentConstraint)->getClassification()
           == ConstraintClassification::Relational;
       ++currentConstraint) {
    // Determine whether we have a constraint below (vs. a constraint above)
    // this type variable.
    bool constraintIsBelow = false;
    if (auto secondTV = (*currentConstraint)->getSecondType()
                           ->getAs<TypeVariableType>()) {
      constraintIsBelow = (secondTV->getImpl().getRepresentative()
                             == tvc.TypeVar);
    }

    // Compute the type bound, and simplify it.
    Type bound = constraintIsBelow? (*currentConstraint)->getFirstType()
                                  : (*currentConstraint)->getSecondType();
    bound = simplifyType(bound);

    Constraint *(RelationalTypeVarConstraints::*which)
      = constraintIsBelow? &RelationalTypeVarConstraints::Below
                         : &RelationalTypeVarConstraints::Above;
    auto &typeVarConstraint = typeVarConstraints[bound->getCanonicalType()];

    if (typeVarConstraint.*which) {
      // We already have a constraint that provides either the same relation
      // or a tighter relation, because the sort orders relational constraints
      // in order of nonincreasing strictness.
      assert(((typeVarConstraint.*which)->getKind()
                < (*currentConstraint)->getKind()) &&
             "Improper ordering of constraints");
      solved.insert(*currentConstraint);
      continue;
    }

    // Record this constraint.
    typeVarConstraint.*which = *currentConstraint;
    if (constraintIsBelow)
      typesBelow.push_back(std::make_pair(bound, *currentConstraint));
    else
      typesAbove.push_back(std::make_pair(bound, *currentConstraint));

    // If a type variable T is bounded both above and below by a nominal type X
    // (or specialization of one), then we can conclude that X == T, e.g.,
    //
    //   X < T and T < X => T == X
    //
    // Note that this rule does not apply in general because tuple types
    // allow one to add or drop labels essentially at will, so there are
    // multiple types T for which X < T and T < X. With nominal types,
    // we prohibit cyclic conversions and subtyping relationships.
    if (typeVarConstraint.Below && typeVarConstraint.Above &&
        (bound->is<NominalType>() || bound->is<BoundGenericType>())) {
      addConstraint(ConstraintKind::Equal, tvc.TypeVar, bound);
      foundEquality = true;
      addedConstraints = true;
    }
  }

  // If we determined an equality constraint, all of the relational constraints
  // are now solved.
  if (foundEquality) {
    for (auto rtv : typeVarConstraints) {
      if (rtv.second.Above)
        solved.insert(rtv.second.Above);
      if (rtv.second.Below)
        solved.insert(rtv.second.Below);
    }
  }
  typeVarConstraints.clear();

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

  // We've solved all of the constraints we can.
  return false;
}

//===--------------------------------------------------------------------===//
// Debugging
//===--------------------------------------------------------------------===//
#pragma mark Debugging

void ConstraintSystem::dump() {
  llvm::raw_ostream &out = llvm::errs();

  out << "Type Variables:\n";
  for (auto tv : TypeVariables) {
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

  out << "\nConstraints:\n";
  for (auto constraint : Constraints) {
    out.indent(2);
    constraint->print(out);
    out << "\n";
  }
}

void TypeChecker::dumpConstraints(Expr *expr) {
  ConstraintSystem CS(*this);
  llvm::errs() << "---Initial constraints for the given expression---\n";
  expr->dump();
  CS.generateConstraints(expr);
  llvm::errs() << "\n";
  CS.dump();
  llvm::errs() << "---Simplified constraints---\n";
  bool error = CS.simplify();
  CS.dump();

  llvm::errs() << "\nConstraint system is "
               << (error? "ill-formed" : "still viable")
               << "\n";
}


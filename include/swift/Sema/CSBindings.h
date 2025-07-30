//===--- ConstraintGraph.h - Constraint Graph -------------------*- C++ -*-===//
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
//
// This file defines the \c PotentialBindings class and its auxiliary types
// such as \c PotentialBinding, that are used to describe bindings which
// a particular type variable could be bound to.
//
//===----------------------------------------------------------------------===//
#ifndef SWIFT_SEMA_CSBINDINGS_H
#define SWIFT_SEMA_CSBINDINGS_H

#include "swift/AST/ASTNode.h"
#include "swift/AST/Type.h"
#include "swift/AST/Types.h"
#include "swift/Basic/Debug.h"
#include "swift/Basic/LLVM.h"
#include "swift/Basic/LLVMExtras.h"
#include "swift/Sema/Constraint.h"
#include "swift/Sema/ConstraintLocator.h"
#include "llvm/ADT/APInt.h"
#include "llvm/ADT/DenseMap.h"
#include "llvm/ADT/MapVector.h"
#include "llvm/ADT/PointerUnion.h"
#include "llvm/ADT/SetVector.h"
#include "llvm/ADT/SmallPtrSet.h"
#include "llvm/ADT/SmallVector.h"
#include "llvm/ADT/TinyPtrVector.h"
#include "llvm/Support/raw_ostream.h"
#include <tuple>

namespace swift {

class DeclContext;
enum class KnownProtocolKind : uint8_t;
class ProtocolDecl;

namespace constraints {

class ConstraintSystem;

namespace inference {

/// The kind of bindings that are permitted.
enum class AllowedBindingKind : uint8_t {
  /// Only the exact type.
  Exact,
  /// Supertypes of the specified type.
  Supertypes,
  /// Subtypes of the specified type.
  Subtypes
};

/// The kind of literal binding found.
enum class LiteralBindingKind : uint8_t {
  None,
  Collection,
  Float,
  Atom,
};

/// A potential binding from the type variable to a particular type,
/// along with information that can be used to construct related
/// bindings, e.g., the supertypes of a given type.
struct PotentialBinding {
  /// The type to which the type variable can be bound.
  Type BindingType;

  /// The kind of bindings permitted.
  AllowedBindingKind Kind;

  /// The source of the type information.
  ///
  /// Determines whether this binding represents a "hole" in
  /// constraint system. Such bindings have no originating constraint
  /// because they are synthetic, they have a locator instead.
  PointerUnion<Constraint *, ConstraintLocator *> BindingSource;

  PotentialBinding(Type type, AllowedBindingKind kind,
                   PointerUnion<Constraint *, ConstraintLocator *> source)
      : BindingType(type), Kind(kind), BindingSource(source) {}

  PotentialBinding(Type type, AllowedBindingKind kind, Constraint *source)
      : PotentialBinding(
            type, kind,
            PointerUnion<Constraint *, ConstraintLocator *>(source)) {}

  bool isDefaultableBinding() const {
    if (auto *constraint = BindingSource.dyn_cast<Constraint *>())
      return constraint->getKind() == ConstraintKind::Defaultable;
    // If binding source is not constraint - it's a hole, which is
    // a last resort default binding for a type variable.
    return true;
  }

  bool hasDefaultedLiteralProtocol() const {
    return bool(getDefaultedLiteralProtocol());
  }

  ProtocolDecl *getDefaultedLiteralProtocol() const {
    auto *constraint = BindingSource.dyn_cast<Constraint *>();
    if (!constraint)
      return nullptr;

    return constraint->getKind() == ConstraintKind::LiteralConformsTo
               ? constraint->getProtocol()
               : nullptr;
  }

  ConstraintLocator *getLocator() const {
    if (auto *constraint = BindingSource.dyn_cast<Constraint *>())
      return constraint->getLocator();
    return cast<ConstraintLocator *>(BindingSource);
  }

  Constraint *getSource() const { return cast<Constraint *>(BindingSource); }

  PotentialBinding withType(Type type) const {
    return {type, Kind, BindingSource};
  }

  PotentialBinding withSameSource(Type type, AllowedBindingKind kind) const {
    return {type, kind, BindingSource};
  }

  /// Determine whether this binding could be a viable candidate
  /// to be "joined" with some other binding. It has to be at least
  /// a non-default r-value supertype binding with no type variables.
  bool isViableForJoin() const;

  static PotentialBinding forHole(TypeVariableType *typeVar,
                                  ConstraintLocator *locator) {
    return {PlaceholderType::get(typeVar->getASTContext(), typeVar),
            AllowedBindingKind::Exact,
            /*source=*/locator};
  }

  static PotentialBinding forPlaceholder(Type placeholderTy) {
    return {placeholderTy, AllowedBindingKind::Exact,
            PointerUnion<Constraint *, ConstraintLocator *>()};
  }
};

struct LiteralRequirement {
  /// The source of the literal requirement.
  Constraint *Source;
  /// The default type associated with this literal (if any).
  Type DefaultType;
  /// Determines whether this literal is a direct requirement
  /// of the current type variable.
  bool IsDirectRequirement;

  /// If the literal is covered by existing type binding,
  /// this points to the source of the binding.
  mutable Constraint *CoveredBy = nullptr;

  LiteralRequirement(Constraint *source, Type defaultTy, bool isDirect)
      : Source(source), DefaultType(defaultTy), IsDirectRequirement(isDirect) {}

  Constraint *getSource() const { return Source; }

  ProtocolDecl *getProtocol() const { return Source->getProtocol(); }

  bool isCovered() const { return bool(CoveredBy); }

  bool isDirectRequirement() const { return IsDirectRequirement; }

  bool hasDefaultType() const { return bool(DefaultType); }

  Type getDefaultType() const {
    assert(hasDefaultType());
    return DefaultType;
  }

  void setCoveredBy(Constraint *coveredBy) {
    assert(!isCovered());
    CoveredBy = coveredBy;
  }

  /// Determines whether this literal requirement is "covered"
  /// by the given binding - type of the binding could either be
  /// equal (in canonical sense) to the protocol's default type,
  /// or conform to a protocol.
  ///
  /// \param binding The binding to check for coverage.
  ///
  /// \param canBeNil The flag that determines whether given type
  /// variable requires all of its bindings to be optional.
  ///
  /// \param CS The constraint system this literal requirement belongs to.
  ///
  /// \returns a pair of bool and a type:
  ///    - bool, true if binding covers given literal protocol;
  ///    - type, non-null if binding type has to be adjusted
  ///      to cover given literal protocol;
  std::pair<bool, Type> isCoveredBy(const PotentialBinding &binding,
                                    bool canBeNil,
                                    ConstraintSystem &CS) const;

  /// Determines whether literal protocol associated with this
  /// meta-information is viable for inclusion as a defaultable binding.
  bool viableAsBinding() const { return !isCovered() && hasDefaultType(); }

private:
  bool isCoveredBy(Type type, ConstraintSystem &CS) const;
};

struct PotentialBindings {
  /// The set of all constraints that have been added via infer().
  llvm::SmallSetVector<Constraint *, 2> Constraints;

  /// The set of potential bindings.
  llvm::SmallVector<PotentialBinding, 4> Bindings;

  /// The set of constraints which delay attempting this type variable.
  llvm::TinyPtrVector<Constraint *> DelayedBy;

  /// The set of type variables adjacent to the current one.
  ///
  /// Type variables contained here are either related through the
  /// bindings (contained in the binding type e.g. `Foo<$T0>`), or
  /// reachable through subtype/conversion  relationship e.g.
  /// `$T0 subtype of $T1` or `$T0 arg conversion $T1`.
  llvm::SmallVector<std::pair<TypeVariableType *, Constraint *>, 2> AdjacentVars;

  /// A set of all not-yet-resolved type variables this type variable
  /// is a subtype of, supertype of or is equivalent to. This is used
  /// to determine ordering inside of a chain of subtypes to help infer
  /// transitive bindings  and protocol requirements.
  llvm::SmallVector<std::pair<TypeVariableType *, Constraint *>, 4> SubtypeOf;
  llvm::SmallVector<std::pair<TypeVariableType *, Constraint *>, 4> SupertypeOf;
  llvm::SmallVector<std::pair<TypeVariableType *, Constraint *>, 4> EquivalentTo;

  ASTNode AssociatedCodeCompletionToken = ASTNode();

  /// Add a potential binding to the list of bindings,
  /// coalescing supertype bounds when we are able to compute the meet.
  void addPotentialBinding(TypeVariableType *typeVar, PotentialBinding binding);

  bool isSubtypeOf(TypeVariableType *typeVar) const {
    return llvm::any_of(
        SubtypeOf,
        [&typeVar](const std::pair<TypeVariableType *, Constraint *> &subtype) {
          return subtype.first == typeVar &&
                 subtype.second->getKind() == ConstraintKind::Subtype;
        });
  }

private:
  /// Attempt to infer a new binding and other useful information
  /// (i.e. whether bindings should be delayed) from the given
  /// relational constraint.
  std::optional<PotentialBinding> inferFromRelational(
      ConstraintSystem &CS,
      TypeVariableType *TypeVar,
      Constraint *constraint);

public:
  void infer(ConstraintSystem &CS,
             TypeVariableType *TypeVar,
             Constraint *constraint);

  /// Retract all bindings and other information related to a given
  /// constraint from this binding set.
  ///
  /// This would happen when constraint is simplified or solver backtracks
  /// (either from overload choice or (some) type variable binding).
  void retract(ConstraintSystem &CS,
               TypeVariableType *TypeVar,
               Constraint *constraint);

  void reset();

  void dump(ConstraintSystem &CS,
            TypeVariableType *TypeVar,
            llvm::raw_ostream &out,
            unsigned indent) const;
};


} // end namespace inference

} // end namespace constraints

} // end namespace swift

namespace llvm {

template <>
struct DenseMapInfo<swift::constraints::inference::PotentialBinding> {
  using Binding = swift::constraints::inference::PotentialBinding;

  static Binding getEmptyKey() {
    return placeholderKey(llvm::DenseMapInfo<swift::TypeBase *>::getEmptyKey());
  }

  static Binding getTombstoneKey() {
    return placeholderKey(
        llvm::DenseMapInfo<swift::TypeBase *>::getTombstoneKey());
  }

  static unsigned getHashValue(const Binding &Val) {
    return DenseMapInfo<swift::Type>::getHashValue(
        Val.BindingType->getCanonicalType());
  }

  static bool isEqual(const Binding &LHS, const Binding &RHS) {
    auto lhsTy = LHS.BindingType.getPointer();
    auto rhsTy = RHS.BindingType.getPointer();

    // Fast path: pointer equality.
    if (DenseMapInfo<swift::TypeBase *>::isEqual(lhsTy, rhsTy))
      return true;

    // If either side is empty or tombstone, let's use pointer equality.
    {
      auto emptyTy = llvm::DenseMapInfo<swift::TypeBase *>::getEmptyKey();
      auto tombstoneTy =
          llvm::DenseMapInfo<swift::TypeBase *>::getTombstoneKey();

      if (lhsTy == emptyTy || lhsTy == tombstoneTy)
        return lhsTy == rhsTy;

      if (rhsTy == emptyTy || rhsTy == tombstoneTy)
        return lhsTy == rhsTy;
    }

    // Otherwise let's drop the sugar and check.
    return LHS.BindingType->isEqual(RHS.BindingType);
  }

private:
  static Binding placeholderKey(swift::Type type) {
    return Binding::forPlaceholder(type);
  }
};

} // end namespace llvm

namespace swift {
namespace constraints {
namespace inference {
class BindingSet {
  using BindingScore =
      std::tuple<bool, bool, bool, bool, bool, unsigned char, int>;

  ConstraintSystem &CS;

  TypeVariableType *TypeVar;

  const PotentialBindings &Info;

  llvm::SmallPtrSet<TypeVariableType *, 4> AdjacentVars;

public:
  swift::SmallSetVector<PotentialBinding, 4> Bindings;

  /// The set of protocol conformance requirements placed on this type variable.
  llvm::SmallVector<Constraint *, 4> Protocols;

  /// The set of unique literal protocol requirements placed on this
  /// type variable or inferred transitively through subtype chains.
  ///
  /// Note that ordering is important when it comes to bindings, we'd
  /// like to add any "direct" default types first to attempt them
  /// before transitive ones.
  llvm::SmallMapVector<ProtocolDecl *, LiteralRequirement, 2> Literals;

  llvm::SmallDenseMap<CanType, Constraint *, 2> Defaults;

  /// The set of transitive protocol requirements inferred through
  /// subtype/conversion/equivalence relations with other type variables.
  std::optional<llvm::SmallPtrSet<Constraint *, 4>> TransitiveProtocols;

  BindingSet(ConstraintSystem &CS, TypeVariableType *TypeVar,
             const PotentialBindings &info);

  ConstraintSystem &getConstraintSystem() const { return CS; }

  TypeVariableType *getTypeVariable() const { return TypeVar; }

  /// Check whether this binding set belongs to a type variable
  /// that represents a result type of a closure.
  bool forClosureResult() const;

  /// Check whether this binding set belongs to a type variable
  /// that represents a generic parameter.
  bool forGenericParameter() const;

  bool canBeNil() const;

  /// If this type variable doesn't have any viable bindings, or
  /// if there is only one binding and it's a placeholder type, consider
  /// this type variable to be a hole in a constraint system
  /// regardless of where the placeholder type originated.
  bool isHole() const {
    if (isDirectHole())
      return true;

    if (Bindings.size() != 1)
      return false;

    const auto &binding = Bindings.front();
    return binding.BindingType->is<PlaceholderType>();
  }

  /// Determines whether the only possible binding for this type variable
  /// would be a placeholder type. This is different from `isHole` method
  /// because type variable could also acquire a placeholder type transitively
  /// if one of the type variables in its subtype/equivalence chain has been
  /// bound to a placeholder type.
  bool isDirectHole() const;

  /// Determine whether attempting this type variable should be
  /// delayed until the rest of the constraint system is considered
  /// "fully bound" meaning constraints, which affect completeness
  /// of the binding set, for this type variable such as - member
  /// constraint, disjunction, function application etc. - are simplified.
  ///
  /// Note that in some situations i.e. when there are no more
  /// disjunctions or type variables left to attempt, it's still
  /// okay to attempt "delayed" type variable to make forward progress.
  bool isDelayed() const;

  /// Whether the bindings of this type involve other type variables,
  /// or the type variable itself is adjacent to other type variables
  /// that could become valid bindings in the future.
  bool involvesTypeVariables() const;

  /// Whether the bindings represent (potentially) incomplete set,
  /// there is no way to say with absolute certainty if that's the
  /// case, but that could happen when certain constraints like
  /// `bind param` are present in the system.
  bool isPotentiallyIncomplete() const;

  /// Determine if the bindings only constrain the type variable from above
  /// with an existential type; such a binding is not very helpful because
  /// it's impossible to enumerate the existential type's subtypes.
  bool isSubtypeOfExistentialType() const {
    if (Bindings.empty())
      return false;

    // Literal requirements always result in a subtype/supertype
    // relationship to a concrete type.
    if (llvm::any_of(Literals, [](const auto &literal) {
          return literal.second.viableAsBinding();
        }))
      return false;

    return llvm::all_of(Bindings, [](const PotentialBinding &binding) {
      return binding.BindingType->isExistentialType() &&
             binding.Kind == AllowedBindingKind::Subtypes;
    });
  }

  /// Check if this binding is viable for inclusion in the set.
  ///
  /// \param binding The binding to validate.
  /// \param isTransitive Indicates whether this binding has been
  /// acquired through transitive inference and requires extra
  /// checking.
  bool isViable(PotentialBinding &binding, bool isTransitive);

  explicit operator bool() const {
    return hasViableBindings() || isDirectHole();
  }

  /// Determine whether this set has any "viable" (or non-hole) bindings.
  ///
  /// A viable binding could be - a direct or transitive binding
  /// inferred from a constraint, literal binding, or defaultable
  /// binding.
  ///
  /// A hole is not considered a viable binding since it doesn't
  /// add any new type information to constraint system.
  bool hasViableBindings() const {
    return !Bindings.empty() || getNumViableLiteralBindings() > 0 ||
           !Defaults.empty();
  }

  ArrayRef<Constraint *> getConformanceRequirements() const {
    return Protocols;
  }

  unsigned getNumViableLiteralBindings() const;

  unsigned getNumViableDefaultableBindings() const {
    if (isDirectHole())
      return 1;

    auto numDefaultable = llvm::count_if(
        Defaults, [](const std::pair<CanType, Constraint *> &entry) {
          return entry.second->getKind() == ConstraintKind::Defaultable;
        });

    // Short-circuit unviable checks if there are no defaultable bindings.
    if (numDefaultable == 0)
      return 0;

    // Defaultable constraint is unviable if its type is covered by
    // an existing direct or transitive binding.
    auto unviable =
        llvm::count_if(Bindings, [&](const PotentialBinding &binding) {
          auto type = binding.BindingType->getCanonicalType();
          auto def = Defaults.find(type);
          return def != Defaults.end()
                     ? def->second->getKind() == ConstraintKind::Defaultable
                     : false;
        });

    assert(numDefaultable >= unviable);
    return numDefaultable - unviable;
  }

  ASTNode getAssociatedCodeCompletionToken() const {
    return Info.AssociatedCodeCompletionToken;
  }

  void forEachLiteralRequirement(
      llvm::function_ref<void(KnownProtocolKind)> callback) const;

  void forEachAdjacentVariable(
      llvm::function_ref<void(TypeVariableType *)> callback) const {
    for (auto *typeVar : AdjacentVars)
      callback(typeVar);
  }

  /// Return a literal requirement that has the most impact on the binding
  /// score.
  LiteralBindingKind getLiteralForScore() const;

  /// Check if this binding is favored over a disjunction e.g.
  /// if it has only concrete types or would resolve a closure.
  bool favoredOverDisjunction(Constraint *disjunction) const;

  /// Check if this binding is favored over a conjunction.
  bool favoredOverConjunction(Constraint *conjunction) const;

  /// Detect `subtype` relationship between two type variables and
  /// attempt to infer supertype bindings transitively e.g.
  ///
  /// Given A <: T1 <: T2 transitively A <: T2
  ///
  /// Which gives us a new (superclass A) binding for T2 as well as T1.
  ///
  /// \param inferredBindings The set of all bindings inferred for type
  /// variables in the workset.
  void inferTransitiveBindings();

  /// Detect subtype, conversion or equivalence relationship
  /// between two type variables and attempt to propagate protocol
  /// requirements down the subtype or equivalence chain.
  void inferTransitiveProtocolRequirements();

  /// Finalize binding computation for this type variable by
  /// inferring bindings from context e.g. transitive bindings.
  ///
  /// \returns true if finalization successful (which makes binding set viable),
  /// and false otherwise.
  bool finalize(bool transitive);

  static BindingScore formBindingScore(const BindingSet &b);

  bool operator==(const BindingSet &other);

  /// Compare two sets of bindings, where \c this < other indicates that
  /// \c this is a better set of bindings that \c other.
  bool operator<(const BindingSet &other);

  void dump(llvm::raw_ostream &out, unsigned indent) const;

private:
  /// Add a new binding to the set.
  ///
  /// \param binding The binding to add.
  /// \param isTransitive Indicates whether this binding has been
  /// acquired through transitive inference and requires validity
  /// checking.
  void addBinding(PotentialBinding binding, bool isTransitive);

  void addLiteralRequirement(Constraint *literal);

  void addDefault(Constraint *constraint);

  /// Check whether the given binding set covers any of the
  /// literal protocols associated with this type variable.
  void determineLiteralCoverage();

  StringRef getLiteralBindingKind(LiteralBindingKind K) const {
#define ENTRY(Kind, String)                                                    \
  case LiteralBindingKind::Kind:                                               \
    return String
    switch (K) {
      ENTRY(None, "none");
      ENTRY(Collection, "collection");
      ENTRY(Float, "float");
      ENTRY(Atom, "atom");
    }
#undef ENTRY
  }
};
} // namespace inference
} // namespace constraints
} // namespace swift

#endif // SWIFT_SEMA_CSBINDINGS_H

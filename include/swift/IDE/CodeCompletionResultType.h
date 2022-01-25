//===--- CodeCompletionResultType.h -----------------------------*- C++ -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2022 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_IDE_CODECOMPLETIONRESULTTYPE_H
#define SWIFT_IDE_CODECOMPLETIONRESULTTYPE_H

#include "swift/AST/Types.h"
#include "swift/Basic/LLVM.h"
#include "llvm/ADT/PointerUnion.h"
#include "llvm/ADT/StringMap.h"

namespace swift {
namespace ide {

/// The expected contextual type(s) for code-completion.
struct ExpectedTypeContext {
  /// Possible types of the code completion expression.
  llvm::SmallVector<Type, 4> possibleTypes;

  /// Pre typechecked type of the expression at the completion position.
  Type idealType;

  /// Whether the `ExpectedTypes` comes from a single-expression body, e.g.
  /// `foo({ here })`.
  ///
  /// Since the input may be incomplete, we take into account that the types are
  /// only a hint.
  bool isImplicitSingleExpressionReturn = false;
  bool preferNonVoid = false;

  bool empty() const { return possibleTypes.empty(); }
  bool requiresNonVoid() const {
    if (isImplicitSingleExpressionReturn)
      return false;
    if (preferNonVoid)
      return true;
    if (possibleTypes.empty())
      return false;
    return llvm::all_of(possibleTypes, [](Type Ty) { return !Ty->isVoid(); });
  }

  ExpectedTypeContext() = default;
};

/// Describes the relationship between the type of the completion results and
/// the expected type at the code completion position.
enum class CodeCompletionResultTypeRelation : uint8_t {
  /// The result does not have a type (e.g. keyword).
  NotApplicable,

  /// The type relation have not been calculated.
  Unknown,

  /// The relationship of the result's type to the expected type is not
  /// invalid, not convertible, and not identical.
  Unrelated,

  /// The result's type is invalid at the expected position.
  Invalid,

  /// The result's type is convertible to the type of the expected.
  Convertible,

  /// The result's type is identical to the type of the expected.
  Identical,

  MAX_VALUE = Identical
};

class USRBasedType;

/// Holds \c USRBasedTypes. Two \c USRBasedTypes with the same USR are
/// represented by the same object if they live in the same arena.
class USRBasedTypeArena {
  friend class USRBasedType;

  /// The allocator allocating the \c USRBasedTypes
  llvm::BumpPtrAllocator Allocator;

  /// Maps USRs to their \c USRBasedType instances.
  llvm::StringMap<USRBasedType *> CanonicalTypes;

  /// Cache of the \c Void type because its frequently needed to compute type
  /// relations.
  USRBasedType *VoidType = nullptr;

public:
  /// Return the USR-based \c Void type. \p ASTCtx is needed to determine the
  /// USR of the \c Void type.
  USRBasedType *getVoidType(const ASTContext &ASTCtx);

  USRBasedTypeArena() {}
};

/// The equivalent of a \c ExpectedTypeContext to compute the type relation of
/// \c USRBasedTypes.
struct USRBasedTypeContext {
  struct ContextualType {
    /// The types that the result type must be identical/convertible to to
    /// compute the type relation. Needs to be a vector because for the
    /// conextual type `some MyProto & MyOtherProto`, the return type must be
    /// convertible to both \c MyProto and \c MyOtherProto to be considered
    /// convertible.
    llvm::SmallVector<USRBasedType *, 1> Types;

    /// Whether a match against this type should be considered convertible
    /// instead of identical. For optional context types, we also add the
    /// non-optional types as contextual types, but only consider them
    /// convertible.
    bool IsConvertible;

    /// Type relation of \p ResultType to this conextual type.
    CodeCompletionResultTypeRelation
    typeRelation(const USRBasedType *ResultType,
                 const USRBasedType *VoidType) const;

    ContextualType(USRBasedType *Ty, bool IsConvertible)
        : Types({Ty}), IsConvertible(IsConvertible) {}
    ContextualType(ArrayRef<USRBasedType *> Types, bool IsConvertible)
        : Types(Types.begin(), Types.end()), IsConvertible(IsConvertible) {
      assert(!Types.empty() && "ContextualType should have at least one type");
    }
  };

  /// Cache of the \c Void type because its regularly used in type relation
  /// computation.
  USRBasedType *VoidType;

  SmallVector<ContextualType, 4> ContextualTypes;

  USRBasedTypeContext(const ExpectedTypeContext &TypeContext,
                      const DeclContext *DC, USRBasedTypeArena &Arena);

  /// Type relation of \p ResultType to this context.
  CodeCompletionResultTypeRelation
  typeRelation(const USRBasedType *ResultType) const;
};

/// A type that is purely USR-based and thus independent of an \c ASTContext.
/// Its identity is defined by its USR. The object also stores the USRs of all
/// its known supertypes (its superclasses and protocol conformances declared
/// within the same module) so it is able to determine whether one type is
/// convertible to another
/// Because the type is purely based on USRs, it can be serialized into the code
/// completion cache and read from it later.
/// \c USRBasedTypes are always allocated within a \c USRBasedTypeArena. Types
/// in the same arena that have the same USR are represented by the same object.
/// \c USRBasedTypes always represent canonical types so that equivalent types
/// are represented by the same USR.
/// A type with an empty USR represents the null type.
class USRBasedType {
  std::string USR;
  /// The superclasses of the type and all protocol conformances that are
  /// declared in the same module that the type was defined.
  std::vector<USRBasedType *> Supertypes;

  /// Memberwise initializer
  USRBasedType(StringRef USR, ArrayRef<USRBasedType *> Supertypes)
      : USR(USR), Supertypes(Supertypes.begin(), Supertypes.end()) {}

public:
  /// A null \c USRBasedType that's represented by an empty USR and has no
  /// supertypes.
  static USRBasedType *null(USRBasedTypeArena &Arena);

  /// Construct a \c USRBasedType from an AST-bound type. Computes the type's
  /// supertypes.
  static USRBasedType *fromType(Type Ty, USRBasedTypeArena &Arena);

  /// Construct as \c USRBasedType from a USR and its supertypes.
  static USRBasedType *fromUSR(StringRef USR,
                               ArrayRef<USRBasedType *> Supertypes,
                               USRBasedTypeArena &Arena);

  StringRef getUSR() const { return USR; }
  ArrayRef<USRBasedType *> getSupertypes() const { return Supertypes; }

  /// Compute the relation of \c ResultType when to this contextual type.
  CodeCompletionResultTypeRelation
  typeRelation(const USRBasedType *ResultType,
               const USRBasedType *VoidType) const;
};

/// The type returned by a \c ContextFreeCodeCompletionResult. Can be either of
/// the following:
///  - NotApplicable: The completion result doesn't produce something that's
///    valid inside an expression like a keyword
///  - An empty list of types if the completion result produces something that's
///    valid inside an expression but the result type isn't known
///  - A list of proper type if the type produced by this completion result is
///    known
/// A \c CodeCompletionResultType does not have a single unique type because for
/// code completion we consider e.g. the expression \c Int as producing both an
/// \c Int metatype and an \c Int instance type. Thus we consider both the
/// instance and the metatype as result types of the expression \c Int.
class CodeCompletionResultType {
  bool IsNotApplicable;

  /// If not \c nullptr, \c ResultTypes contains \c USRBasedTypes that are
  /// allocated in this arena.
  USRBasedTypeArena *USRArena = nullptr;

  /// If \c USRArena is \c nullptr, all elements are \c Types.
  /// If \c USRArena is not a \c nullptr, all elements are \c USRBasedTypes.
  SmallVector<PointerUnion<Type, USRBasedType *>, 2> ResultTypes;

  /// Private to make instantiation of 'notApplicable' result types explicit via
  /// a static function.
  CodeCompletionResultType() : IsNotApplicable(true), ResultTypes() {}

public:
  static CodeCompletionResultType notApplicable() {
    return CodeCompletionResultType();
  }

  static CodeCompletionResultType unknown() {
    return CodeCompletionResultType(ArrayRef<Type>());
  }

  CodeCompletionResultType(ArrayRef<Type> Types)
      : IsNotApplicable(false), ResultTypes(Types.begin(), Types.end()) {}

  CodeCompletionResultType(Type Ty)
      : CodeCompletionResultType(ArrayRef<Type>(Ty)) {}

  /// \p USRArena is the arena in which the \p Types are allocated
  CodeCompletionResultType(ArrayRef<USRBasedType *> Types,
                           USRBasedTypeArena &USRArena)
      : IsNotApplicable(false), USRArena(&USRArena),
        ResultTypes(Types.begin(), Types.end()) {}

  bool isNotApplicable() const { return IsNotApplicable; }

  /// Return the result types as \c USRBasedTypes, converting AST-bound types
  /// to \c USRBasedTypes if necessary.
  llvm::SmallVector<USRBasedType *, 2>
  getUSRBasedResultTypes(USRBasedTypeArena &Arena) const;

  /// Calculates the type relation of this type to the given type context. If a
  /// \c USRBasedTypeContext has already been computed, it can be passed to the
  /// function too speed up the computation, otherwise the
  /// \c USRBasedTypeContext will be computed on demand.
  CodeCompletionResultTypeRelation
  calculateTypeRelation(const ExpectedTypeContext &TypeContext,
                        const DeclContext *DC,
                        USRBasedTypeContext *USRTypeContext = nullptr) const;
};
} // namespace ide
} // namespace swift

#endif // SWIFT_IDE_CODECOMPLETIONRESULTTYPE_H

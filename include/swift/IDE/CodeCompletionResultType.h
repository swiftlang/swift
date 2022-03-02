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

namespace swift {
namespace ide {

/// The expected contextual type(s) for code-completion.
class ExpectedTypeContext {
  /// Possible types of the code completion expression.
  llvm::SmallVector<Type, 4> PossibleTypes;

  /// Pre typechecked type of the expression at the completion position.
  Type IdealType;

  /// Whether the `ExpectedTypes` comes from a single-expression body, e.g.
  /// `foo({ here })`.
  ///
  /// Since the input may be incomplete, we take into account that the types are
  /// only a hint.
  bool IsImplicitSingleExpressionReturn = false;
  bool PreferNonVoid = false;

public:
  ExpectedTypeContext() = default;

  bool empty() const { return PossibleTypes.empty(); }

  ArrayRef<Type> getPossibleTypes() const { return PossibleTypes; }

  void setPossibleTypes(ArrayRef<Type> Types) {
    PossibleTypes.clear();
    PossibleTypes.reserve(Types.size());
    for (auto T : Types) {
      if (T) {
        PossibleTypes.push_back(T);
      }
    }
  }

  Type getIdealType() const { return IdealType; }

  void setIdealType(Type IdealType) { this->IdealType = IdealType; }

  bool requiresNonVoid() const {
    if (IsImplicitSingleExpressionReturn)
      return false;
    if (PreferNonVoid)
      return true;
    if (PossibleTypes.empty())
      return false;
    return llvm::all_of(PossibleTypes, [](Type Ty) { return !Ty->isVoid(); });
  }

  bool isImplicitSingleExpressionReturn() const {
    return IsImplicitSingleExpressionReturn;
  }

  void
  setIsImplicitSingleExpressionReturn(bool IsImplicitSingleExpressionReturn) {
    this->IsImplicitSingleExpressionReturn = IsImplicitSingleExpressionReturn;
  }

  bool getPreferNonVoid() const { return PreferNonVoid; }

  void setPreferNonVoid(bool PreferNonVoid) {
    this->PreferNonVoid = PreferNonVoid;
  }
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

/// The type returned by a \c ContextFreeCodeCompletionResult. Can be either of
/// the following:
///  - Have the NotApplicable flag set: The completion result doesn't produce
///    something that's valid inside an expression like a keyword
///  - An null type if the completion result produces something that's
///    valid inside an expression but the result type isn't known
///  - A proper type if the type produced by this completion result is known
class CodeCompletionResultType {
public:
  enum class Flags : unsigned {
    IsNotApplicable = 1 << 0,
    /// If \p AlsoConsiderMetatype is set the code completion item will be
    /// considered as producing the declared interface type (which is passed as
    /// \p ResultTypes ) as well as the corresponding metatype.
    /// This allows us to suggest 'Int' as 'Identical' for both of the following
    /// functions
    ///
    ///   func receiveInstance(_: Int) {}
    ///   func receiveMetatype(_: Int.Type) {}
    AlsoConsiderMetatype = 1 << 1
  };

private:
  llvm::PointerIntPair<Type, 2, OptionSet<Flags>> TypeAndFlags;

  CodeCompletionResultType(Type Ty, OptionSet<Flags> Flag)
      : TypeAndFlags(Ty, Flag) {}

public:
  static CodeCompletionResultType notApplicable() {
    return CodeCompletionResultType(Type(), Flags::IsNotApplicable);
  }
  static CodeCompletionResultType unknown() {
    return CodeCompletionResultType(Type(), /*Flags=*/0);
  }
  CodeCompletionResultType(Type Ty, bool AlsoConsiderMetatype = false)
      : CodeCompletionResultType(Ty, AlsoConsiderMetatype
                                         ? Flags::AlsoConsiderMetatype
                                         : OptionSet<Flags>()) {}

  bool isNotApplicable() const {
    return TypeAndFlags.getInt().contains(Flags::IsNotApplicable);
  }

  /// Calculates the type realtion of this type to the given
  CodeCompletionResultTypeRelation
  calculateTypeRelation(const ExpectedTypeContext *TypeContext,
                        const DeclContext *DC) const;
};
} // namespace ide
} // namespace swift

#endif // SWIFT_IDE_CODECOMPLETIONRESULTTYPE_H

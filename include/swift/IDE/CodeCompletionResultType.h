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
  SmallVector<Type, 2> ResultTypes;

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

  bool isNotApplicable() const { return IsNotApplicable; }

  /// Calculates the type realtion of this type to the given
  CodeCompletionResultTypeRelation
  calculateTypeRelation(const ExpectedTypeContext &TypeContext,
                        const DeclContext *DC) const;
};
} // namespace ide
} // namespace swift

#endif // SWIFT_IDE_CODECOMPLETIONRESULTTYPE_H

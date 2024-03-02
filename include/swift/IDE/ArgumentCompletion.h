//===--- ArgumentCompletion.h -----------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2022 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_IDE_ARGUMENTCOMPLETION_H
#define SWIFT_IDE_ARGUMENTCOMPLETION_H

#include "swift/IDE/CodeCompletionConsumer.h"
#include "swift/IDE/CodeCompletionContext.h"
#include "swift/IDE/PossibleParamInfo.h"
#include "swift/IDE/TypeCheckCompletionCallback.h"

namespace swift {
namespace ide {

class ArgumentTypeCheckCompletionCallback : public TypeCheckCompletionCallback {
  struct Result {
    /// The type associated with the code completion expression itself.
    Type ExpectedType;

    /// The expected return type of the function call.
    Type ExpectedCallType;

    /// True if this is a subscript rather than a function call.
    bool IsSubscript;

    /// The FuncDecl or SubscriptDecl associated with the call.
    ValueDecl *FuncD;

    /// The type of the function being called.
    AnyFunctionType *FuncTy;

    /// The index of the argument containing the completion location
    unsigned ArgIdx;

    /// The index of the parameter corresponding to the completion argument.
    std::optional<unsigned> ParamIdx;

    /// The indices of all params that were bound to non-synthesized
    /// arguments. Used so we don't suggest them even when the args are out of
    /// order.
    std::set<unsigned> ClaimedParamIndices;

    /// True if the completion is a noninitial term in a variadic argument.
    bool IsNoninitialVariadic;

    /// Whether to suggest the entire functions signature instead of a single
    /// argument for this result.
    ///
    /// This is the case if there are no arguments or labels in the call yet and
    /// causes us to add a completion result that completes `Point(|)` to
    /// `Point(x: <Int>, y: <Int>)`.
    bool IncludeSignature;

    /// The base type of the call/subscript (null for free functions).
    Type BaseType;

    /// True if an argument label precedes the completion location.
    bool HasLabel;

    /// The argument index of the first trailing closure.
    ///
    /// \c None if the call doesn't have a trailing closure.
    std::optional<unsigned> FirstTrailingClosureIndex;

    /// Whether the surrounding context is async and thus calling async
    /// functions is supported.
    bool IsInAsyncContext;

    /// A bitfield to mark whether the parameter at a given index is optional.
    /// Parameters can be optional if they have a default argument or belong to
    /// a parameter pack.
    /// Indices are based on the parameters in \c FuncTy. Note that the number
    /// of parameters in \c FuncTy and \c FuncD is different when a parameter
    /// pack has been exploded.
    llvm::BitVector DeclParamIsOptional;

    /// Types of variables that were determined in the solution that produced
    /// this result. This in particular includes parameters of closures that
    /// were type-checked with the code completion expression.
    llvm::SmallDenseMap<const VarDecl *, Type> SolutionSpecificVarTypes;
  };

  CodeCompletionExpr *CompletionExpr;
  DeclContext *DC;

  SmallVector<Result, 4> Results;

  /// Populates a vector of parameters to suggest along with a vector of types
  /// to match the lookup results against.
  ///
  /// \Returns true if global lookup should be performed.
  bool addPossibleParams(const ArgumentTypeCheckCompletionCallback::Result &Res,
                         SmallVectorImpl<PossibleParamInfo> &Params,
                         SmallVectorImpl<Type> &Types);

  void sawSolutionImpl(const constraints::Solution &solution) override;

  /// Populates \p ShadowedDecls with all \c FuncD in \p Results that are
  /// defined in protocol extensions but redeclared on a nominal type and thus
  /// cannot be accessed
  void computeShadowedDecls(SmallPtrSetImpl<ValueDecl *> &ShadowedDecls);

public:
  ArgumentTypeCheckCompletionCallback(CodeCompletionExpr *CompletionExpr,
                                      DeclContext *DC)
      : CompletionExpr(CompletionExpr), DC(DC) {}

  /// \param IsLabeledTrailingClosure Whether we are completing the label of a
  /// labeled trailing closure, ie. if the code completion location is outside
  /// the call after the first trailing closure of the call.
  void collectResults(bool IsLabeledTrailingClosure,
                      SourceLoc Loc, DeclContext *DC,
                      CodeCompletionContext &CompletionCtx);
};

} // end namespace ide
} // end namespace swift

#endif // SWIFT_IDE_ARGUMENTCOMPLETION_H

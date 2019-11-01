//===--- CallerCandidateInfo.h - Failure Diagnosis Info -------------*- C++ -*-===//
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
// This file represents an analyzed function pointer to determine the
// candidates that could be called, or the one concrete decl that will be
// called if not ambiguous.
//
//===----------------------------------------------------------------------===//


#ifndef SWIFT_SEMA_CALLEECANDIDATEINFO_H
#define SWIFT_SEMA_CALLEECANDIDATEINFO_H

#include "swift/Basic/Debug.h"

namespace swift {
  
  using namespace constraints;

  /// Each match in an ApplyExpr is evaluated for how close of a match it is.
  /// The result is captured in this enum value, where the earlier entries are
  /// most specific.
  enum CandidateCloseness {
    CC_ExactMatch,              ///< This is a perfect match for the arguments.
    CC_Unavailable,             ///< Marked unavailable with @available.
    CC_Inaccessible,            ///< Not accessible from the current context.
    CC_NonLValueInOut,          ///< First arg is inout but no lvalue present.
    CC_SelfMismatch,            ///< Self argument mismatches.
    CC_OneArgumentNearMismatch, ///< All arguments except one match, near miss.
    CC_OneArgumentMismatch,     ///< All arguments except one match.
    CC_OneGenericArgumentNearMismatch, ///< All arguments except one match, guessing generic binding, near miss.
    CC_OneGenericArgumentMismatch,     ///< All arguments except one match, guessing generic binding.
    CC_ArgumentNearMismatch,    ///< Argument list mismatch, near miss.
    CC_ArgumentMismatch,        ///< Argument list mismatch.
    CC_GenericNonsubstitutableMismatch, ///< Arguments match each other, but generic binding not substitutable.
    CC_ArgumentLabelMismatch,   ///< Argument label mismatch.
    CC_ArgumentCountMismatch,   ///< This candidate has wrong # arguments.
    CC_GeneralMismatch          ///< Something else is wrong.
  };

  /// This is a candidate for a callee.
  ///
  /// `skipCurriedSelf` specifies that function type associated with this
  /// candidate might have a curried self parameter which needs to be
  /// skipped.
  ///
  /// `entityType` specifies a specific type to use for this decl/expr that may
  /// be more resolved than the concrete type.  For example, it may have generic
  /// arguments substituted in.
  ///
  struct OverloadCandidate {
    PointerUnion<ValueDecl *, Expr*> declOrExpr;
    bool skipCurriedSelf;
    Type entityType;
    
    // If true, entityType is written in terms of caller archetypes,
    // with any unbound generic arguments remaining as interface
    // type parameters in terms of the callee generic signature.
    //
    // If false, entityType is written in terms of callee archetypes.
    //
    // FIXME: Clean this up.
    bool substituted;

    OverloadCandidate(ValueDecl *decl, bool skipCurriedSelf);
    OverloadCandidate(Expr *expr, Type type)
        : declOrExpr(expr), skipCurriedSelf(false), entityType(type),
          substituted(true) {}

    ValueDecl *getDecl() const {
      return declOrExpr.dyn_cast<ValueDecl*>();
    }
    
    Expr *getExpr() const {
      return declOrExpr.dyn_cast<Expr*>();
    }

    Type getType() const {
      // Start with the known type of the decl.
      auto type = entityType;
      if (skipCurriedSelf) {
        auto funcTy = type->getAs<AnyFunctionType>();
        if (!funcTy) return Type();
        type = funcTy->getResult();
      }
      return type;
    }

    AnyFunctionType *getFunctionType() const {
      if (auto type = getType())
        return type->getAs<AnyFunctionType>();
      return nullptr;
    }

    /// Given a function candidate with an uncurry level, return the parameter
    /// type at the specified uncurry level.  If there is an error getting to
    /// the specified input, this returns a null Type.
    Type getArgumentType(ASTContext &ctx) const {
      if (!hasParameters())
        return Type();

      auto params = getParameters();
      return FunctionType::composeInput(ctx, params, false);
    }

    bool hasParameters() const { return getFunctionType(); }

    ArrayRef<AnyFunctionType::Param> getParameters() const {
      assert(hasParameters());
      return getFunctionType()->getParams();
    }

    ParameterListInfo
    getParameterListInfo(ArrayRef<AnyFunctionType::Param> params) const {
      auto *decl = getDecl();

      // FIXME: Subscript interface types don't have curried self parameters,
      // however CalleeCandidateInfo curries them with self. Therefore if we're
      // not supposed to skip the curried self parameter of a subscript,
      // return a zeroed ParameterListInfo.
      if (decl && isa<SubscriptDecl>(decl) && !skipCurriedSelf)
        return ParameterListInfo(params, nullptr, skipCurriedSelf);

      return ParameterListInfo(params, decl, skipCurriedSelf);
    }
    
    /// Given a function candidate with an uncurry level, return the parameter
    /// type at the specified uncurry level.  If there is an error getting to
    /// the specified input, this returns a null Type.
    Type getResultType() const {
      if (auto *funcTy = getFunctionType())
        return funcTy->getResult();
      return Type();
    }

    void dump(llvm::raw_ostream &os) const;
    SWIFT_DEBUG_DUMP;
  };

  class CalleeCandidateInfo {
  public:
    ConstraintSystem &CS;
    
    /// This is the name of the callee as extracted from the call expression.
    /// This can be empty in cases like calls to closure exprs.
    std::string declName;
    
    /// True if the call site for this callee syntactically has a trailing
    /// closure specified.
    bool hasTrailingClosure;
    
    /// This is the list of candidates identified.
    SmallVector<OverloadCandidate, 4> candidates;

    /// This tracks how close the candidates are, after filtering.
    CandidateCloseness closeness = CC_GeneralMismatch;
    
    /// When we have a candidate that differs by a single argument mismatch, we
    /// keep track of which argument passed to the call is failed, and what the
    /// expected type is.  If the candidate set disagrees, or if there is more
    /// than a single argument mismatch, then this is "{ -1, Type() }".
    struct FailedArgumentInfo {
      int argumentNumber = -1;      ///< Arg # at the call site.
      Type parameterType = Type();  ///< Expected type at the decl site.
      DeclContext *declContext = nullptr; ///< Context at the candidate declaration.
      
      bool isValid() const { return argumentNumber != -1; }
      
      bool operator!=(const FailedArgumentInfo &other) {
        if (argumentNumber != other.argumentNumber) return true;
        if (declContext != other.declContext) return true;
        // parameterType can be null, and isEqual doesn't handle this.
        if (!parameterType || !other.parameterType)
          return parameterType.getPointer() != other.parameterType.getPointer();
        return !parameterType->isEqual(other.parameterType);
      }
    };
    FailedArgumentInfo failedArgument = FailedArgumentInfo();
    
    /// Analyze a function expr and break it into a candidate set.  On failure,
    /// this leaves the candidate list empty.
    CalleeCandidateInfo(Expr *Fn, bool hasTrailingClosure, ConstraintSystem &CS)
    : CS(CS), hasTrailingClosure(hasTrailingClosure) {
      collectCalleeCandidates(Fn, /*implicitDotSyntax=*/false);
    }
    
    CalleeCandidateInfo(Type baseType, ArrayRef<OverloadChoice> candidates,
                        bool hasTrailingClosure, ConstraintSystem &CS,
                        bool selfAlreadyApplied = true);

    ~CalleeCandidateInfo() = default;
    CalleeCandidateInfo(const CalleeCandidateInfo &CCI) = default;
    CalleeCandidateInfo &operator=(const CalleeCandidateInfo &CCI);
    CalleeCandidateInfo(CalleeCandidateInfo &&CCI) = delete;
    CalleeCandidateInfo &operator=(CalleeCandidateInfo &&CCI) = delete;

    using ClosenessResultTy = std::pair<CandidateCloseness, FailedArgumentInfo>;
    using ClosenessPredicate =
        const std::function<ClosenessResultTy(OverloadCandidate)> &;

    /// After the candidate list is formed, it can be filtered down to discard
    /// obviously mismatching candidates and compute a "closeness" for the
    /// resultant set.
    ClosenessResultTy
    evaluateCloseness(OverloadCandidate candidate,
                      ArrayRef<AnyFunctionType::Param> actualArgs);

    void filterListArgs(ArrayRef<AnyFunctionType::Param> actualArgs);
    void filterList(ClosenessPredicate predicate);
    void filterContextualMemberList(Expr *argExpr);
    
    bool empty() const { return candidates.empty(); }
    unsigned size() const { return candidates.size(); }
    OverloadCandidate operator[](unsigned i) const { return candidates[i]; }

    /// Given a set of parameter lists from an overload group, and a list of
    /// arguments, emit a diagnostic indicating any partially matching
    /// overloads.
    void suggestPotentialOverloads(SourceLoc loc, bool isResult = false);

    /// Emit a diagnostic and return true if this is an error condition we can
    /// handle uniformly.  This should be called after filtering the candidate
    /// list.
    bool diagnoseSimpleErrors(const Expr *E);
    
    void dump(llvm::raw_ostream &os) const;
    SWIFT_DEBUG_DUMP;
    
  private:
    void collectCalleeCandidates(Expr *fnExpr, bool implicitDotSyntax);
  };
} // end swift namespace

#endif /* SWIFT_SEMA_CALLEECANDIDATEINFO_H */

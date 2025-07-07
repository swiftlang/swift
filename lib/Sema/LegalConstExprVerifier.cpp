//===--------------------- LegalConstExprVerifier.cpp ---------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2025 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
// This file implements syntactic validation that a `@const` expression
// consists strictly of values and operations on those values as allowed
// by the `@const` feature specification at this time.
//
//===----------------------------------------------------------------------===//

#include "MiscDiagnostics.h"
#include "TypeChecker.h"
#include "swift/AST/ASTContext.h"
#include "swift/AST/ASTWalker.h"
#include "swift/AST/ParameterList.h"
#include "swift/AST/SemanticAttrs.h"
#include "swift/Basic/Assertions.h"
using namespace swift;

namespace {
static bool isIntegerType(Type type) {
  return type->isInt() || type->isInt8() || type->isInt16() ||
         type->isInt32() || type->isInt64() || type->isUInt() ||
         type->isUInt8() || type->isUInt16() || type->isUInt32() ||
         type->isUInt64();
}

static bool isFloatType(Type type) {
  // TODO: CGFloat?
  return type->isFloat() || type->isDouble() || type->isFloat80();
}

enum IllegalConstErrorDiagnosis {
  TypeNotSupported,
  AssociatedValue,
  UnsupportedBinaryOperator,
  UnsupportedUnaryOperator,
  TypeExpression,
  KeyPath,
  Closue,
  OpaqueDeclRef,
  OpaqueFuncDeclRef,
  NonConventionCFunc,
  OpaqueCalleeRef,
  NonConstParameter,
  Default
};

/// Given a provided error expression \p errorExpr, and the reason
/// \p reason for the failure, emit corresponding diagnostic.
static void diagnoseError(const Expr *errorExpr,
                          IllegalConstErrorDiagnosis reason,
                          DiagnosticEngine &diags) {
  SourceLoc errorLoc = errorExpr->getLoc();
  switch (reason) {
  case TypeNotSupported:
    diags.diagnose(errorLoc, diag::const_unsupported_type);
    break;
  case AssociatedValue:
    diags.diagnose(errorLoc, diag::const_unsupported_enum_associated_value);
    break;
  case UnsupportedBinaryOperator:
    diags.diagnose(errorLoc, diag::const_unsupported_operator);
    break;
  case UnsupportedUnaryOperator:
    diags.diagnose(errorLoc, diag::const_unsupported_operator);
    break;
  case TypeExpression:
    diags.diagnose(errorLoc, diag::const_unsupported_type_expr);
    break;
  case KeyPath:
    diags.diagnose(errorLoc, diag::const_unsupported_keypath);
    break;
  case Closue:
    diags.diagnose(errorLoc, diag::const_unsupported_closure);
    break;
  case OpaqueDeclRef:
    diags.diagnose(errorLoc, diag::const_opaque_decl_ref);
    break;
  case OpaqueFuncDeclRef:
    diags.diagnose(errorLoc, diag::const_opaque_func_decl_ref);
    break;
  case NonConventionCFunc:
    diags.diagnose(errorLoc, diag::const_non_convention_c_conversion);
    break;
  case OpaqueCalleeRef:
    diags.diagnose(errorLoc, diag::const_opaque_callee);
    break;
  case NonConstParameter:
    diags.diagnose(errorLoc, diag::const_non_const_param);
    break;
  case Default:
    diags.diagnose(errorLoc, diag::const_unknown_default);
    break;
  }
}

static bool supportedOperator(const ApplyExpr *operatorApplyExpr) {
  const auto operatorDeclRefExpr =
      operatorApplyExpr->getFn()->getMemberOperatorRef();
  if (!operatorDeclRefExpr)
    return false;

  // Non-stdlib operators are not allowed, for now
  auto operatorDecl = operatorDeclRefExpr->getDecl();
  if (!operatorDecl->getModuleContext()->isStdlibModule())
    return false;

  auto operatorName = operatorDecl->getBaseName();
  if (!operatorName.isOperator())
    return false;

  auto operatorIdentifier = operatorName.getIdentifier();
  if (!operatorIdentifier.isArithmeticOperator() &&
      !operatorIdentifier.isBitwiseOperator() &&
      !operatorIdentifier.isShiftOperator() &&
      !operatorIdentifier.isStandardComparisonOperator())
    return false;

  // Operators which are not integer or floating point type are not
  // allowed, for now.
  auto operatorType = operatorApplyExpr->getType();
  if (!isIntegerType(operatorType) && !isFloatType(operatorType))
    return false;

  return true;
}

static std::optional<std::pair<const Expr *, IllegalConstErrorDiagnosis>>
checkSupportedInConst(const Expr *expr, const DeclContext *declContext) {
  SmallVector<const Expr *, 4> expressionsToCheck;
  expressionsToCheck.push_back(expr);
  while (!expressionsToCheck.empty()) {
    const Expr *expr = expressionsToCheck.pop_back_val();
    // Lookthrough IdentityExpr, Tuple, Array, and InjectIntoOptional
    // expressions.
    if (const IdentityExpr *identityExpr = dyn_cast<IdentityExpr>(expr)) {
      expressionsToCheck.push_back(identityExpr->getSubExpr());
      continue;
    }
    if (const TupleExpr *tupleExpr = dyn_cast<TupleExpr>(expr)) {
      for (const Expr *element : tupleExpr->getElements())
        expressionsToCheck.push_back(element);
      continue;
    }
    if (const ArrayExpr *arrayExpr = dyn_cast<ArrayExpr>(expr)) {
      for (const Expr *element : arrayExpr->getElements())
        expressionsToCheck.push_back(element);
      continue;
    }
    if (const InjectIntoOptionalExpr *optionalExpr =
            dyn_cast<InjectIntoOptionalExpr>(expr)) {
      expressionsToCheck.push_back(optionalExpr->getSubExpr());
      continue;
    }

    // Ensure that binary expressions consist of literals, references
    // to other variables, and supported operators on integer and floating
    // point types only.
    if (const BinaryExpr *binaryExpr = dyn_cast<BinaryExpr>(expr)) {
      if (!supportedOperator(binaryExpr))
        return std::make_pair(binaryExpr, UnsupportedBinaryOperator);

      expressionsToCheck.push_back(binaryExpr->getLHS());
      expressionsToCheck.push_back(binaryExpr->getRHS());
      continue;
    }
    if (const PrefixUnaryExpr *unaryExpr = dyn_cast<PrefixUnaryExpr>(expr)) {
      if (!supportedOperator(unaryExpr))
        return std::make_pair(unaryExpr, UnsupportedUnaryOperator);

      expressionsToCheck.push_back(unaryExpr->getOperand());
      continue;
    }

    // Literal expressions are okay
    if (isa<LiteralExpr>(expr))
      continue;

    // Type expressions not supported in `@const` expressions
    if (isa<TypeExpr>(expr))
      return std::make_pair(expr, TypeExpression);

    // Keypath expressions not supported in `@const` expressions for now
    if (isa<KeyPathExpr>(expr))
      return std::make_pair(expr, KeyPath);

    // Closure expressions are not supported in `@const` expressions
    // TODO: `@const`-evaluable closures
    if (isa<AbstractClosureExpr>(expr))
      return std::make_pair(expr, Closue);

    // Function conversions, as long as the conversion is to a 'convention(c)'
    // then consider the operand sub-expression
    if (auto functionConvExpr = dyn_cast<FunctionConversionExpr>(expr)) {
      if (auto targetFnTy =
              functionConvExpr->getType()->getAs<AnyFunctionType>()) {
        if (targetFnTy->getExtInfo().getRepresentation() ==
            FunctionTypeRepresentation::CFunctionPointer) {
          expressionsToCheck.push_back(functionConvExpr->getSubExpr());
          continue;
        } else {
          return std::make_pair(expr, NonConventionCFunc);
        }
      }
      return std::make_pair(expr, Default);
    }

    // Default argument expressions of a function must be ensured to be a
    // constant by the definition of the function.
    if (isa<DefaultArgumentExpr>(expr))
      continue;

    auto checkVarDecl = [&](const VarDecl *varDecl) -> bool {
      // `@const` variables are always okay, their initializer expressions
      // will be checked separately, individually.
      if (varDecl->isConstValue())
        return true;

      // Non-explicitly-`@const` variables must have an initial value
      // we can look through.
      if (!varDecl->hasInitialValue())
        return false;

      if (auto initExpr = varDecl->getParentInitializer()) {
        expressionsToCheck.push_back(initExpr);
        return true;
      }

      return false;
    };

    auto checkFuncDecl = [&](const FuncDecl *funcDecl) -> bool {
      if (funcDecl->hasBody() &&
          funcDecl->getDeclContext()->getOutermostParentSourceFile() ==
              declContext->getOutermostParentSourceFile())
        return true;

      return false;
    };

    // Look through to initial value expressions of memeber ref expressions
    if (const MemberRefExpr *memberRef = dyn_cast<MemberRefExpr>(expr)) {
      if (VarDecl *memberVarDecl =
              dyn_cast<VarDecl>(memberRef->getMember().getDecl())) {
        if (checkVarDecl(memberVarDecl))
          continue;
        return std::make_pair(expr, OpaqueDeclRef);
      }
      return std::make_pair(expr, OpaqueDeclRef);
    }

    // Look through to initial value expressions of decl ref expressions
    if (const DeclRefExpr *declRef = dyn_cast<DeclRefExpr>(expr)) {
      auto decl = declRef->getDecl();
      // `@const` paramters are always okay
      if (auto *paramDecl = dyn_cast<ParamDecl>(decl)) {
        if (!paramDecl->isConstVal())
          return std::make_pair(expr, NonConstParameter);
        continue;
      }

      // function values
      if (auto *funcDecl = dyn_cast<FuncDecl>(decl)) {
        if (checkFuncDecl(funcDecl))
          continue;
        return std::make_pair(expr, OpaqueDeclRef);
      }

      if (const VarDecl *varDecl = dyn_cast<VarDecl>(declRef->getDecl())) {
        if (checkVarDecl(varDecl))
          continue;
        return std::make_pair(expr, OpaqueDeclRef);
      }
      return std::make_pair(expr, OpaqueDeclRef);
    }

    // Otherwise only allow enum cases and function calls
    if (!isa<ApplyExpr>(expr))
      return std::make_pair(expr, Default);

    const ApplyExpr *apply = cast<ApplyExpr>(expr);
    ValueDecl *calledValue = apply->getCalledValue();
    if (!calledValue)
      return std::make_pair(expr, OpaqueCalleeRef);

    // If this is an enum case, check that it does not have associated values
    if (EnumElementDecl *enumCase = dyn_cast<EnumElementDecl>(calledValue)) {
      if (enumCase->hasAssociatedValues())
        return std::make_pair(expr, AssociatedValue);
      continue;
    }

    // Explicitly support calls to `Int` and `Float` constructors
    if (ConstructorRefCallExpr *initCallRef =
            dyn_cast<ConstructorRefCallExpr>(apply->getSemanticFn())) {
      if (auto type = initCallRef->getType()) {
        if (auto *funcType = type->getAs<FunctionType>()) {
          auto resultTy = funcType->getResult();
          if (isIntegerType(resultTy) || isFloatType(resultTy)) {
            assert(apply->getArgs()->size() == 1);
            expressionsToCheck.push_back(apply->getArgs()->getExpr(0));
            continue;
          }
        }
      }
    }

    // TODO: calls to `@const` functions
    //  AbstractFunctionDecl *callee =
    //  dyn_cast<AbstractFunctionDecl>(calledValue); if (!callee)
    //    return expr;
    //  if (callee->isConstFunction()) {
    //    for (auto arg : *apply->getArgs())
    //      expressionsToCheck.push_back(arg.getExpr());
    //  }

    return std::make_pair(expr, Default);
  }
  return std::nullopt;
}

/// Given a call \c callExpr, if some or all of its arguments are required to be
/// constants, check the argument expressions.
static void verifyConstArguments(const CallExpr *callExpr,
                                 const DeclContext *declContext) {
  ValueDecl *calledDecl = callExpr->getCalledValue();
  if (!calledDecl || !isa<AbstractFunctionDecl>(calledDecl))
    return;
  AbstractFunctionDecl *callee = cast<AbstractFunctionDecl>(calledDecl);
  SmallVector<unsigned, 4> constArgumentIndices;
  auto paramList = callee->getParameters();
  for (unsigned i = 0; i < paramList->size(); ++i) {
    ParamDecl *param = paramList->get(i);
    if (param->isConstVal())
      constArgumentIndices.push_back(i);
  }
  if (constArgumentIndices.empty())
    return;

  // Check that the arguments at the constArgumentIndices are all expressions
  // consisting of @const-compatible constructs.
  SmallVector<Expr *, 4> arguments;
  for (auto arg : *callExpr->getArgs())
    arguments.push_back(arg.getExpr());

  for (unsigned constantIndex : constArgumentIndices) {
    assert(constantIndex < arguments.size() &&
           "constantIndex exceeds the number of arguments to the function");
    if (auto error =
            checkSupportedInConst(arguments[constantIndex], declContext)) {
      diagnoseError(error->first, error->second,
                    declContext->getASTContext().Diags);
      declContext->getASTContext().Diags.diagnose(
          arguments[constantIndex]->getLoc(),
          diag::require_const_arg_for_parameter);
    }
  }
}
} // anonymous namespace

void swift::diagnoseInvalidConstExpressions(const Expr *expr,
                                            const DeclContext *declContext,
                                            bool isConstInitExpr) {
  if (isConstInitExpr) {
    if (auto error = checkSupportedInConst(expr, declContext))
      diagnoseError(error->first, error->second,
                    declContext->getASTContext().Diags);
  } else if (auto *callExpr = dyn_cast<CallExpr>(expr))
    verifyConstArguments(callExpr, declContext);
}

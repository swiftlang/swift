//===------------------------ ConstantnessSemaDiagnostics.cpp -------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2020 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
// This file implements checks for checking whether certain arguments to some
// specific APIs are compile-time constants (see below for the definition of
// constants). In particular, this code checks whether the new os_log APIs are
// invoked with constant arguments, and whether the primitive atomic operations
// are invoked with constant "orderings". These APIs are identified through
// @_semantics attributes.
//
// A "compile-time constant" is either a literal (including
// string/integer/float/boolean/string-interpolation literal) or a call to a
// "constant_evaluable" function (or property) with compile-time constant
// arguments. A closure expression is also considered a compile-time constant
// (it is a constant of a function type).
//===----------------------------------------------------------------------===//

#include "MiscDiagnostics.h"
#include "TypeChecker.h"
#include "swift/AST/ASTContext.h"
#include "swift/AST/ASTWalker.h"
#include "swift/AST/ParameterList.h"
#include "swift/AST/SemanticAttrs.h"
using namespace swift;

/// Check whether a given \p decl has a @_semantics attribute with the given
/// attribute name \c attrName.
static bool hasSemanticsAttr(ValueDecl *decl, StringRef attrName) {
  for (auto semantics : decl->getAttrs().getAttributes<SemanticsAttr>()) {
    if (semantics->Value.equals(attrName))
      return true;
  }
  return false;
}

/// Return true iff  the given \p structDecl has a name that matches one of the
/// known atomic orderings structs.
static bool isAtomicOrderingDecl(StructDecl *structDecl) {
  ASTContext &astContext = structDecl->getASTContext();
  Identifier structName = structDecl->getName();
  return (structName == astContext.Id_AtomicLoadOrdering ||
          structName == astContext.Id_AtomicStoreOrdering ||
          structName == astContext.Id_AtomicUpdateOrdering);
}

/// Return true iff the given nominal type decl \p nominal has a name that
/// matches one of the known OSLog types that need not be a constant in the new
/// os_log APIs.
static bool isOSLogDynamicObject(NominalTypeDecl *nominal) {
  ASTContext &astContext = nominal->getASTContext();
  Identifier name = nominal->getName();
  return (name == astContext.Id_OSLog || name == astContext.Id_OSLogType);
}

/// Return true iff the parameter \p param of function \c funDecl is required to
/// be a constant. This is true if either the function is an os_log function or
/// it is an atomics operation and the parameter represents the ordering.
static bool isParamRequiredToBeConstant(FuncDecl *funcDecl, ParamDecl *param) {
  assert(funcDecl && param && "funcDecl and param must not be null");
  if (hasSemanticsAttr(funcDecl, semantics::OSLOG_REQUIRES_CONSTANT_ARGUMENTS))
    return true;
  if (hasSemanticsAttr(funcDecl, semantics::OSLOG_LOG_WITH_LEVEL)) {
    // We are looking at a top-level os_log function that accepts level and
    // possibly custom log object. Those need not be constants, but every other
    // parameter must be.
    Type paramType = param->getType();
    NominalTypeDecl *nominal = paramType->getNominalOrBoundGenericNominal();
    return !nominal || !isOSLogDynamicObject(nominal);
  }
  if (!hasSemanticsAttr(funcDecl,
                        semantics::ATOMICS_REQUIRES_CONSTANT_ORDERINGS))
    return false;
  Type paramType = param->getType();
  StructDecl *structDecl = paramType->getStructOrBoundGenericStruct();
  if (!structDecl)
    return false;
  return isAtomicOrderingDecl(structDecl);
}

/// Return true iff the \c decl is annotated as
/// @_semantics("constant_evaluable").
static bool hasConstantEvaluableAttr(ValueDecl *decl) {
  return hasSemanticsAttr(decl, semantics::CONSTANT_EVALUABLE);
}

/// Return true iff the \p decl is annotated with oslog.message.init semantics
/// attribute.
static bool isOSLogMessageInitializer(ValueDecl *decl) {
  return hasSemanticsAttr(decl, semantics::OSLOG_MESSAGE_INIT_STRING_LITERAL) ||
         hasSemanticsAttr(decl, semantics::OSLOG_MESSAGE_INIT_INTERPOLATION);
}

/// Check whether \p expr is a compile-time constant. It must either be a
/// literal_expr, which does not include array and dictionary literal, or a
/// closure expression, which is considered a compile-time constant of a
/// function type, or a call to a "constant_evaluable" function (or property)
/// whose arguments are themselves compile-time constants.
static Expr *checkConstantness(Expr *expr) {
  SmallVector<Expr *, 4> expressionsToCheck;
  expressionsToCheck.push_back(expr);
  while (!expressionsToCheck.empty()) {
    Expr *expr = expressionsToCheck.pop_back_val();
    // Lookthrough identity_expr, tuple and inject_into_optional expressions.
    if (IdentityExpr *identityExpr = dyn_cast<IdentityExpr>(expr)) {
      expressionsToCheck.push_back(identityExpr->getSubExpr());
      continue;
    }
    if (TupleExpr *tupleExpr = dyn_cast<TupleExpr>(expr)) {
      for (Expr *element : tupleExpr->getElements())
        expressionsToCheck.push_back(element);
      continue;
    }
    if (InjectIntoOptionalExpr *optionalExpr =
            dyn_cast<InjectIntoOptionalExpr>(expr)) {
      expressionsToCheck.push_back(optionalExpr->getSubExpr());
      continue;
    }
    // Literal expressions also includes InterpolatedStringLiteralExpr.
    if (isa<LiteralExpr>(expr))
      continue;
    if (isa<TypeExpr>(expr))
      continue;
    // Closure expressions are always treated as constants. They are
    // constants of function types.
    if (isa<AbstractClosureExpr>(expr))
      continue;
    // Default argument expressions of a constant_evaluable or a
    // requires_constant function must be ensured to be a constant by the
    // definition of the function.
    if (isa<DefaultArgumentExpr>(expr))
      continue;

    // If this is a member-ref, it has to be annotated constant evaluable.
    if (MemberRefExpr *memberRef = dyn_cast<MemberRefExpr>(expr)) {
      if (ValueDecl *memberDecl = memberRef->getMember().getDecl()) {
        if (hasConstantEvaluableAttr(memberDecl))
          continue;
      }
      return expr;
    }

    // If this is a variable, it has to be a known constant parameter of the
    // enclosing function.
    if (DeclRefExpr *declRef = dyn_cast<DeclRefExpr>(expr)) {
      ValueDecl *decl = declRef->getDecl();
      if (!decl)
        return expr;
      ParamDecl *paramDecl = dyn_cast<ParamDecl>(decl);
      if (!paramDecl)
        return expr;
      Decl *declContext = paramDecl->getDeclContext()->getAsDecl();
      if (!declContext)
        return expr;
      FuncDecl *funcDecl = dyn_cast<FuncDecl>(declContext);
      if (!funcDecl || !isParamRequiredToBeConstant(funcDecl, paramDecl))
        return expr;
      continue;
    }

    if (!isa<ApplyExpr>(expr))
      return expr;

    ApplyExpr *apply = cast<ApplyExpr>(expr);
    ValueDecl *calledValue = apply->getCalledValue();
    if (!calledValue)
      return expr;

    // If this is an enum case, check whether the arguments are constants.
    if (isa<EnumElementDecl>(calledValue)) {
      expressionsToCheck.push_back(apply->getArg());
      continue;
    }

    AbstractFunctionDecl *callee = dyn_cast<AbstractFunctionDecl>(calledValue);
    if (!callee)
      return expr;

    // If this is an application of OSLogMessage initializer, fail the check
    // as this type must be created from string interpolations.
    if (isOSLogMessageInitializer(callee))
      return expr;

    // If this is a constant_evaluable function, check whether the arguments are
    // constants.
    if (!hasConstantEvaluableAttr(callee))
      return expr;
    expressionsToCheck.push_back(apply->getArg());
  }
  return nullptr;
}

/// Return true iff the norminal type decl \c numberDecl is a known stdlib
/// integer decl.
static bool isStdlibInteger(NominalTypeDecl *numberDecl) {
  ASTContext &astCtx = numberDecl->getASTContext();
  return (numberDecl == astCtx.getIntDecl() ||
          numberDecl == astCtx.getInt8Decl() ||
          numberDecl == astCtx.getInt16Decl() ||
          numberDecl == astCtx.getInt32Decl() ||
          numberDecl == astCtx.getInt64Decl() ||
          numberDecl == astCtx.getUIntDecl() ||
          numberDecl == astCtx.getUInt8Decl() ||
          numberDecl == astCtx.getUInt16Decl() ||
          numberDecl == astCtx.getUInt32Decl() ||
          numberDecl == astCtx.getUInt64Decl());
}

/// Return true iff the given \p type is a Stdlib integer type.
static bool isIntegerType(Type type) {
  NominalTypeDecl *nominalDecl = type->getNominalOrBoundGenericNominal();
  return nominalDecl && isStdlibInteger(nominalDecl);
}

/// Return true iff the norminal type decl \c numberDecl is a known stdlib float
/// decl.
static bool isStdlibFloat(NominalTypeDecl *numberDecl) {
  ASTContext &astCtx = numberDecl->getASTContext();
  return (numberDecl == astCtx.getFloatDecl() ||
          numberDecl == astCtx.getFloat80Decl() ||
          numberDecl == astCtx.getDoubleDecl());
}

/// Return true iff the given \p type is a Bool type.
static bool isFloatType(Type type) {
  NominalTypeDecl *nominalDecl = type->getNominalOrBoundGenericNominal();
  return nominalDecl && isStdlibFloat(nominalDecl);
}

/// Return true iff the given \p type is a String type.
static bool isStringType(Type type) {
  NominalTypeDecl *nominalDecl = type->getNominalOrBoundGenericNominal();
  return nominalDecl && nominalDecl == type->getASTContext().getStringDecl();
}

/// Given an error expression \p errorExpr, diagnose the error based on the type
/// of the expression. For instance, if the expression's type is expressible by
/// a literal e.g. integer, boolean etc. report that it must be a literal.
/// Otherwise, if the expression is a nominal type, report that it must be
/// static member of the type.
static void diagnoseError(Expr *errorExpr, const ASTContext &astContext,
                          FuncDecl *funcDecl) {
  DiagnosticEngine &diags = astContext.Diags;
  Type exprType = errorExpr->getType();
  SourceLoc errorLoc = errorExpr->getLoc();

  // Diagnose atomics ordering related error here.
  if (hasSemanticsAttr(funcDecl,
                       semantics::ATOMICS_REQUIRES_CONSTANT_ORDERINGS)) {
    NominalTypeDecl *nominalDecl = exprType->getNominalOrBoundGenericNominal();
    if (!nominalDecl) {
      // This case should normally not happen. This is a safe guard against
      // possible mismatch between the atomics library and the compiler.
      diags.diagnose(errorLoc, diag::argument_must_be_constant);
    }
    diags.diagnose(errorLoc, diag::atomics_ordering_must_be_constant,
                   nominalDecl->getName());
    return;
  }

  // Diagnose os_log specific errors here.

  // Diagnose primitive stdlib types.
  if (exprType->isBool()) {
    diags.diagnose(errorLoc, diag::oslog_arg_must_be_bool_literal);
    return;
  }
  if (isStringType(exprType)) {
    diags.diagnose(errorLoc, diag::oslog_arg_must_be_string_literal);
    return;
  }
  if (isIntegerType(exprType)) {
    diags.diagnose(errorLoc, diag::oslog_arg_must_be_integer_literal);
    return;
  }
  if (isFloatType(exprType)) {
    diags.diagnose(errorLoc, diag::oslog_arg_must_be_float_literal);
    return;
  }
  if (exprType->is<MetatypeType>()) {
    diags.diagnose(errorLoc, diag::oslog_arg_must_be_metatype_literal);
    return;
  }
  if (exprType->is<AnyFunctionType>()) {
    diags.diagnose(errorLoc, diag::oslog_arg_must_be_closure);
    return;
  }
  if (EnumDecl *enumDecl = exprType->getEnumOrBoundGenericEnum()) {
    diags.diagnose(errorLoc, diag::oslog_arg_must_be_enum_case,
                   enumDecl->getName());
    return;
  }
  NominalTypeDecl *nominalDecl = exprType->getNominalOrBoundGenericNominal();
  if (!nominalDecl) {
    // This case should normally not happen. This is a safe guard against
    // possible mismatch between the os overlay and the compiler.
    diags.diagnose(errorLoc, diag::argument_must_be_constant);
    return;
  }
  // If this is OSLogMessage, it should be a string-interpolation literal.
  Identifier declName = nominalDecl->getName();
  if (declName == astContext.Id_OSLogMessage) {
    diags.diagnose(errorLoc, diag::oslog_message_must_be_string_interpolation);
    return;
  }
  diags.diagnose(errorLoc, diag::oslog_arg_must_be_type_member_access,
                 declName);
}

/// Given a call \c callExpr, if some or all of its arguments are required to be
/// constants, check that property on the arguments.
static void diagnoseConstantArgumentRequirementOfCall(const CallExpr *callExpr,
                                                      const ASTContext &ctx) {
  assert(callExpr && callExpr->getType() &&
         "callExpr should have a valid type");
  ValueDecl *calledDecl = callExpr->getCalledValue();
  if (!calledDecl || !isa<FuncDecl>(calledDecl))
    return;
  FuncDecl *callee = cast<FuncDecl>(calledDecl);

  // Collect argument indices that are required to be constants.
  SmallVector<unsigned, 4> constantArgumentIndices;
  auto paramList = callee->getParameters();
  for (unsigned i = 0; i < paramList->size(); i++) {
    ParamDecl *param = paramList->get(i);
    if (isParamRequiredToBeConstant(callee, param))
      constantArgumentIndices.push_back(i);
  }
  if (constantArgumentIndices.empty())
    return;

  // Check that the arguments at the constantArgumentIndices are constants.
  Expr *argumentExpr = callExpr->getArg();
  SmallVector<Expr *, 4> arguments;
  if (TupleExpr *tupleExpr = dyn_cast<TupleExpr>(argumentExpr)) {
    auto elements = tupleExpr->getElements();
    arguments.append(elements.begin(), elements.end());
  } else if (ParenExpr *parenExpr = dyn_cast<ParenExpr>(argumentExpr)) {
    arguments.push_back(parenExpr->getSubExpr());
  } else {
    arguments.push_back(argumentExpr);
  }

  for (unsigned constantIndex : constantArgumentIndices) {
    assert(constantIndex < arguments.size() &&
           "constantIndex exceeds the number of arguments to the function");
    Expr *argument = arguments[constantIndex];
    Expr *errorExpr = checkConstantness(argument);
    if (errorExpr)
      diagnoseError(errorExpr, ctx, callee);
  }
}

void swift::diagnoseConstantArgumentRequirement(
    const Expr *expr, const DeclContext *declContext) {
  class ConstantReqCallWalker : public ASTWalker {
    const ASTContext &astContext;

  public:
    ConstantReqCallWalker(ASTContext &ctx) : astContext(ctx) {}

    // Descend until we find a call expressions. Note that the input expression
    // could be an assign expression or another expression that contains the
    // call.
    std::pair<bool, Expr *> walkToExprPre(Expr *expr) override {
      if (!expr || isa<ErrorExpr>(expr) || !expr->getType())
        return {false, expr};
      if (auto *callExpr = dyn_cast<CallExpr>(expr)) {
        diagnoseConstantArgumentRequirementOfCall(callExpr, astContext);
        return {false, expr};
      }
      return {true, expr};
    }
  };

  ConstantReqCallWalker walker(declContext->getASTContext());
  const_cast<Expr *>(expr)->walk(walker);
}

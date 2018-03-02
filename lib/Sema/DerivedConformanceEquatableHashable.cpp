//===--- DerivedConformanceEquatableHashable.cpp - Derived Equatable & co -===//
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
//  This file implements implicit derivation of the Equatable and Hashable
//  protocols. (Comparable is similar enough in spirit that it would make
//  sense to live here too when we implement its derivation.)
//
//===----------------------------------------------------------------------===//

#include "TypeChecker.h"
#include "swift/AST/Decl.h"
#include "swift/AST/Stmt.h"
#include "swift/AST/Expr.h"
#include "swift/AST/Module.h"
#include "swift/AST/Pattern.h"
#include "swift/AST/ParameterList.h"
#include "swift/AST/ProtocolConformance.h"
#include "swift/AST/Types.h"
#include "llvm/ADT/APInt.h"
#include "llvm/ADT/SmallString.h"
#include "llvm/Support/raw_ostream.h"
#include "DerivedConformances.h"

using namespace swift;
using namespace DerivedConformance;

/// Returns true if, for every element of the given enum, it either has no
/// associated values or all of them conform to a protocol.
/// \p theEnum The enum whose elements and associated values should be checked.
/// \p protocol The protocol being requested.
/// \return True if all associated values of all elements of the enum conform.
bool allAssociatedValuesConformToProtocol(TypeChecker &tc, EnumDecl *theEnum,
                                          ProtocolDecl *protocol) {
  auto declContext = theEnum->getDeclContext();

  for (auto elt : theEnum->getAllElements()) {
    if (!elt->getArgumentTypeLoc().getType())
      tc.validateDecl(elt);

    auto argumentType = elt->getArgumentTypeLoc().getType();
    if (!argumentType)
      continue;

    if (auto tupleType = argumentType->getAs<TupleType>()) {
      // One associated value with a label or multiple associated values
      // (labeled or unlabeled) are tuple types.
      for (auto tupleElementType : tupleType->getElementTypes()) {
        if (!tc.conformsToProtocol(tupleElementType, protocol, declContext,
                                   ConformanceCheckFlags::Used)) {
          return false;
        }
      }
    } else {
      // One associated value with no label is represented as a paren type.
      auto actualType = argumentType->getWithoutParens();
      if (!tc.conformsToProtocol(actualType, protocol, declContext,
                                 ConformanceCheckFlags::Used)) {
        return false;
      }
    }
  }
  return true;
}

/// Returns true if every stored property in the given struct conforms to the
/// protocol (or, vacuously, if it has no stored properties).
/// \p theStruct The struct whose stored properties should be checked.
/// \p protocol The protocol being requested.
/// \return True if all stored properties of the struct conform.
bool allStoredPropertiesConformToProtocol(TypeChecker &tc,
                                          StructDecl *theStruct,
                                          ProtocolDecl *protocol) {
  auto declContext = theStruct->getDeclContext();

  auto storedProperties =
    theStruct->getStoredProperties(/*skipInaccessible=*/true);
  for (auto propertyDecl : storedProperties) {
    if (!propertyDecl->hasType())
      tc.validateDecl(propertyDecl);

    if (!propertyDecl->hasType() ||
        !tc.conformsToProtocol(propertyDecl->getType(), protocol, declContext,
                               ConformanceCheckFlags::Used)) {
      return false;
    }
  }
  return true;
}

/// Common preconditions for Equatable and Hashable.
static bool canDeriveConformance(TypeChecker &tc, NominalTypeDecl *target,
                                 ProtocolDecl *protocol) {
  // The type must be an enum or a struct.
  if (auto enumDecl = dyn_cast<EnumDecl>(target)) {
    // The enum must have cases.
    if (!enumDecl->hasCases())
      return false;

    // The cases must not have associated values, or all associated values must
    // conform to the protocol.
    return allAssociatedValuesConformToProtocol(tc, enumDecl, protocol);
  }

  if (auto structDecl = dyn_cast<StructDecl>(target)) {
    // All stored properties of the struct must conform to the protocol.
    return allStoredPropertiesConformToProtocol(tc, structDecl, protocol);
  }

  return false;
}

/// Creates a named variable based on a prefix character and a numeric index.
/// \p prefixChar The prefix character for the variable's name.
/// \p index The numeric index to append to the variable's name.
/// \p type The type of the variable.
/// \p varContext The context of the variable.
/// \return A VarDecl named with the prefix and number.
static VarDecl *indexedVarDecl(char prefixChar, int index, Type type,
                               DeclContext *varContext) {
  ASTContext &C = varContext->getASTContext();

  llvm::SmallString<8> indexVal;
  indexVal.append(1, prefixChar);
  APInt(32, index).toString(indexVal, 10, /*signed*/ false);
  auto indexStr = C.AllocateCopy(indexVal);
  auto indexStrRef = StringRef(indexStr.data(), indexStr.size());

  auto varDecl = new (C) VarDecl(/*IsStatic*/false, VarDecl::Specifier::Let,
                                 /*IsCaptureList*/true, SourceLoc(),
                                 C.getIdentifier(indexStrRef), type,
                                 varContext);
  varDecl->setHasNonPatternBindingInit(true);
  return varDecl;
}

/// Returns the pattern used to match and bind the associated values (if any) of
/// an enum case.
/// \p enumElementDecl The enum element to match.
/// \p varPrefix The prefix character for variable names (e.g., a0, a1, ...).
/// \p varContext The context into which payload variables should be declared.
/// \p boundVars The array to which the pattern's variables will be appended.
static Pattern*
enumElementPayloadSubpattern(EnumElementDecl *enumElementDecl,
                             char varPrefix, DeclContext *varContext,
                             SmallVectorImpl<VarDecl*> &boundVars) {
  auto parentDC = enumElementDecl->getDeclContext();
  ASTContext &C = parentDC->getASTContext();

  auto argumentTypeLoc = enumElementDecl->getArgumentTypeLoc();
  if (argumentTypeLoc.isNull())
    // No arguments, so no subpattern to match.
    return nullptr;

  auto argumentType = argumentTypeLoc.getType();
  if (auto tupleType = argumentType->getAs<TupleType>()) {
    // Either multiple (labeled or unlabeled) arguments, or one labeled
    // argument. Return a tuple pattern that matches the enum element in arity,
    // types, and labels. For example:
    // case a(x: Int) => (x: let a0)
    // case b(Int, String) => (let a0, let a1)
    SmallVector<TuplePatternElt, 3> elementPatterns;
    int index = 0;
    for (auto tupleElement : tupleType->getElements()) {
      auto payloadVar = indexedVarDecl(varPrefix, index++,
                                       tupleElement.getType(), varContext);
      boundVars.push_back(payloadVar);

      auto namedPattern = new (C) NamedPattern(payloadVar);
      namedPattern->setImplicit();
      auto letPattern = new (C) VarPattern(SourceLoc(), /*isLet*/ true,
                                           namedPattern);
      elementPatterns.push_back(TuplePatternElt(tupleElement.getName(),
                                                SourceLoc(), letPattern));
    }

    auto pat = TuplePattern::create(C, SourceLoc(), elementPatterns,
                                    SourceLoc());
    pat->setImplicit();
    return pat;
  }

  // Otherwise, a one-argument unlabeled payload. Return a paren pattern whose
  // underlying type is the same as the payload. For example:
  // case a(Int) => (let a0)
  auto underlyingType = argumentType->getWithoutParens();
  auto payloadVar = indexedVarDecl(varPrefix, 0, underlyingType, varContext);
  boundVars.push_back(payloadVar);

  auto namedPattern = new (C) NamedPattern(payloadVar);
  namedPattern->setImplicit();
  auto letPattern = new (C) VarPattern(SourceLoc(), /*isLet*/ true,
                                       namedPattern);
  auto pat = new (C) ParenPattern(SourceLoc(), letPattern, SourceLoc());
  pat->setImplicit();
  return pat;
}

/// Create AST statements which convert from an enum to an Int with a switch.
/// \p stmts The generated statements are appended to this vector.
/// \p parentDC Either an extension or the enum itself.
/// \p enumDecl The enum declaration.
/// \p enumVarDecl The enum input variable.
/// \p funcDecl The parent function.
/// \p indexName The name of the output variable.
/// \return A DeclRefExpr of the output variable (of type Int).
static DeclRefExpr *convertEnumToIndex(SmallVectorImpl<ASTNode> &stmts,
                                       DeclContext *parentDC,
                                       EnumDecl *enumDecl,
                                       VarDecl *enumVarDecl,
                                       AbstractFunctionDecl *funcDecl,
                                       const char *indexName) {
  ASTContext &C = enumDecl->getASTContext();
  Type enumType = enumVarDecl->getType();
  Type intType = C.getIntDecl()->getDeclaredType();

  auto indexVar = new (C) VarDecl(/*IsStatic*/false, VarDecl::Specifier::Var,
                                  /*IsCaptureList*/false, SourceLoc(),
                                  C.getIdentifier(indexName), intType,
                                  funcDecl);
  indexVar->setInterfaceType(intType);
  indexVar->setImplicit();

  // generate: var indexVar
  Pattern *indexPat = new (C) NamedPattern(indexVar, /*implicit*/ true);
  indexPat->setType(intType);
  indexPat = new (C) TypedPattern(indexPat, TypeLoc::withoutLoc(intType));
  indexPat->setType(intType);
  auto indexBind = PatternBindingDecl::create(C, SourceLoc(),
                                              StaticSpellingKind::None,
                                              SourceLoc(),
                                              indexPat, nullptr, funcDecl);

  unsigned index = 0;
  SmallVector<ASTNode, 4> cases;
  for (auto elt : enumDecl->getAllElements()) {
    // generate: case .<Case>:
    auto pat = new (C) EnumElementPattern(TypeLoc::withoutLoc(enumType),
                                          SourceLoc(), SourceLoc(),
                                          Identifier(), elt, nullptr);
    pat->setImplicit();

    auto labelItem = CaseLabelItem(/*IsDefault=*/false, pat, SourceLoc(),
                                   nullptr);

    // generate: indexVar = <index>
    llvm::SmallString<8> indexVal;
    APInt(32, index++).toString(indexVal, 10, /*signed*/ false);
    auto indexStr = C.AllocateCopy(indexVal);

    auto indexExpr = new (C) IntegerLiteralExpr(StringRef(indexStr.data(),
                                                indexStr.size()), SourceLoc(),
                                                /*implicit*/ true);
    auto indexRef = new (C) DeclRefExpr(indexVar, DeclNameLoc(),
                                        /*implicit*/true);
    auto assignExpr = new (C) AssignExpr(indexRef, SourceLoc(),
                                         indexExpr, /*implicit*/ true);
    auto body = BraceStmt::create(C, SourceLoc(), ASTNode(assignExpr),
                                  SourceLoc());
    cases.push_back(CaseStmt::create(C, SourceLoc(), labelItem,
                                     /*HasBoundDecls=*/false,
                                     SourceLoc(), body));
  }

  // generate: switch enumVar { }
  auto enumRef = new (C) DeclRefExpr(enumVarDecl, DeclNameLoc(),
                                     /*implicit*/true);
  auto switchStmt = SwitchStmt::create(LabeledStmtInfo(), SourceLoc(), enumRef,
                                       SourceLoc(), cases, SourceLoc(), C);

  stmts.push_back(indexBind);
  stmts.push_back(switchStmt);

  return new (C) DeclRefExpr(indexVar, DeclNameLoc(), /*implicit*/ true,
                             AccessSemantics::Ordinary, intType);
}

/// Generates a guard statement that checks whether the given lhs and rhs
/// variables are equal; if they are not, then the isEqual variable is set to
/// false and a break statement is executed.
/// \p C The AST context.
/// \p lhsVar The first variable to test for equality.
/// \p rhsVar The second variable to test for equality.
/// \p isEqualVar The variable to set to false if the guard condition fails.
static GuardStmt *returnIfNotEqualGuard(ASTContext &C,
                                        Expr *lhsExpr,
                                        Expr *rhsExpr) {
  SmallVector<StmtConditionElement, 1> conditions;
  SmallVector<ASTNode, 2> statements;

  // First, generate the statements for the body of the guard.
  // return false
  auto falseExpr = new (C) BooleanLiteralExpr(false, SourceLoc(),
                                              /*Implicit*/true);
  auto returnStmt = new (C) ReturnStmt(SourceLoc(), falseExpr);
  statements.emplace_back(ASTNode(returnStmt));

  // Next, generate the condition being checked.
  // lhs == rhs
  auto cmpFuncExpr = new (C) UnresolvedDeclRefExpr(
    DeclName(C.getIdentifier("==")), DeclRefKind::BinaryOperator,
    DeclNameLoc());
  auto cmpArgsTuple = TupleExpr::create(C, SourceLoc(),
                                        { lhsExpr, rhsExpr },
                                        { }, { }, SourceLoc(),
                                        /*HasTrailingClosure*/false,
                                        /*Implicit*/true);
  auto cmpExpr = new (C) BinaryExpr(cmpFuncExpr, cmpArgsTuple,
                                    /*Implicit*/true);
  conditions.emplace_back(cmpExpr);

  // Build and return the complete guard statement.
  // guard lhs == rhs else { return false }
  auto body = BraceStmt::create(C, SourceLoc(), statements, SourceLoc());
  return new (C) GuardStmt(SourceLoc(), C.AllocateCopy(conditions), body);
}

/// Derive the body for an '==' operator for an enum that has no associated
/// values. This generates code that converts each value to its integer ordinal
/// and compares them, which produces an optimal single icmp instruction.
static void
deriveBodyEquatable_enum_noAssociatedValues_eq(AbstractFunctionDecl *eqDecl) {
  auto parentDC = eqDecl->getDeclContext();
  ASTContext &C = parentDC->getASTContext();

  auto args = eqDecl->getParameterLists().back();
  auto aParam = args->get(0);
  auto bParam = args->get(1);

  auto enumDecl = cast<EnumDecl>(aParam->getType()->getAnyNominal());

  // Generate the conversion from the enums to integer indices.
  SmallVector<ASTNode, 6> statements;
  DeclRefExpr *aIndex = convertEnumToIndex(statements, parentDC, enumDecl,
                                           aParam, eqDecl, "index_a");
  DeclRefExpr *bIndex = convertEnumToIndex(statements, parentDC, enumDecl,
                                           bParam, eqDecl, "index_b");

  // Generate the compare of the indices.
  FuncDecl *cmpFunc = C.getEqualIntDecl();
  assert(cmpFunc && "should have a == for int as we already checked for it");

  auto fnType = cmpFunc->getInterfaceType()->castTo<FunctionType>();

  Expr *cmpFuncExpr;
  if (cmpFunc->getDeclContext()->isTypeContext()) {
    auto contextTy = cmpFunc->getDeclContext()->getSelfInterfaceType();
    Expr *base = TypeExpr::createImplicitHack(SourceLoc(), contextTy, C);
    Expr *ref = new (C) DeclRefExpr(cmpFunc, DeclNameLoc(), /*Implicit*/ true,
                                    AccessSemantics::Ordinary, fnType);

    fnType = fnType->getResult()->castTo<FunctionType>();
    cmpFuncExpr = new (C) DotSyntaxCallExpr(ref, SourceLoc(), base, fnType);
    cmpFuncExpr->setImplicit();
  } else {
    cmpFuncExpr = new (C) DeclRefExpr(cmpFunc, DeclNameLoc(),
                                      /*implicit*/ true,
                                      AccessSemantics::Ordinary,
                                      fnType);
  }

  TupleExpr *abTuple = TupleExpr::create(C, SourceLoc(), { aIndex, bIndex },
                                         { }, { }, SourceLoc(),
                                         /*HasTrailingClosure*/ false,
                                         /*Implicit*/ true);

  auto *cmpExpr = new (C) BinaryExpr(cmpFuncExpr, abTuple, /*implicit*/ true);
  statements.push_back(new (C) ReturnStmt(SourceLoc(), cmpExpr));

  BraceStmt *body = BraceStmt::create(C, SourceLoc(), statements, SourceLoc());
  eqDecl->setBody(body);
}

/// Derive the body for an '==' operator for an enum where at least one of the
/// cases has associated values.
static void
deriveBodyEquatable_enum_hasAssociatedValues_eq(AbstractFunctionDecl *eqDecl) {
  auto parentDC = eqDecl->getDeclContext();
  ASTContext &C = parentDC->getASTContext();

  auto args = eqDecl->getParameterLists().back();
  auto aParam = args->get(0);
  auto bParam = args->get(1);

  Type enumType = aParam->getType();
  auto enumDecl = cast<EnumDecl>(aParam->getType()->getAnyNominal());

  SmallVector<ASTNode, 6> statements;
  SmallVector<ASTNode, 4> cases;
  unsigned elementCount = 0;

  // For each enum element, generate a case statement matching a pair containing
  // the same case, binding variables for the left- and right-hand associated
  // values.
  for (auto elt : enumDecl->getAllElements()) {
    elementCount++;

    // .<elt>(let l0, let l1, ...)
    SmallVector<VarDecl*, 3> lhsPayloadVars;
    auto lhsSubpattern = enumElementPayloadSubpattern(elt, 'l', eqDecl,
                                                      lhsPayloadVars);
    auto lhsElemPat = new (C) EnumElementPattern(TypeLoc::withoutLoc(enumType),
                                                 SourceLoc(), SourceLoc(),
                                                 Identifier(), elt,
                                                 lhsSubpattern);
    lhsElemPat->setImplicit();

    // .<elt>(let r0, let r1, ...)
    SmallVector<VarDecl*, 3> rhsPayloadVars;
    auto rhsSubpattern = enumElementPayloadSubpattern(elt, 'r', eqDecl,
                                                      rhsPayloadVars);
    auto rhsElemPat = new (C) EnumElementPattern(TypeLoc::withoutLoc(enumType),
                                                 SourceLoc(), SourceLoc(),
                                                 Identifier(), elt,
                                                 rhsSubpattern);
    rhsElemPat->setImplicit();

    auto hasBoundDecls = !lhsPayloadVars.empty();

    // case (.<elt>(let l0, let l1, ...), .<elt>(let r0, let r1, ...))
    auto caseTuplePattern = TuplePattern::create(C, SourceLoc(), {
      TuplePatternElt(lhsElemPat), TuplePatternElt(rhsElemPat) },
                                                 SourceLoc());
    caseTuplePattern->setImplicit();

    auto labelItem = CaseLabelItem(/*IsDefault*/ false, caseTuplePattern,
                                   SourceLoc(), nullptr);

    // Generate a guard statement for each associated value in the payload,
    // breaking out early if any pair is unequal. (This is done to avoid
    // constructing long lists of autoclosure-wrapped conditions connected by
    // &&, which the type checker has more difficulty processing.)
    SmallVector<ASTNode, 6> statementsInCase;
    for (size_t varIdx = 0; varIdx < lhsPayloadVars.size(); varIdx++) {
      auto lhsVar = lhsPayloadVars[varIdx];
      auto lhsExpr = new (C) DeclRefExpr(lhsVar, DeclNameLoc(),
                                         /*implicit*/true);
      auto rhsVar = rhsPayloadVars[varIdx];
      auto rhsExpr = new (C) DeclRefExpr(rhsVar, DeclNameLoc(),
                                         /*Implicit*/true);
      auto guardStmt = returnIfNotEqualGuard(C, lhsExpr, rhsExpr);
      statementsInCase.emplace_back(guardStmt);
    }

    // If none of the guard statements caused an early exit, then all the pairs
    // were true.
    // return true
    auto trueExpr = new (C) BooleanLiteralExpr(true, SourceLoc(),
                                               /*Implicit*/true);
    auto returnStmt = new (C) ReturnStmt(SourceLoc(), trueExpr);
    statementsInCase.push_back(returnStmt);

    auto body = BraceStmt::create(C, SourceLoc(), statementsInCase,
                                  SourceLoc());
    cases.push_back(CaseStmt::create(C, SourceLoc(), labelItem, hasBoundDecls,
                                     SourceLoc(), body));
  }

  // default: result = false
  //
  // We only generate this if the enum has more than one case. If it has exactly
  // one case, then that single case statement is already exhaustive.
  if (elementCount > 1) {
    auto defaultPattern = new (C) AnyPattern(SourceLoc());
    defaultPattern->setImplicit();
    auto defaultItem = CaseLabelItem(/*IsDefault*/ true, defaultPattern,
                                     SourceLoc(), nullptr);
    auto falseExpr = new (C) BooleanLiteralExpr(false, SourceLoc(),
                                                /*implicit*/ true);
    auto returnStmt = new (C) ReturnStmt(SourceLoc(), falseExpr);
    auto body = BraceStmt::create(C, SourceLoc(), ASTNode(returnStmt),
                                  SourceLoc());
    cases.push_back(CaseStmt::create(C, SourceLoc(), defaultItem,
                                     /*HasBoundDecls*/ false,
                                     SourceLoc(), body));
  }

  // switch (a, b) { <case statements> }
  auto aRef = new (C) DeclRefExpr(aParam, DeclNameLoc(), /*implicit*/true);
  auto bRef = new (C) DeclRefExpr(bParam, DeclNameLoc(), /*implicit*/true);
  auto abExpr = TupleExpr::create(C, SourceLoc(), { aRef, bRef }, {}, {},
                                  SourceLoc(), /*HasTrailingClosure*/ false,
                                  /*implicit*/ true);
  auto switchStmt = SwitchStmt::create(LabeledStmtInfo(), SourceLoc(), abExpr,
                                       SourceLoc(), cases, SourceLoc(), C);
  statements.push_back(switchStmt);

  auto body = BraceStmt::create(C, SourceLoc(), statements, SourceLoc());
  eqDecl->setBody(body);
}

/// Derive the body for an '==' operator for a struct.
static void deriveBodyEquatable_struct_eq(AbstractFunctionDecl *eqDecl) {
  auto parentDC = eqDecl->getDeclContext();
  ASTContext &C = parentDC->getASTContext();

  auto args = eqDecl->getParameterLists().back();
  auto aParam = args->get(0);
  auto bParam = args->get(1);

  auto structDecl = cast<StructDecl>(aParam->getType()->getAnyNominal());

  SmallVector<ASTNode, 6> statements;

  auto storedProperties =
    structDecl->getStoredProperties(/*skipInaccessible=*/true);

  // For each stored property element, generate a guard statement that returns
  // false if a property is not pairwise-equal.
  for (auto propertyDecl : storedProperties) {
    auto aPropertyRef = new (C) DeclRefExpr(propertyDecl, DeclNameLoc(),
                                            /*implicit*/ true);
    auto aParamRef = new (C) DeclRefExpr(aParam, DeclNameLoc(),
                                         /*implicit*/ true);
    auto aPropertyExpr = new (C) DotSyntaxCallExpr(aPropertyRef, SourceLoc(),
                                                   aParamRef);

    auto bPropertyRef = new (C) DeclRefExpr(propertyDecl, DeclNameLoc(),
                                            /*implicit*/ true);
    auto bParamRef = new (C) DeclRefExpr(bParam, DeclNameLoc(),
                                         /*implicit*/ true);
    auto bPropertyExpr = new (C) DotSyntaxCallExpr(bPropertyRef, SourceLoc(),
                                                   bParamRef);

    auto guardStmt = returnIfNotEqualGuard(C, aPropertyExpr, bPropertyExpr);
    statements.emplace_back(guardStmt);
  }

  // If none of the guard statements caused an early exit, then all the pairs
  // were true.
  // return true
  auto trueExpr = new (C) BooleanLiteralExpr(true, SourceLoc(),
                                             /*Implicit*/true);
  auto returnStmt = new (C) ReturnStmt(SourceLoc(), trueExpr);
  statements.push_back(returnStmt);

  auto body = BraceStmt::create(C, SourceLoc(), statements, SourceLoc());
  eqDecl->setBody(body);
}

/// Derive an '==' operator implementation for an enum or a struct.
static ValueDecl *
deriveEquatable_eq(TypeChecker &tc, Decl *parentDecl, NominalTypeDecl *typeDecl,
                   Identifier generatedIdentifier,
                   void (*bodySynthesizer)(AbstractFunctionDecl *)) {
  // enum SomeEnum<T...> {
  //   case A, B(Int), C(String, Int)
  //
  //   @derived
  //   @_implements(Equatable, ==(_:_:))
  //   func __derived_enum_equals(a: SomeEnum<T...>,
  //                              b: SomeEnum<T...>) -> Bool {
  //     switch (a, b) {
  //     case (.A, .A):
  //       return true
  //     case (.B(let l0), .B(let r0)):
  //       guard l0 == r0 else { return false }
  //       return true
  //     case (.C(let l0, let l1), .C(let r0, let r1)):
  //       guard l0 == r0 else { return false }
  //       guard l1 == r1 else { return false }
  //       return true
  //     default: return false
  //   }
  // }
  //
  // struct SomeStruct<T...> {
  //   var x: Int
  //   var y: String
  //
  //   @derived
  //   @_implements(Equatable, ==(_:_:))
  //   func __derived_struct_equals(a: SomeStruct<T...>,
  //                                b: SomeStruct<T...>) -> Bool {
  //     guard a.x == b.x else { return false; }
  //     guard a.y == b.y else { return false; }
  //     return true;
  //   }
  // }

  ASTContext &C = tc.Context;

  auto parentDC = cast<DeclContext>(parentDecl);
  auto enumTy = parentDC->getDeclaredTypeInContext();
  auto enumIfaceTy = parentDC->getDeclaredInterfaceType();

  auto getParamDecl = [&](StringRef s) -> ParamDecl* {
    auto *param = new (C) ParamDecl(VarDecl::Specifier::Owned, SourceLoc(), SourceLoc(),
                                    Identifier(), SourceLoc(), C.getIdentifier(s),
                                    enumTy, parentDC);
    param->setInterfaceType(enumIfaceTy);
    return param;
  };

  auto selfDecl = ParamDecl::createSelf(SourceLoc(), parentDC,
                                        /*isStatic=*/true);

  ParameterList *params[] = {
    ParameterList::createWithoutLoc(selfDecl),
    ParameterList::create(C, {
        getParamDecl("a"),
        getParamDecl("b")
    })
  };

  auto boolTy = C.getBoolDecl()->getDeclaredType();

  DeclName name(C, generatedIdentifier, params[1]);
  auto eqDecl =
    FuncDecl::create(C, /*StaticLoc=*/SourceLoc(),
                     StaticSpellingKind::KeywordStatic,
                     /*FuncLoc=*/SourceLoc(), name, /*NameLoc=*/SourceLoc(),
                     /*Throws=*/false, /*ThrowsLoc=*/SourceLoc(),
                     /*GenericParams=*/nullptr,
                     params,
                     TypeLoc::withoutLoc(boolTy),
                     parentDC);
  eqDecl->setImplicit();
  eqDecl->setUserAccessible(false);
  eqDecl->getAttrs().add(new (C) InfixAttr(/*implicit*/false));

  // Add the @_implements(Equatable, ==(_:_:)) attribute
  auto equatableProto = C.getProtocol(KnownProtocolKind::Equatable);
  auto equatableTy = equatableProto->getDeclaredType();
  auto equatableTypeLoc = TypeLoc::withoutLoc(equatableTy);
  SmallVector<Identifier, 2> argumentLabels = { Identifier(), Identifier() };
  auto equalsDeclName = DeclName(C, DeclBaseName(C.Id_EqualsOperator),
                                 argumentLabels);
  eqDecl->getAttrs().add(new (C) ImplementsAttr(SourceLoc(),
                                                SourceRange(),
                                                equatableTypeLoc,
                                                equalsDeclName,
                                                DeclNameLoc()));

  if (!C.getEqualIntDecl()) {
    tc.diagnose(parentDecl->getLoc(), diag::no_equal_overload_for_int);
    return nullptr;
  }

  eqDecl->setBodySynthesizer(bodySynthesizer);

  // Compute the type.
  Type paramsTy = params[1]->getType(tc.Context);

  // Compute the interface type.
  Type interfaceTy;
  auto selfParam = computeSelfParam(eqDecl);
  if (auto genericSig = parentDC->getGenericSignatureOfContext()) {
    eqDecl->setGenericEnvironment(parentDC->getGenericEnvironmentOfContext());

    Type enumIfaceTy = parentDC->getDeclaredInterfaceType();
    TupleTypeElt ifaceParamElts[] = {
      enumIfaceTy, enumIfaceTy,
    };
    auto ifaceParamsTy = TupleType::get(ifaceParamElts, C);
    interfaceTy = FunctionType::get(ifaceParamsTy, boolTy,
                                    AnyFunctionType::ExtInfo());
    interfaceTy = GenericFunctionType::get(genericSig, {selfParam}, interfaceTy,
                                           AnyFunctionType::ExtInfo());
  } else {
    interfaceTy = FunctionType::get(paramsTy, boolTy);
    interfaceTy = FunctionType::get({selfParam}, interfaceTy,
                                    FunctionType::ExtInfo());
  }
  eqDecl->setInterfaceType(interfaceTy);
  eqDecl->copyFormalAccessAndVersionedAttrFrom(typeDecl);

  // If the enum was not imported, the derived conformance is either from the
  // enum itself or an extension, in which case we will emit the declaration
  // normally.
  if (typeDecl->hasClangNode())
    tc.Context.addExternalDecl(eqDecl);

  // Add the operator to the parent scope.
  cast<IterableDeclContext>(parentDecl)->addMember(eqDecl);

  return eqDecl;
}

bool DerivedConformance::canDeriveEquatable(TypeChecker &tc,
                                            NominalTypeDecl *type,
                                            ValueDecl *requirement) {
  auto equatableProto = tc.Context.getProtocol(KnownProtocolKind::Equatable);
  return canDeriveConformance(tc, type, equatableProto);
}

ValueDecl *DerivedConformance::deriveEquatable(TypeChecker &tc,
                                               Decl *parentDecl,
                                               NominalTypeDecl *type,
                                               ValueDecl *requirement) {
  // Conformance can't be synthesized in an extension; we allow it as a special
  // case for enums with no associated values to preserve source compatibility.
  auto theEnum = dyn_cast<EnumDecl>(type);
  if (!(theEnum && theEnum->hasOnlyCasesWithoutAssociatedValues()) &&
      type != parentDecl) {
    auto equatableProto = tc.Context.getProtocol(KnownProtocolKind::Equatable);
    auto equatableType = equatableProto->getDeclaredType();
    tc.diagnose(parentDecl->getLoc(), diag::cannot_synthesize_in_extension,
                equatableType);
    return nullptr;
  }

  // Build the necessary decl.
  if (requirement->getBaseName() == "==") {
    if (theEnum) {
      auto bodySynthesizer =
          theEnum->hasOnlyCasesWithoutAssociatedValues()
              ? &deriveBodyEquatable_enum_noAssociatedValues_eq
              : &deriveBodyEquatable_enum_hasAssociatedValues_eq;
      return deriveEquatable_eq(tc, parentDecl, theEnum,
                                tc.Context.Id_derived_enum_equals,
                                bodySynthesizer);
    }
    else if (auto theStruct = dyn_cast<StructDecl>(type))
      return deriveEquatable_eq(tc, parentDecl, theStruct,
                                tc.Context.Id_derived_struct_equals,
                                &deriveBodyEquatable_struct_eq);
    else
      llvm_unreachable("todo");
  }
  tc.diagnose(requirement->getLoc(),
              diag::broken_equatable_requirement);
  return nullptr;
}

/// Returns a new integer literal expression with the given value.
/// \p C The AST context.
/// \p value The integer value.
/// \return The integer literal expression.
static Expr *integerLiteralExpr(ASTContext &C, int64_t value) {
  llvm::SmallString<8> integerVal;
  APInt(32, value).toString(integerVal, 10, /*signed*/ false);
  auto integerStr = C.AllocateCopy(integerVal);
  auto integerExpr = new (C) IntegerLiteralExpr(
    StringRef(integerStr.data(), integerStr.size()), SourceLoc(),
    /*implicit*/ true);
  return integerExpr;
}

/// Returns a new \c CallExpr representing
///
///   hasher.appending(hashable)
///
/// \param C The AST context to create the expression in.
///
/// \param hasher The base expression to make the call on.
///
/// \param hashable The parameter to the call.
static CallExpr *createHasherAppendingCall(ASTContext &C,
                                           Expr *hasher,
                                           Expr *hashable) {
  // hasher.appending(_:)
  DeclName name(C, C.Id_appending, {Identifier()});
  auto *appendingCall = new (C) UnresolvedDotExpr(hasher, SourceLoc(),
                                                  name, DeclNameLoc(),
                                                  /*implicit*/ true);
  
  // hasher.appending(hashable)
  return CallExpr::createImplicit(C, appendingCall, {hashable}, {Identifier()});
}

static FuncDecl *
deriveHashable_hashInto(TypeChecker &tc, Decl *parentDecl,
                        NominalTypeDecl *typeDecl,
                        void (*bodySynthesizer)(AbstractFunctionDecl *)) {
  // @derived func _hash(into hasher: _UnsafeHasher) -> _UnsafeHasher

  ASTContext &C = tc.Context;
  auto parentDC = cast<DeclContext>(parentDecl);

  // Expected type: (Self) -> (into: _UnsafeHasher) -> _UnsafeHasher
  // Constructed as:
  //   func type(input: Self,
  //             output: func type(input: _UnsafeHasher,
  //                               output: _UnsafeHasher))
  // Created from the inside out:

  Type hasherType = C.getUnsafeHasherDecl()->getDeclaredType();

  // Params: self (implicit), hasher
  auto *selfDecl = ParamDecl::createSelf(SourceLoc(), parentDC);
  auto *hasherParam = new (C) ParamDecl(VarDecl::Specifier::Owned, SourceLoc(),
                                        SourceLoc(), C.Id_into, SourceLoc(),
                                        C.Id_hasher, hasherType, parentDC);
  hasherParam->setInterfaceType(hasherType);

  ParameterList *params[] = {ParameterList::createWithoutLoc(selfDecl),
                             ParameterList::createWithoutLoc(hasherParam)};

  // Func name: _hash(into: _UnsafeHasher)
  DeclName name(C, C.Id_hash, params[1]);
  auto *hashDecl = FuncDecl::create(C,
                                    SourceLoc(), StaticSpellingKind::None,
                                    SourceLoc(), name, SourceLoc(),
                                    /*Throws=*/false, SourceLoc(),
                                    nullptr, params,
                                    TypeLoc::withoutLoc(hasherType),
                                    parentDC);
  hashDecl->setImplicit();
  hashDecl->setBodySynthesizer(bodySynthesizer);

  // Evaluate type of Self in (Self) -> (into: _UnsafeHasher) -> _UnsafeHasher
  auto selfParam = computeSelfParam(hashDecl);
  auto inputTypeElt = TupleTypeElt(hasherType, C.Id_into);
  auto inputType = TupleType::get({inputTypeElt}, C);
  auto innerType = FunctionType::get(inputType, hasherType,
                                     FunctionType::ExtInfo());

  Type interfaceType;
  if (auto sig = parentDC->getGenericSignatureOfContext()) {
    hashDecl->setGenericEnvironment(parentDC->getGenericEnvironmentOfContext());
    interfaceType = GenericFunctionType::get(sig, {selfParam}, innerType,
                                             FunctionType::ExtInfo());
  } else {
    // (Self) -> innerType == (_UnsafeHasher) -> _UnsafeHasher
    interfaceType = FunctionType::get({selfParam}, innerType,
                                      FunctionType::ExtInfo());
  }
  hashDecl->setInterfaceType(interfaceType);
  hashDecl->copyFormalAccessAndVersionedAttrFrom(typeDecl);

  // If we aren't synthesizing into an imported/derived type, the derived conformance is
  // either from the type itself or an extension, in which case we will emit the
  // declaration normally.
  //
  // We're checking for a source file here rather than the presence of a Clang
  // node because otherwise we wouldn't catch synthesized Hashable structs, like
  // SE-0112's imported Error types.
  if (!isa<SourceFile>(parentDC->getModuleScopeContext())) {
    C.addExternalDecl(hashDecl);
    // Assume the new function is already typechecked; TypeChecker::validateDecl
    // would otherwise reject it.
    hashDecl->setValidationStarted();
  }
 
  cast<IterableDeclContext>(parentDecl)->addMember(hashDecl);
  return hashDecl;
}

static bool
parentHasDerivedHashValueImplementation(AbstractFunctionDecl *hashIntoDecl) {
  ASTContext &C = hashIntoDecl->getASTContext();
  auto parentDC = hashIntoDecl->getDeclContext();
  auto decl = parentDC->getAsDeclOrDeclExtensionContext();
  for (auto member: cast<IterableDeclContext>(decl)->getMembers()) {
    if (auto varDecl = dyn_cast<VarDecl>(member)) {
      if (varDecl->getBaseName() == C.Id_hashValue) {
        return varDecl->isImplicit();
      }
    }
  }
  return false;
}

/// Derive the body for the _hash(into:) method when hashValue has a non-derived
/// implementation.
static void
deriveBodyHashable_compat_hashInto(AbstractFunctionDecl *hashIntoDecl) {
  // func _hash(into hasher: _UnsafeHasher) -> _UnsafeHasher {
  //   return hasher.appending(self.hashValue)
  // }
  auto parentDC = hashIntoDecl->getDeclContext();
  ASTContext &C = parentDC->getASTContext();

  auto selfDecl = hashIntoDecl->getImplicitSelfDecl();
  auto selfRef = new (C) DeclRefExpr(selfDecl, DeclNameLoc(),
                                     /*implicit*/ true);
  auto hashValueExpr = new (C) UnresolvedDotExpr(selfRef, SourceLoc(),
                                                 C.Id_hashValue, DeclNameLoc(),
                                                 /*implicit*/ true);
  auto hasherParam = hashIntoDecl->getParameterList(1)->get(0);
  auto hasherRef = new (C) DeclRefExpr(ConcreteDeclRef(hasherParam),
                                       DeclNameLoc(), /*implicit*/ true);
  auto hasherExpr = createHasherAppendingCall(C, hasherRef, hashValueExpr);

  auto returnStmt = new (C) ReturnStmt(SourceLoc(), hasherExpr);
  auto body = BraceStmt::create(C, SourceLoc(), {ASTNode(returnStmt)},
                                SourceLoc(), /*implicit*/ true);
  hashIntoDecl->setBody(body);
}

/// Derive the body for the '_hash(into:)' method for an enum.
static void
deriveBodyHashable_enum_hashInto(AbstractFunctionDecl *hashIntoDecl) {
  if (!parentHasDerivedHashValueImplementation(hashIntoDecl)) {
    deriveBodyHashable_compat_hashInto(hashIntoDecl);
    return;
  }
  // enum SomeEnum {
  //   case A, B, C
  //   @derived func _hash(into hasher: _UnsafeHasher) -> _UnsafeHasher {
  //     switch self {
  //     case A:
  //       return hasher.appending(0)
  //     case B:
  //       return hasher.appending(1)
  //     case C:
  //       return hasher.appending(2)
  //     }
  //   }
  // }
  //
  // enum SomeEnumWithAssociatedValues {
  //   case A, B(Int), C(String, Int)
  //   @derived func _hash(into hasher: _UnsafeHasher) -> _UnsafeHasher {
  //     switch self {
  //     case A:
  //       return hasher.appending(0)
  //     case B(let a0):
  //       return hasher.appending(1).appending(a0)
  //     case C(let a0, let a1):
  //       return hasher.appending(2).appending(a0).appending(a1)
  //     }
  //   }
  // }
  auto parentDC = hashIntoDecl->getDeclContext();
  ASTContext &C = parentDC->getASTContext();

  auto enumDecl = parentDC->getAsEnumOrEnumExtensionContext();
  auto selfDecl = hashIntoDecl->getImplicitSelfDecl();

  Type enumType = selfDecl->getType();

  // Extract the decl for the hasher parameter.
  auto hasherParam = hashIntoDecl->getParameterList(1)->get(0);

  unsigned index = 0;
  SmallVector<ASTNode, 4> cases;

  auto hasNoAssociatedValues = enumDecl->hasOnlyCasesWithoutAssociatedValues();

  // For each enum element, generate a case statement that binds the associated
  // values so that they can be fed to the hasher.
  for (auto elt : enumDecl->getAllElements()) {
    // case .<elt>(let a0, let a1, ...):
    SmallVector<VarDecl*, 3> payloadVars;

    // hasherExpr will hold the returned expression. It starts by the hasher
    // parameter, but we will add one or more chained method calls to it below.
    //
    // <hasherExpr> := hasher
    Expr* hasherExpr = new (C) DeclRefExpr(ConcreteDeclRef(hasherParam),
                                           DeclNameLoc(), /*implicit*/ true);

    auto payloadPattern = enumElementPayloadSubpattern(elt, 'a', hashIntoDecl,
                                                       payloadVars);
    auto pat = new (C) EnumElementPattern(TypeLoc::withoutLoc(enumType),
                                          SourceLoc(), SourceLoc(),
                                          elt->getName(), elt, payloadPattern);
    pat->setImplicit();

    auto labelItem = CaseLabelItem(/*IsDefault*/ false, pat, SourceLoc(),
                                   nullptr);

    // If the enum has no associated values, we use the ordinal alone as the
    // hash value, because that is sufficient for a good distribution. If any
    // case does have associated values, then the ordinal is used as the first
    // term fed into the hasher.

    // <hasherExpr> := <hasherExpr>.appending(<ordinal>)
    {
      auto ordinalExpr = integerLiteralExpr(C, index++);
      hasherExpr = createHasherAppendingCall(C, hasherExpr, ordinalExpr);
    }

    if (!hasNoAssociatedValues) {
      // Generate a sequence of expressions that combine the payload's hash
      // values into result.
      for (auto payloadVar : payloadVars) {
        auto payloadVarRef = new (C) DeclRefExpr(payloadVar, DeclNameLoc(),
                                                 /*implicit*/ true);
        // <hasherExpr> := <hasherExpr>.appending(<payloadVar>)
        hasherExpr = createHasherAppendingCall(C, hasherExpr, payloadVarRef);
      }
    }

    auto returnStmt = new (C) ReturnStmt(SourceLoc(), hasherExpr);
    auto hasBoundDecls = !payloadVars.empty();
    auto body = BraceStmt::create(C, SourceLoc(),
                                  {ASTNode(returnStmt)}, SourceLoc());
    cases.push_back(CaseStmt::create(C, SourceLoc(), labelItem, hasBoundDecls,
                                     SourceLoc(), body));
  }

  // generate: switch enumVar { }
  auto enumRef = new (C) DeclRefExpr(selfDecl, DeclNameLoc(),
                                     /*implicit*/true);
  auto switchStmt = SwitchStmt::create(LabeledStmtInfo(), SourceLoc(), enumRef,
                                       SourceLoc(), cases, SourceLoc(), C);

  auto body = BraceStmt::create(C, SourceLoc(), {ASTNode(switchStmt)}, SourceLoc());
  hashIntoDecl->setBody(body);
}

/// Derive the body for the '_hash(into:)' method for a struct.
static void
deriveBodyHashable_struct_hashInto(AbstractFunctionDecl *hashIntoDecl) {
  if (!parentHasDerivedHashValueImplementation(hashIntoDecl)) {
    deriveBodyHashable_compat_hashInto(hashIntoDecl);
    return;
  }
  // struct SomeStruct {
  //   var x: Int
  //   var y: String
  //   @derived func _hash(into hasher: _UnsafeHasher) -> _UnsafeHasher {
  //     return hasher.appending(x).appending(y)
  //   }
  // }
  auto parentDC = hashIntoDecl->getDeclContext();
  ASTContext &C = parentDC->getASTContext();

  auto structDecl = parentDC->getAsStructOrStructExtensionContext();
  auto selfDecl = hashIntoDecl->getImplicitSelfDecl();

  // Extract the decl for the hasher parameter.
  auto hasherParam = hashIntoDecl->getParameterList(1)->get(0);

  // hasherExpr will hold the returned expression. It starts by the hasher
  // parameter, but we will add one or more chained method calls to it below.
  //
  // <hasherExpr> := hasher
  Expr* hasherExpr = new (C) DeclRefExpr(hasherParam, DeclNameLoc(),
                                         /*implicit*/ true);

  auto storedProperties =
    structDecl->getStoredProperties(/*skipInaccessible=*/true);

  // Feed each stored property into the hasher.
  for (auto propertyDecl : storedProperties) {
    auto propertyRef = new (C) DeclRefExpr(propertyDecl, DeclNameLoc(),
                                           /*implicit*/ true);
    auto selfRef = new (C) DeclRefExpr(selfDecl, DeclNameLoc(),
                                       /*implicit*/ true);
    auto selfPropertyExpr = new (C) DotSyntaxCallExpr(propertyRef, SourceLoc(),
                                                      selfRef);
    // <hasherExpr> := <hasherExpr>.appending(self.<property>)
    hasherExpr = createHasherAppendingCall(C, hasherExpr, selfPropertyExpr);
  }

  auto returnStmt = new (C) ReturnStmt(SourceLoc(), hasherExpr);
  auto body = BraceStmt::create(C, SourceLoc(), {ASTNode(returnStmt)},
                                SourceLoc(), /*implicit*/ true);
  hashIntoDecl->setBody(body);
}

/// Derive the body for the '_hash(into:)' method for a class.
static void
deriveBodyHashable_class_hashInto(AbstractFunctionDecl *hashIntoDecl) {
  if (parentHasDerivedHashValueImplementation(hashIntoDecl)) {
    llvm_unreachable("Derived hashValue into a class.");
  }
  deriveBodyHashable_compat_hashInto(hashIntoDecl);
}

/// Derive the body for the 'hashValue' getter.
static void
deriveBodyHashable_hashValue(AbstractFunctionDecl *hashValueDecl) {
  auto parentDC = hashValueDecl->getDeclContext();
  ASTContext &C = parentDC->getASTContext();

  // return _hashValue(for: self)
  auto *hashFunc = C.getHashValueForDecl();
  auto hashExpr = new (C) DeclRefExpr(hashFunc, DeclNameLoc(),
                                      /*implicit*/ true);
  auto selfDecl = hashValueDecl->getImplicitSelfDecl();
  auto selfRef = new (C) DeclRefExpr(selfDecl, DeclNameLoc(),
                                     /*implicit*/ true);
  auto callExpr = CallExpr::createImplicit(C, hashExpr,
                                           { selfRef }, { C.Id_for });
  auto returnStmt = new (C) ReturnStmt(SourceLoc(), callExpr);

  auto body = BraceStmt::create(C, SourceLoc(), {returnStmt}, SourceLoc(),
                                /*implicit*/ true);
  hashValueDecl->setBody(body);
}

/// Derive a 'hashValue' implementation.
static ValueDecl *
deriveHashable_hashValue(TypeChecker &tc, Decl *parentDecl,
                         NominalTypeDecl *typeDecl) {
  // @derived var hashValue: Int {
  //   return _hashValue(for: self)
  // }
  ASTContext &C = tc.Context;

  auto parentDC = cast<DeclContext>(parentDecl);
  Type intType = C.getIntDecl()->getDeclaredType();

  // We can't form a Hashable conformance if Int isn't Hashable or
  // ExpressibleByIntegerLiteral.
  if (!tc.conformsToProtocol(intType,C.getProtocol(KnownProtocolKind::Hashable),
                             typeDecl, None)) {
    tc.diagnose(typeDecl->getLoc(), diag::broken_int_hashable_conformance);
    return nullptr;
  }

  ProtocolDecl *intLiteralProto =
      C.getProtocol(KnownProtocolKind::ExpressibleByIntegerLiteral);
  if (!tc.conformsToProtocol(intType, intLiteralProto, typeDecl, None)) {
    tc.diagnose(typeDecl->getLoc(),
                diag::broken_int_integer_literal_convertible_conformance);
    return nullptr;
  }

  VarDecl *hashValueDecl =
    new (C) VarDecl(/*IsStatic*/false, VarDecl::Specifier::Var,
                    /*IsCaptureList*/false, SourceLoc(),
                    C.Id_hashValue, intType, parentDC);

  auto selfDecl = ParamDecl::createSelf(SourceLoc(), parentDC);

  ParameterList *params[] = {
    ParameterList::createWithoutLoc(selfDecl),
    ParameterList::createEmpty(C)
  };

  AccessorDecl *getterDecl = AccessorDecl::create(C,
      /*FuncLoc=*/SourceLoc(), /*AccessorKeywordLoc=*/SourceLoc(),
      AccessorKind::IsGetter, AddressorKind::NotAddressor, hashValueDecl,
      /*StaticLoc=*/SourceLoc(), StaticSpellingKind::None,
      /*Throws=*/false, /*ThrowsLoc=*/SourceLoc(),
      /*GenericParams=*/nullptr, params,
      TypeLoc::withoutLoc(intType), parentDC);
  getterDecl->setImplicit();
  getterDecl->setBodySynthesizer(&deriveBodyHashable_hashValue);

  // Compute the type of hashValue().
  Type methodType = FunctionType::get(TupleType::getEmpty(tc.Context), intType);

  // Compute the interface type of hashValue().
  Type interfaceType;
  auto selfParam = computeSelfParam(getterDecl);
  if (auto sig = parentDC->getGenericSignatureOfContext()) {
    getterDecl->setGenericEnvironment(parentDC->getGenericEnvironmentOfContext());
    interfaceType = GenericFunctionType::get(sig, {selfParam}, methodType,
                                             AnyFunctionType::ExtInfo());
  } else
    interfaceType = FunctionType::get({selfParam}, methodType,
                                      AnyFunctionType::ExtInfo());

  getterDecl->setInterfaceType(interfaceType);
  getterDecl->copyFormalAccessAndVersionedAttrFrom(typeDecl);

  // If the enum was not imported, the derived conformance is either from the
  // enum itself or an extension, in which case we will emit the declaration
  // normally.
  if (typeDecl->hasClangNode())
    tc.Context.addExternalDecl(getterDecl);

  // Finish creating the property.
  hashValueDecl->setImplicit();
  hashValueDecl->setInterfaceType(intType);
  hashValueDecl->makeComputed(SourceLoc(), getterDecl,
                              nullptr, nullptr, SourceLoc());
  hashValueDecl->copyFormalAccessAndVersionedAttrFrom(typeDecl);

  Pattern *hashValuePat = new (C) NamedPattern(hashValueDecl, /*implicit*/true);
  hashValuePat->setType(intType);
  hashValuePat
    = new (C) TypedPattern(hashValuePat, TypeLoc::withoutLoc(intType),
                           /*implicit*/ true);
  hashValuePat->setType(intType);

  auto patDecl = PatternBindingDecl::create(C, SourceLoc(),
                                            StaticSpellingKind::None,
                                            SourceLoc(), hashValuePat, nullptr,
                                            parentDC);
  patDecl->setImplicit();

  auto dc = cast<IterableDeclContext>(parentDecl);
  dc->addMember(getterDecl);
  dc->addMember(hashValueDecl);
  dc->addMember(patDecl);
  return hashValueDecl;
}

bool DerivedConformance::canDeriveHashable(TypeChecker &tc,
                                           NominalTypeDecl *type,
                                           ValueDecl *requirement) {
  if (!isa<EnumDecl>(type) && !isa<StructDecl>(type) && !isa<ClassDecl>(type))
    return false;
  // FIXME: This is not actually correct. We cannot promise to always
  // provide a witness here in all cases. Unfortunately, figuring out
  // whether this is actually possible requires a parent decl context.
  // When the answer is no, DerivedConformance::deriveHashable will output
  // its own diagnostics.
  return true;
}

ValueDecl *DerivedConformance::deriveHashable(TypeChecker &tc,
                                              Decl *parentDecl,
                                              NominalTypeDecl *type,
                                              ValueDecl *requirement) {
  ASTContext &C = parentDecl->getASTContext();
  auto theEnum = dyn_cast<EnumDecl>(type);

  // var hashValue: Int
  if (requirement->getBaseName() == C.Id_hashValue) {
    auto hashableProto = C.getProtocol(KnownProtocolKind::Hashable);
    // Refuse to synthesize hashValue if type isn't a struct or enum, or if it
    // has non-Hashable stored properties/associated values.
    if (!canDeriveConformance(tc, type, hashableProto)) {
      tc.diagnose(parentDecl->getLoc(), diag::type_does_not_conform,
                  type->getDeclaredType(), hashableProto->getDeclaredType());
      return nullptr;
    }
    // hashValue can't be synthesized in an extension; we allow it as a special
    // case for enums with no associated values to preserve source
    // compatibility.
    if (!(theEnum && theEnum->hasOnlyCasesWithoutAssociatedValues()) &&
        type != parentDecl) {
      tc.diagnose(parentDecl->getLoc(), diag::cannot_synthesize_in_extension,
                  hashableProto->getDeclaredType());
      return nullptr;
    }
    return deriveHashable_hashValue(tc, parentDecl, type);
  }

  // Hashable._hash(into:)
  if (requirement->getBaseName() == C.Id_hash) {
    // We always allow _hash(into:) to be synthesized.  Its body changes
    // depending on whether hashValue was implemented explicitly, and whether
    // the type is a struct, an enum, or a class.
    if (theEnum)
      return deriveHashable_hashInto(tc, parentDecl, theEnum,
                                     &deriveBodyHashable_enum_hashInto);
    else if (auto theStruct = dyn_cast<StructDecl>(type))
      return deriveHashable_hashInto(tc, parentDecl, theStruct,
                                     &deriveBodyHashable_struct_hashInto);
    else if (auto theClass = dyn_cast<ClassDecl>(type))
      return deriveHashable_hashInto(tc, parentDecl, theClass,
                                     &deriveBodyHashable_class_hashInto);
    else
        llvm_unreachable("Attempt to derive Hashable for a type other "
                         "than a struct, enum or class");
  }

  tc.diagnose(requirement->getLoc(),
              diag::broken_hashable_requirement);
  return nullptr;
}

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

/// Returns true if, for every element of the given enum, it either has no
/// associated values or all of them conform to a protocol.
/// \p theEnum The enum whose elements and associated values should be checked.
/// \p protocol The protocol being requested.
/// \return True if all associated values of all elements of the enum conform.
static bool allAssociatedValuesConformToProtocol(DeclContext *DC,
                                                 EnumDecl *theEnum,
                                                 ProtocolDecl *protocol) {
  auto lazyResolver = DC->getASTContext().getLazyResolver();
  for (auto elt : theEnum->getAllElements()) {
    if (!elt->hasInterfaceType())
      lazyResolver->resolveDeclSignature(elt);

    auto PL = elt->getParameterList();
    if (!PL)
      continue;

    for (auto param : *PL) {
      auto type = param->getType()->mapTypeOutOfContext();
      if (!TypeChecker::conformsToProtocol(DC->mapTypeIntoContext(type),
                                           protocol, DC,
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
static bool allStoredPropertiesConformToProtocol(DeclContext *DC,
                                                 StructDecl *theStruct,
                                                 ProtocolDecl *protocol) {
  auto lazyResolver = DC->getASTContext().getLazyResolver();
  auto storedProperties =
    theStruct->getStoredProperties(/*skipInaccessible=*/true);
  for (auto propertyDecl : storedProperties) {
    if (!propertyDecl->hasType())
      lazyResolver->resolveDeclSignature(propertyDecl);
    if (!propertyDecl->hasType())
      return false;

    auto type = propertyDecl->getType()->mapTypeOutOfContext();
    if (!TypeChecker::conformsToProtocol(DC->mapTypeIntoContext(type),
                                         protocol, DC,
                                         ConformanceCheckFlags::Used)) {
      return false;
    }
  }
  return true;
}

/// Common preconditions for Equatable and Hashable.
static bool canDeriveConformance(DeclContext *DC,
                                 NominalTypeDecl *target,
                                 ProtocolDecl *protocol) {
  // The type must be an enum or a struct.
  if (auto enumDecl = dyn_cast<EnumDecl>(target)) {
    // The cases must not have associated values, or all associated values must
    // conform to the protocol.
    return allAssociatedValuesConformToProtocol(DC, enumDecl, protocol);
  }

  if (auto structDecl = dyn_cast<StructDecl>(target)) {
    // All stored properties of the struct must conform to the protocol.
    return allStoredPropertiesConformToProtocol(DC, structDecl, protocol);
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
                                 C.getIdentifier(indexStrRef),
                                 varContext);
  varDecl->setType(type);
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

  // No arguments, so no subpattern to match.
  if (!enumElementDecl->hasAssociatedValues())
    return nullptr;

  auto argumentType = enumElementDecl->getArgumentInterfaceType();
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
                                  C.getIdentifier(indexName),
                                  funcDecl);
  indexVar->setInterfaceType(intType);
  indexVar->setImplicit();

  // generate: var indexVar
  Pattern *indexPat = new (C) NamedPattern(indexVar, /*implicit*/ true);
  indexPat->setType(intType);
  indexPat = TypedPattern::createImplicit(C, indexPat, intType);
  indexPat->setType(intType);
  auto *indexBind = PatternBindingDecl::createImplicit(
      C, StaticSpellingKind::None, indexPat, /*InitExpr*/ nullptr, funcDecl);

  unsigned index = 0;
  SmallVector<ASTNode, 4> cases;
  for (auto elt : enumDecl->getAllElements()) {
    // generate: case .<Case>:
    auto pat = new (C) EnumElementPattern(TypeLoc::withoutLoc(enumType),
                                          SourceLoc(), SourceLoc(),
                                          Identifier(), elt, nullptr);
    pat->setImplicit();

    auto labelItem = CaseLabelItem(pat);

    // generate: indexVar = <index>
    auto indexExpr = IntegerLiteralExpr::createFromUnsigned(C, index++);
    auto indexRef = new (C) DeclRefExpr(indexVar, DeclNameLoc(),
                                        /*implicit*/true);
    auto assignExpr = new (C) AssignExpr(indexRef, SourceLoc(),
                                         indexExpr, /*implicit*/ true);
    auto body = BraceStmt::create(C, SourceLoc(), ASTNode(assignExpr),
                                  SourceLoc());
    cases.push_back(CaseStmt::create(C, SourceLoc(), labelItem,
                                     /*HasBoundDecls=*/false, SourceLoc(),
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

/// Returns a generated guard statement that checks whether the given lhs and
/// rhs expressions are equal. If not equal, the else block for the guard
/// returns false.
/// \p C The AST context.
/// \p lhsExpr The first expression to compare for equality.
/// \p rhsExpr The second expression to compare for equality.
static GuardStmt *returnIfNotEqualGuard(ASTContext &C,
                                        Expr *lhsExpr,
                                        Expr *rhsExpr) {
  SmallVector<StmtConditionElement, 1> conditions;
  SmallVector<ASTNode, 1> statements;

  // First, generate the statement for the body of the guard.
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

static void
deriveBodyEquatable_enum_uninhabited_eq(AbstractFunctionDecl *eqDecl) {
  auto parentDC = eqDecl->getDeclContext();
  ASTContext &C = parentDC->getASTContext();

  auto args = eqDecl->getParameters();
  auto aParam = args->get(0);
  auto bParam = args->get(1);

  assert(!cast<EnumDecl>(aParam->getType()->getAnyNominal())->hasCases());

  SmallVector<ASTNode, 1> statements;
  SmallVector<ASTNode, 0> cases;

  // switch (a, b) { }
  auto aRef = new (C) DeclRefExpr(aParam, DeclNameLoc(), /*implicit*/ true);
  auto bRef = new (C) DeclRefExpr(bParam, DeclNameLoc(), /*implicit*/ true);
  auto abExpr = TupleExpr::create(C, SourceLoc(), {aRef, bRef}, {}, {},
                                  SourceLoc(), /*HasTrailingClosure*/ false,
                                  /*implicit*/ true);
  auto switchStmt = SwitchStmt::create(LabeledStmtInfo(), SourceLoc(), abExpr,
                                       SourceLoc(), cases, SourceLoc(), C);
  statements.push_back(switchStmt);

  auto body = BraceStmt::create(C, SourceLoc(), statements, SourceLoc());
  eqDecl->setBody(body);
}

/// Derive the body for an '==' operator for an enum that has no associated
/// values. This generates code that converts each value to its integer ordinal
/// and compares them, which produces an optimal single icmp instruction.
static void
deriveBodyEquatable_enum_noAssociatedValues_eq(AbstractFunctionDecl *eqDecl) {
  auto parentDC = eqDecl->getDeclContext();
  ASTContext &C = parentDC->getASTContext();

  auto args = eqDecl->getParameters();
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

  auto args = eqDecl->getParameters();
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

    auto labelItem = CaseLabelItem(caseTuplePattern);

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
                                     SourceLoc(), SourceLoc(), body));
  }

  // default: result = false
  //
  // We only generate this if the enum has more than one case. If it has exactly
  // one case, then that single case statement is already exhaustive.
  if (elementCount > 1) {
    auto defaultPattern = new (C) AnyPattern(SourceLoc());
    defaultPattern->setImplicit();
    auto defaultItem = CaseLabelItem::getDefault(defaultPattern);
    auto falseExpr = new (C) BooleanLiteralExpr(false, SourceLoc(),
                                                /*implicit*/ true);
    auto returnStmt = new (C) ReturnStmt(SourceLoc(), falseExpr);
    auto body = BraceStmt::create(C, SourceLoc(), ASTNode(returnStmt),
                                  SourceLoc());
    cases.push_back(CaseStmt::create(C, SourceLoc(), defaultItem,
                                     /*HasBoundDecls*/ false,
                                     SourceLoc(), SourceLoc(), body));
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

  auto args = eqDecl->getParameters();
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
deriveEquatable_eq(DerivedConformance &derived,
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

  ASTContext &C = derived.TC.Context;

  auto parentDC = derived.getConformanceContext();
  auto selfIfaceTy = parentDC->getDeclaredInterfaceType();

  auto getParamDecl = [&](StringRef s) -> ParamDecl * {
    auto *param = new (C) ParamDecl(VarDecl::Specifier::Default, SourceLoc(),
                                    SourceLoc(), Identifier(), SourceLoc(),
                                    C.getIdentifier(s), parentDC);
    param->setInterfaceType(selfIfaceTy);
    return param;
  };

  ParameterList *params = ParameterList::create(C, {
    getParamDecl("a"),
    getParamDecl("b")
  });

  auto boolTy = C.getBoolDecl()->getDeclaredType();

  Identifier generatedIdentifier;
  if (parentDC->getParentModule()->getResilienceStrategy() ==
      ResilienceStrategy::Resilient) {
    generatedIdentifier = C.Id_EqualsOperator;
  } else if (selfIfaceTy->getEnumOrBoundGenericEnum()) {
    generatedIdentifier = C.Id_derived_enum_equals;
  } else {
    assert(selfIfaceTy->getStructOrBoundGenericStruct());
    generatedIdentifier = C.Id_derived_struct_equals;
  }

  DeclName name(C, generatedIdentifier, params);
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
  if (generatedIdentifier != C.Id_EqualsOperator) {
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
  }

  if (!C.getEqualIntDecl()) {
    derived.TC.diagnose(derived.ConformanceDecl->getLoc(),
                        diag::no_equal_overload_for_int);
    return nullptr;
  }

  eqDecl->setBodySynthesizer(bodySynthesizer);

  // Compute the interface type.
  if (auto genericEnv = parentDC->getGenericEnvironmentOfContext())
    eqDecl->setGenericEnvironment(genericEnv);
  eqDecl->computeType();

  eqDecl->copyFormalAccessFrom(derived.Nominal, /*sourceIsParentContext*/ true);
  eqDecl->setValidationToChecked();

  C.addSynthesizedDecl(eqDecl);

  // Add the operator to the parent scope.
  derived.addMembersToConformanceContext({eqDecl});

  return eqDecl;
}

bool DerivedConformance::canDeriveEquatable(DeclContext *DC,
                                            NominalTypeDecl *type) {
  ASTContext &ctx = DC->getASTContext();
  auto equatableProto = ctx.getProtocol(KnownProtocolKind::Equatable);
  if (!equatableProto) return false;
  return canDeriveConformance(DC, type, equatableProto);
}

ValueDecl *DerivedConformance::deriveEquatable(ValueDecl *requirement) {
  if (checkAndDiagnoseDisallowedContext(requirement))
    return nullptr;

  // Build the necessary decl.
  if (requirement->getBaseName() == "==") {
    if (auto ed = dyn_cast<EnumDecl>(Nominal)) {
      auto bodySynthesizer =
          !ed->hasCases()
              ? &deriveBodyEquatable_enum_uninhabited_eq
              : ed->hasOnlyCasesWithoutAssociatedValues()
                    ? &deriveBodyEquatable_enum_noAssociatedValues_eq
                    : &deriveBodyEquatable_enum_hasAssociatedValues_eq;
      return deriveEquatable_eq(*this, bodySynthesizer);
    } else if (isa<StructDecl>(Nominal))
      return deriveEquatable_eq(*this, &deriveBodyEquatable_struct_eq);
    else
      llvm_unreachable("todo");
  }
  TC.diagnose(requirement->getLoc(), diag::broken_equatable_requirement);
  return nullptr;
}

/// Returns a new \c CallExpr representing
///
///   hasher.combine(hashable)
///
/// \param C The AST context to create the expression in.
///
/// \param hasher The parameter decl to make the call on.
///
/// \param hashable The parameter to the call.
static CallExpr *createHasherCombineCall(ASTContext &C,
                                         ParamDecl *hasher,
                                         Expr *hashable) {
  Expr *hasherExpr = new (C) DeclRefExpr(ConcreteDeclRef(hasher),
                                         DeclNameLoc(), /*implicit*/ true);
  DeclName name(C, C.Id_combine, {Identifier()});
  // hasher.combine(_:)
  auto *combineCall = new (C) UnresolvedDotExpr(hasherExpr, SourceLoc(),
                                                name, DeclNameLoc(),
                                                /*implicit*/ true);
  
  // hasher.combine(hashable)
  return CallExpr::createImplicit(C, combineCall, {hashable}, {Identifier()});
}

static FuncDecl *
deriveHashable_hashInto(DerivedConformance &derived,
                        void (*bodySynthesizer)(AbstractFunctionDecl *)) {
  // @derived func hash(into hasher: inout Hasher)

  ASTContext &C = derived.TC.Context;
  auto parentDC = derived.getConformanceContext();

  // Expected type: (Self) -> (into: inout Hasher) -> ()
  // Constructed as:
  //   func type(input: Self,
  //             output: func type(input: inout Hasher,
  //                               output: ()))
  // Created from the inside out:

  auto hasherDecl = C.getHasherDecl();
  if (!hasherDecl) {
    auto hashableProto = C.getProtocol(KnownProtocolKind::Hashable);
    derived.TC.diagnose(hashableProto->getLoc(),
                        diag::broken_hashable_no_hasher);
    return nullptr;
  }
  Type hasherType = hasherDecl->getDeclaredType();

  // Params: self (implicit), hasher
  auto *hasherParamDecl = new (C) ParamDecl(VarDecl::Specifier::InOut,
                                            SourceLoc(),
                                            SourceLoc(), C.Id_into, SourceLoc(),
                                            C.Id_hasher, parentDC);
  hasherParamDecl->setInterfaceType(hasherType);

  ParameterList *params = ParameterList::createWithoutLoc(hasherParamDecl);

  // Return type: ()
  auto returnType = TupleType::getEmpty(C);

  // Func name: hash(into: inout Hasher) -> ()
  DeclName name(C, C.Id_hash, params);
  auto *hashDecl = FuncDecl::create(C,
                                    SourceLoc(), StaticSpellingKind::None,
                                    SourceLoc(), name, SourceLoc(),
                                    /*Throws=*/false, SourceLoc(),
                                    nullptr, params,
                                    TypeLoc::withoutLoc(returnType),
                                    parentDC);
  hashDecl->setImplicit();
  hashDecl->setBodySynthesizer(bodySynthesizer);

  if (auto env = parentDC->getGenericEnvironmentOfContext())
    hashDecl->setGenericEnvironment(env);
  hashDecl->computeType();
  hashDecl->copyFormalAccessFrom(derived.Nominal);
  hashDecl->setValidationToChecked();

  C.addSynthesizedDecl(hashDecl);

  derived.addMembersToConformanceContext({hashDecl});
  return hashDecl;
}

/// Derive the body for the hash(into:) method when hashValue has a
/// user-supplied implementation.
static void
deriveBodyHashable_compat_hashInto(AbstractFunctionDecl *hashIntoDecl) {
  // func hash(into hasher: inout Hasher) {
  //   hasher.combine(self.hashValue)
  // }
  auto parentDC = hashIntoDecl->getDeclContext();
  ASTContext &C = parentDC->getASTContext();

  auto selfDecl = hashIntoDecl->getImplicitSelfDecl();
  auto selfRef = new (C) DeclRefExpr(selfDecl, DeclNameLoc(),
                                     /*implicit*/ true);
  auto hashValueExpr = new (C) UnresolvedDotExpr(selfRef, SourceLoc(),
                                                 C.Id_hashValue, DeclNameLoc(),
                                                 /*implicit*/ true);
  auto hasherParam = hashIntoDecl->getParameters()->get(0);
  auto hasherExpr = createHasherCombineCall(C, hasherParam, hashValueExpr);

  auto body = BraceStmt::create(C, SourceLoc(), {ASTNode(hasherExpr)},
                                SourceLoc(), /*implicit*/ true);
  hashIntoDecl->setBody(body);
}

/// Derive the body for the 'hash(into:)' method for an enum by using its raw
/// value.
static void
deriveBodyHashable_enum_rawValue_hashInto(
  AbstractFunctionDecl *hashIntoDecl
) {
  // enum SomeEnum: Int {
  //   case A, B, C
  //   @derived func hash(into hasher: inout Hasher) {
  //     hasher.combine(self.rawValue)
  //   }
  // }
  ASTContext &C = hashIntoDecl->getASTContext();

  // generate: self.rawValue
  auto *selfRef = DerivedConformance::createSelfDeclRef(hashIntoDecl);
  auto *rawValueRef = new (C) UnresolvedDotExpr(selfRef, SourceLoc(),
                                                C.Id_rawValue, DeclNameLoc(),
                                                /*Implicit=*/true);

  // generate: hasher.combine(discriminator)
  auto hasherParam = hashIntoDecl->getParameters()->get(0);
  ASTNode combineStmt = createHasherCombineCall(C, hasherParam, rawValueRef);

  auto body = BraceStmt::create(C, SourceLoc(), combineStmt, SourceLoc(),
                                /*implicit*/ true);
  hashIntoDecl->setBody(body);
}

/// Derive the body for the 'hash(into:)' method for an enum without associated
/// values.
static void
deriveBodyHashable_enum_noAssociatedValues_hashInto(
  AbstractFunctionDecl *hashIntoDecl
) {
  // enum SomeEnum {
  //   case A, B, C
  //   @derived func hash(into hasher: inout Hasher) {
  //     let discriminator: Int
  //     switch self {
  //     case A:
  //       discriminator = 0
  //     case B:
  //       discriminator = 1
  //     case C:
  //       discriminator = 2
  //     }
  //     hasher.combine(discriminator)
  //   }
  // }
  auto parentDC = hashIntoDecl->getDeclContext();
  ASTContext &C = parentDC->getASTContext();

  auto enumDecl = parentDC->getSelfEnumDecl();
  auto selfDecl = hashIntoDecl->getImplicitSelfDecl();

  // generate: switch self {...}
  SmallVector<ASTNode, 3> stmts;
  auto discriminatorExpr = convertEnumToIndex(stmts, parentDC, enumDecl,
                                              selfDecl, hashIntoDecl,
                                              "discriminator");
  // generate: hasher.combine(discriminator)
  auto hasherParam = hashIntoDecl->getParameters()->get(0);
  auto combineStmt = createHasherCombineCall(C, hasherParam, discriminatorExpr);
  stmts.push_back(combineStmt);

  auto body = BraceStmt::create(C, SourceLoc(), stmts, SourceLoc(),
                                /*implicit*/ true);
  hashIntoDecl->setBody(body);
}

/// Derive the body for the 'hash(into:)' method for an enum with associated
/// values.
static void
deriveBodyHashable_enum_hasAssociatedValues_hashInto(
  AbstractFunctionDecl *hashIntoDecl
) {
  // enum SomeEnumWithAssociatedValues {
  //   case A, B(Int), C(String, Int)
  //   @derived func hash(into hasher: inout Hasher) {
  //     switch self {
  //     case A:
  //       hasher.combine(0)
  //     case B(let a0):
  //       hasher.combine(1)
  //       hasher.combine(a0)
  //     case C(let a0, let a1):
  //       hasher.combine(2)
  //       hasher.combine(a0)
  //       hasher.combine(a1)
  //     }
  //   }
  // }
  auto parentDC = hashIntoDecl->getDeclContext();
  ASTContext &C = parentDC->getASTContext();

  auto enumDecl = parentDC->getSelfEnumDecl();
  auto selfDecl = hashIntoDecl->getImplicitSelfDecl();

  Type enumType = selfDecl->getType();

  // Extract the decl for the hasher parameter.
  auto hasherParam = hashIntoDecl->getParameters()->get(0);

  unsigned index = 0;
  SmallVector<ASTNode, 4> cases;

  // For each enum element, generate a case statement that binds the associated
  // values so that they can be fed to the hasher.
  for (auto elt : enumDecl->getAllElements()) {
    // case .<elt>(let a0, let a1, ...):
    SmallVector<VarDecl*, 3> payloadVars;
    SmallVector<ASTNode, 3> statements;

    auto payloadPattern = enumElementPayloadSubpattern(elt, 'a', hashIntoDecl,
                                                       payloadVars);
    auto pat = new (C) EnumElementPattern(TypeLoc::withoutLoc(enumType),
                                          SourceLoc(), SourceLoc(),
                                          elt->getName(), elt, payloadPattern);
    pat->setImplicit();

    auto labelItem = CaseLabelItem(pat);

    // If the enum has no associated values, we use the ordinal as the single
    // hash component, because that is sufficient for a good distribution. If
    // any case does have associated values, then the ordinal is used as the
    // first term fed into the hasher.

    {
      // Generate: hasher.combine(<ordinal>)
      auto ordinalExpr = IntegerLiteralExpr::createFromUnsigned(C, index++);
      auto combineExpr = createHasherCombineCall(C, hasherParam, ordinalExpr);
      statements.emplace_back(ASTNode(combineExpr));
    }

    // Generate a sequence of statements that feed the payloads into hasher.
    for (auto payloadVar : payloadVars) {
      auto payloadVarRef = new (C) DeclRefExpr(payloadVar, DeclNameLoc(),
                                               /*implicit*/ true);
      // Generate: hasher.combine(<payloadVar>)
      auto combineExpr = createHasherCombineCall(C, hasherParam, payloadVarRef);
      statements.emplace_back(ASTNode(combineExpr));
    }

    auto hasBoundDecls = !payloadVars.empty();
    auto body = BraceStmt::create(C, SourceLoc(), statements, SourceLoc());
    cases.push_back(CaseStmt::create(C, SourceLoc(), labelItem, hasBoundDecls,
                                     SourceLoc(), SourceLoc(), body,
                                     /*implicit*/ true));
  }

  // generate: switch enumVar { }
  auto enumRef = new (C) DeclRefExpr(selfDecl, DeclNameLoc(),
                                     /*implicit*/true);
  auto switchStmt = SwitchStmt::create(LabeledStmtInfo(), SourceLoc(), enumRef,
                                       SourceLoc(), cases, SourceLoc(), C);

  auto body = BraceStmt::create(C, SourceLoc(), {ASTNode(switchStmt)},
                                SourceLoc());
  hashIntoDecl->setBody(body);
}

/// Derive the body for the 'hash(into:)' method for a struct.
static void
deriveBodyHashable_struct_hashInto(AbstractFunctionDecl *hashIntoDecl) {
  // struct SomeStruct {
  //   var x: Int
  //   var y: String
  //   @derived func hash(into hasher: inout Hasher) {
  //     hasher.combine(x)
  //     hasher.combine(y)
  //   }
  // }
  auto parentDC = hashIntoDecl->getDeclContext();
  ASTContext &C = parentDC->getASTContext();

  auto structDecl = parentDC->getSelfStructDecl();
  SmallVector<ASTNode, 6> statements;
  auto selfDecl = hashIntoDecl->getImplicitSelfDecl();

  // Extract the decl for the hasher parameter.
  auto hasherParam = hashIntoDecl->getParameters()->get(0);

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
    // Generate: hasher.combine(self.<property>)
    auto combineExpr = createHasherCombineCall(C, hasherParam, selfPropertyExpr);
    statements.emplace_back(ASTNode(combineExpr));
  }

  auto body = BraceStmt::create(C, SourceLoc(), statements,
                                SourceLoc(), /*implicit*/ true);
  hashIntoDecl->setBody(body);
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
static ValueDecl *deriveHashable_hashValue(DerivedConformance &derived) {
  // @derived var hashValue: Int {
  //   return _hashValue(for: self)
  // }
  auto &tc = derived.TC;
  ASTContext &C = tc.Context;

  auto parentDC = derived.getConformanceContext();
  Type intType = C.getIntDecl()->getDeclaredType();

  // We can't form a Hashable conformance if Int isn't Hashable or
  // ExpressibleByIntegerLiteral.
  if (!tc.conformsToProtocol(intType,
                             C.getProtocol(KnownProtocolKind::Hashable),
                             parentDC, None)) {
    tc.diagnose(derived.ConformanceDecl, diag::broken_int_hashable_conformance);
    return nullptr;
  }

  ProtocolDecl *intLiteralProto =
      C.getProtocol(KnownProtocolKind::ExpressibleByIntegerLiteral);
  if (!tc.conformsToProtocol(intType, intLiteralProto, parentDC, None)) {
    tc.diagnose(derived.ConformanceDecl,
                diag::broken_int_integer_literal_convertible_conformance);
    return nullptr;
  }

  VarDecl *hashValueDecl =
    new (C) VarDecl(/*IsStatic*/false, VarDecl::Specifier::Var,
                    /*IsCaptureList*/false, SourceLoc(),
                    C.Id_hashValue, parentDC);
  hashValueDecl->setType(intType);

  ParameterList *params = ParameterList::createEmpty(C);

  AccessorDecl *getterDecl = AccessorDecl::create(C,
      /*FuncLoc=*/SourceLoc(), /*AccessorKeywordLoc=*/SourceLoc(),
      AccessorKind::Get, hashValueDecl,
      /*StaticLoc=*/SourceLoc(), StaticSpellingKind::None,
      /*Throws=*/false, /*ThrowsLoc=*/SourceLoc(),
      /*GenericParams=*/nullptr, params,
      TypeLoc::withoutLoc(intType), parentDC);
  getterDecl->setImplicit();
  getterDecl->setBodySynthesizer(&deriveBodyHashable_hashValue);

  // Compute the interface type of hashValue().
  if (auto env = parentDC->getGenericEnvironmentOfContext())
    getterDecl->setGenericEnvironment(env);
  getterDecl->computeType();

  getterDecl->setValidationToChecked();
  getterDecl->copyFormalAccessFrom(derived.Nominal,
                                   /*sourceIsParentContext*/ true);

  // Finish creating the property.
  hashValueDecl->setImplicit();
  hashValueDecl->setInterfaceType(intType);
  hashValueDecl->setValidationToChecked();
  hashValueDecl->setAccessors(StorageImplInfo::getImmutableComputed(),
                              SourceLoc(), {getterDecl}, SourceLoc());
  hashValueDecl->copyFormalAccessFrom(derived.Nominal,
                                      /*sourceIsParentContext*/ true);

  Pattern *hashValuePat = new (C) NamedPattern(hashValueDecl, /*implicit*/true);
  hashValuePat->setType(intType);
  hashValuePat = TypedPattern::createImplicit(C, hashValuePat, intType);
  hashValuePat->setType(intType);

  auto *patDecl = PatternBindingDecl::createImplicit(
      C, StaticSpellingKind::None, hashValuePat, /*InitExpr*/ nullptr,
      parentDC);
  C.addSynthesizedDecl(hashValueDecl);
  C.addSynthesizedDecl(getterDecl);

  derived.addMembersToConformanceContext({getterDecl, hashValueDecl, patDecl});
  return hashValueDecl;
}

static ValueDecl *
getHashValueRequirement(ASTContext &C) {
  auto hashableProto = C.getProtocol(KnownProtocolKind::Hashable);
  for (auto member: hashableProto->getMembers()) {
    if (auto fd = dyn_cast<VarDecl>(member)) {
      if (fd->getBaseName() == C.Id_hashValue)
        return fd;
    }
  }
  return nullptr;
}

static ProtocolConformance *
getHashableConformance(Decl *parentDecl) {
  ASTContext &C = parentDecl->getASTContext();
  auto DC = cast<DeclContext>(parentDecl);
  auto hashableProto = C.getProtocol(KnownProtocolKind::Hashable);
  for (auto conformance: DC->getLocalConformances()) {
    if (conformance->getProtocol() == hashableProto) {
      return conformance;
    }
  }
  return nullptr;
}

bool DerivedConformance::canDeriveHashable(NominalTypeDecl *type) {
  if (!isa<EnumDecl>(type) && !isa<StructDecl>(type) && !isa<ClassDecl>(type))
    return false;
  // FIXME: This is not actually correct. We cannot promise to always
  // provide a witness here in all cases. Unfortunately, figuring out
  // whether this is actually possible requires a parent decl context.
  // When the answer is no, DerivedConformance::deriveHashable will output
  // its own diagnostics.
  return true;
}

ValueDecl *DerivedConformance::deriveHashable(ValueDecl *requirement) {
  ASTContext &C = ConformanceDecl->getASTContext();

  // var hashValue: Int
  if (requirement->getBaseName() == C.Id_hashValue) {
    // We always allow hashValue to be synthesized; invalid cases are diagnosed
    // during hash(into:) synthesis.
    return deriveHashable_hashValue(*this);
  }

  // Hashable.hash(into:)
  if (requirement->getBaseName() == C.Id_hash) {
    // Start by resolving hashValue conformance.
    auto hashValueReq = getHashValueRequirement(C);
    auto conformance = getHashableConformance(ConformanceDecl);
    auto hashValueDecl = conformance->getWitnessDecl(hashValueReq, &TC);
    if (!hashValueDecl) {
      // We won't derive hash(into:) if hashValue cannot be resolved.
      // The hashValue failure will produce a diagnostic elsewhere.
      return nullptr;
    }
    if (hashValueDecl && hashValueDecl->isImplicit()) {
      // Neither hashValue nor hash(into:) is explicitly defined; we need to do
      // a full Hashable derivation.
      
      // Refuse to synthesize Hashable if type isn't a struct or enum, or if it
      // has non-Hashable stored properties/associated values.
      auto hashableProto = C.getProtocol(KnownProtocolKind::Hashable);
      if (!canDeriveConformance(getConformanceContext(), Nominal,
                                hashableProto)) {
        TC.diagnose(ConformanceDecl->getLoc(), diag::type_does_not_conform,
                    Nominal->getDeclaredType(),
                    hashableProto->getDeclaredType());
        return nullptr;
      }

      if (checkAndDiagnoseDisallowedContext(requirement))
        return nullptr;

      if (auto ED = dyn_cast<EnumDecl>(Nominal)) {
        void (*bodySynthesizer)(AbstractFunctionDecl *);
        if (ED->isObjC())
          bodySynthesizer = deriveBodyHashable_enum_rawValue_hashInto;
        else if (ED->hasOnlyCasesWithoutAssociatedValues())
          bodySynthesizer = deriveBodyHashable_enum_noAssociatedValues_hashInto;
        else
          bodySynthesizer=deriveBodyHashable_enum_hasAssociatedValues_hashInto;
        return deriveHashable_hashInto(*this, bodySynthesizer);
      } else if (isa<StructDecl>(Nominal))
        return deriveHashable_hashInto(*this,
                                       &deriveBodyHashable_struct_hashInto);
      else // This should've been caught by canDeriveHashable above.
        llvm_unreachable("Attempt to derive Hashable for a type other "
                         "than a struct or enum");      
    } else {
      // We can always derive hash(into:) if hashValue has an explicit
      // implementation.
      return deriveHashable_hashInto(*this,
                                     &deriveBodyHashable_compat_hashInto);
    }
  }

  TC.diagnose(requirement->getLoc(), diag::broken_hashable_requirement);
  return nullptr;
}

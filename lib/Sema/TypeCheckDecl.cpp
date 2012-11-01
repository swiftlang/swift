//===--- TypeCheckDecl.cpp - Type Checking for Declarations ---------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2015 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See http://swift.org/LICENSE.txt for license information
// See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
// This file implements semantic analysis for declarations.
//
//===----------------------------------------------------------------------===//

#include "TypeChecker.h"
#include "ArchetypeBuilder.h"
#include "swift/AST/ASTVisitor.h"
#include "swift/AST/Attr.h"
#include "llvm/ADT/Twine.h"
using namespace swift;

namespace {

class DeclChecker : public DeclVisitor<DeclChecker> {
  
public:
  TypeChecker &TC;

  // For library-style parsing, we need to make two passes over the global
  // scope.  These booleans indicate whether this is currently the first or
  // second pass over the global scope (or neither, if we're in a context where
  // we only visit each decl once).
  bool IsFirstPass;
  bool IsSecondPass;

  DeclChecker(TypeChecker &TC, bool IsFirstPass, bool IsSecondPass)
      : TC(TC), IsFirstPass(IsFirstPass), IsSecondPass(IsSecondPass) {}

  //===--------------------------------------------------------------------===//
  // Helper Functions.
  //===--------------------------------------------------------------------===//

  void validateAttributes(ValueDecl *VD);

  /// \brief Check the list of inherited protocols on the declaration D.
  void checkInherited(Decl *D, MutableArrayRef<TypeLoc> Inherited) {
    // Check the list of inherited protocols.
    for (unsigned i = 0, e = Inherited.size(); i != e; ++i) {
      if (TC.validateType(Inherited[i], IsFirstPass)) {
        Inherited[i].setInvalidType(TC.Context);
        continue;
      }

      if (!Inherited[i].getType()->isExistentialType() &&
          !Inherited[i].getType()->is<ErrorType>()) {
        // FIXME: Terrible location information.
        TC.diagnose(D->getStartLoc(), diag::nonprotocol_inherit,
                    Inherited[i].getType());
      }
    }
  }

  void checkExplicitConformance(Decl *D, Type T,
                                MutableArrayRef<TypeLoc> Inherited) {
    for (auto InheritedTy : Inherited) {
      // FIXME: Poor location info.
      SmallVector<ProtocolDecl *, 4> InheritedProtos;
      if (InheritedTy.getType()->isExistentialType(InheritedProtos))
        for (auto Proto : InheritedProtos)
          TC.conformsToProtocol(T, Proto, nullptr, D->getStartLoc());
    }
  }

  void checkGenericParams(GenericParamList *GenericParams) {
    assert(GenericParams && "Missing generic parameters");

    // Assign archetypes to each of the generic parameters.
    ArchetypeBuilder Builder(TC);
    unsigned Index = 0;
    for (auto GP : *GenericParams) {
      auto TypeParam = GP.getAsTypeParam();

      // Check the constraints on the type parameter.
      checkInherited(TypeParam, TypeParam->getInherited());

      // Add the generic parameter to the builder.
      Builder.addGenericParameter(TypeParam, Index++);
    }

    // Add the requirements clause to the builder, validating only those
    // types that need to be complete at this point.
    // FIXME: Tell the type validator not to assert about unresolved types.
    for (auto &Req : GenericParams->getRequirements()) {
      if (Req.isInvalid())
        continue;
      
      switch (Req.getKind()) {
      case RequirementKind::Conformance: {
        if (TC.validateType(Req.getProtocolLoc(), IsFirstPass)) {
          Req.setInvalid();
          continue;
        }

        if (!Req.getProtocol()->isExistentialType()) {
          TC.diagnose(GenericParams->getRequiresLoc(),
                      diag::requires_conformance_nonprotocol,
                      Req.getSubject(), Req.getProtocol());
          Req.getProtocolLoc().setInvalidType(TC.Context);
          Req.setInvalid();
          continue;
        }
        break;
      }

      case RequirementKind::SameType:
        break;
      }

      if (Builder.addRequirement(Req))
        Req.setInvalid();
    }

    // Wire up the archetypes.
    Builder.assignArchetypes();
    for (auto GP : *GenericParams) {
      auto TypeParam = GP.getAsTypeParam();

      TypeParam->getUnderlyingTypeLoc()
        = TypeLoc::withoutLoc(Builder.getArchetype(TypeParam));
    }
    GenericParams->setAllArchetypes(
      TC.Context.AllocateCopy(Builder.getAllArchetypes()));

    // Validate the types in the requirements clause.
    for (auto &Req : GenericParams->getRequirements()) {
      if (Req.isInvalid())
        continue;

      switch (Req.getKind()) {
      case RequirementKind::Conformance: {
        if (TC.validateType(Req.getSubjectLoc(), IsFirstPass)) {
          Req.setInvalid();
          continue;
        }
        break;
      }

      case RequirementKind::SameType:
        if (TC.validateType(Req.getFirstTypeLoc(), IsFirstPass)) {
          Req.setInvalid();
          continue;
        }

        if (TC.validateType(Req.getSecondTypeLoc(), IsFirstPass)) {
          Req.setInvalid();
          continue;
        }
        break;
      }
    }
  }

  //===--------------------------------------------------------------------===//
  // Visit Methods.
  //===--------------------------------------------------------------------===//

  void visitImportDecl(ImportDecl *ID) {
    // Nothing to do.
  }

  void visitBoundVars(Pattern *P) {
    switch (P->getKind()) {
    // Recurse into patterns.
    case PatternKind::Tuple:
      for (auto &field : cast<TuplePattern>(P)->getFields())
        visitBoundVars(field.getPattern());
      return;
    case PatternKind::Paren:
      return visitBoundVars(cast<ParenPattern>(P)->getSubPattern());
    case PatternKind::Typed:
      return visitBoundVars(cast<TypedPattern>(P)->getSubPattern());

    // Handle vars.
    case PatternKind::Named: {
      VarDecl *VD = cast<NamedPattern>(P)->getDecl();

      if (!VD->getType()->isMaterializable()) {
        TC.diagnose(VD->getStartLoc(), diag::var_type_not_materializable,
                    VD->getType());
        VD->overwriteType(ErrorType::get(TC.Context));
      }

      validateAttributes(VD);
      return;
    }

    // Handle non-vars.
    case PatternKind::Any:
      return;
    }
    llvm_unreachable("bad pattern kind!");
  }

  void visitPatternBindingDecl(PatternBindingDecl *PBD) {
    bool DelayCheckingPattern = TC.TU.Kind != TranslationUnit::Library &&
                                PBD->getDeclContext()->isModuleContext();
    if (IsSecondPass && !DelayCheckingPattern) {
      if (PBD->getInit() && PBD->getPattern()->hasType()) {
        Expr *Init = PBD->getInit();
        Type DestTy = PBD->getPattern()->getType();
        if (TC.typeCheckExpression(Init, DestTy)) {
          if (DestTy)
            TC.diagnose(PBD->getStartLoc(), diag::while_converting_var_init,
                        DestTy);
        } else {
          PBD->setInit(Init);
        }
      }
      return;
    }
    if (PBD->getInit() && !IsFirstPass) {
      Type DestTy;
      if (isa<TypedPattern>(PBD->getPattern())) {
        if (TC.typeCheckPattern(PBD->getPattern(), /*isFirstPass*/false,
                                /*allowUnknownTypes*/false))
          return;
        DestTy = PBD->getPattern()->getType();
      }
      Expr *Init = PBD->getInit();
      if (TC.typeCheckExpression(Init, DestTy)) {
        if (DestTy)
          TC.diagnose(PBD->getStartLoc(), diag::while_converting_var_init,
                      DestTy);
        return;
      }
      if (!DestTy) {
        Expr *newInit = TC.convertToMaterializable(Init);
        if (newInit) Init = newInit;
      }
      PBD->setInit(Init);
      if (!DestTy) {
        if (TC.coerceToType(PBD->getPattern(), Init->getType(),
                            /*isFirstPass*/false))
          return;
      }
    } else if (!IsFirstPass || !DelayCheckingPattern) {
      if (TC.typeCheckPattern(PBD->getPattern(), IsFirstPass,
                              /*allowUnknownTypes*/false))
        return;
    }
    visitBoundVars(PBD->getPattern());
  }

  void visitSubscriptDecl(SubscriptDecl *SD) {
    if (IsSecondPass)
      return;

    // The getter and setter functions will be type-checked separately.
    if (!SD->getDeclContext()->isTypeContext())
      TC.diagnose(SD->getStartLoc(), diag::subscript_not_member);

    bool isInvalid = TC.validateType(SD->getElementTypeLoc(), IsFirstPass);
    isInvalid |= TC.typeCheckPattern(SD->getIndices(), IsFirstPass,
                                     /*allowUnknownTypes*/false);

    if (isInvalid) {
      SD->setType(ErrorType::get(TC.Context));
    } else {
      SD->setType(FunctionType::get(SD->getIndices()->getType(),
                                    SD->getElementType(), TC.Context));
    }
  }
  
  void visitTypeAliasDecl(TypeAliasDecl *TAD) {
    if (!IsSecondPass) {
      // FIXME: Need to fix the validateType API for typealias.
      TypeLoc FakeTypeLoc = TypeLoc::withoutLoc(TAD->getDeclaredType());
      TC.validateType(FakeTypeLoc, IsFirstPass);
      if (!isa<ProtocolDecl>(TAD->getDeclContext()))
        checkInherited(TAD, TAD->getInherited());
    }

    if (!IsFirstPass)
      checkExplicitConformance(TAD, TAD->getDeclaredType(),
                               TAD->getInherited());
  }

  void visitOneOfDecl(OneOfDecl *OOD) {
    if (!IsSecondPass) {
      checkInherited(OOD, OOD->getInherited());

      if (auto gp = OOD->getGenericParams()) {
        gp->setOuterParameters(
                            OOD->getDeclContext()->getGenericParamsOfContext());
        checkGenericParams(gp);
      }

      // Now that we have archetypes for our generic parameters (including
      // generic parameters from outer scopes), we can canonicalize our type.
      OOD->overwriteType(OOD->getType()->getCanonicalType());
      OOD->overwriteDeclaredType(OOD->getDeclaredType()->getCanonicalType());
      TC.validateTypeSimple(OOD->getDeclaredTypeInContext());

      validateAttributes(OOD);
    }
    
    for (Decl *member : OOD->getMembers())
      visit(member);
    
    if (!IsFirstPass)
      checkExplicitConformance(OOD, OOD->getDeclaredType(),
                               OOD->getInherited());
  }

  void visitStructDecl(StructDecl *SD) {
    if (!IsSecondPass) {
      checkInherited(SD, SD->getInherited());

      if (auto gp = SD->getGenericParams()) {
        gp->setOuterParameters(
                             SD->getDeclContext()->getGenericParamsOfContext());
        checkGenericParams(gp);
      }

      // Now that we have archetypes for our generic parameters (including
      // generic parameters from outer scopes), we can canonicalize our type.
      SD->overwriteType(SD->getType()->getCanonicalType());
      SD->overwriteDeclaredType(SD->getDeclaredType()->getCanonicalType());
      TC.validateTypeSimple(SD->getDeclaredTypeInContext());

      validateAttributes(SD);
    }

    ConstructorDecl *ValueCD = cast<ConstructorDecl>(SD->getMembers().back());

    for (Decl *Member : SD->getMembers()) {
      if (Member != ValueCD)
        visit(Member);
    }

    if (!IsSecondPass) {
      // FIXME: This is a bit of a hack.
      SmallVector<TuplePatternElt, 8> PatternElts;
      SmallVector<TupleTypeElt, 8> TupleElts;
      for (Decl *Member : SD->getMembers()) {
        if (VarDecl *VarD = dyn_cast<VarDecl>(Member)) {
          if (!VarD->isProperty()) {
            VarDecl *ArgD = new (TC.Context) VarDecl(SD->getLoc(),
                                                     VarD->getName(),
                                                     Type(), ValueCD);
            Pattern *P = new (TC.Context) NamedPattern(ArgD);
            TypeLoc Ty = TypeLoc::withoutLoc(VarD->getType());
            P = new (TC.Context) TypedPattern(P, Ty);
            PatternElts.push_back(TuplePatternElt(P));
            TupleElts.push_back(TupleTypeElt(VarD->getType(),
                                             VarD->getName()));
          }
        }
      }
      TuplePattern *TP = TuplePattern::create(TC.Context, SD->getLoc(),
                                              PatternElts, SD->getLoc());
      ValueCD->setArguments(TP);
    }

    visit(ValueCD);

    if (!IsFirstPass)
      checkExplicitConformance(SD, SD->getDeclaredType(),
                               SD->getInherited());
  }

  void visitClassDecl(ClassDecl *CD) {
    if (!IsSecondPass) {
      if (CD->hasBaseClass())
        TC.validateType(CD->getBaseClassLoc(), IsFirstPass);

      checkInherited(CD, CD->getInherited());

      if (auto gp = CD->getGenericParams()) {
        gp->setOuterParameters(
                             CD->getDeclContext()->getGenericParamsOfContext());
        checkGenericParams(gp);
      }

      // Now that we have archetypes for our generic parameters (including
      // generic parameters from outer scopes), we can canonicalize our type.
      CD->overwriteType(CD->getType()->getCanonicalType());
      CD->overwriteDeclaredType(CD->getDeclaredType()->getCanonicalType());
      TC.validateTypeSimple(CD->getDeclaredTypeInContext());

      validateAttributes(CD);
    }

    for (Decl *Member : CD->getMembers())
      visit(Member);
    
    if (!IsFirstPass)
      checkExplicitConformance(CD, CD->getDeclaredType(),
                               CD->getInherited());
  }

  void visitProtocolDecl(ProtocolDecl *PD) {
    if (IsSecondPass)
      return;

    // Fix the 'This' associated type.
    TypeAliasDecl *thisDecl = nullptr;
    for (auto Member : PD->getMembers()) {
      if (auto AssocType = dyn_cast<TypeAliasDecl>(Member)) {
        if (AssocType->getName().str() == "This") {
          thisDecl = AssocType;
          break;
        }
      }
    }

    // Build archetypes for this protocol.
    ArchetypeBuilder builder(TC);
    builder.addGenericParameter(thisDecl, 0);
    builder.addImplicitConformance(thisDecl, PD);
    builder.assignArchetypes();

    // Set the underlying type of each of the associated types to the
    // appropriate archetype.
    ArchetypeType *thisArchetype = builder.getArchetype(thisDecl);
    for (auto member : PD->getMembers()) {
      if (auto assocType = dyn_cast<TypeAliasDecl>(member)) {
        TypeLoc underlyingTy;
        if (assocType == thisDecl)
          underlyingTy = TypeLoc::withoutLoc(thisArchetype);
        else
          underlyingTy = TypeLoc::withoutLoc(thisArchetype->getNestedType(
                                               assocType->getName()));
        assocType->getUnderlyingTypeLoc() = underlyingTy;
      }
    }

    // Check the members.
    for (auto Member : PD->getMembers())
      visit(Member);

    validateAttributes(PD);
  }
  
  void visitVarDecl(VarDecl *VD) {
    // Delay type-checking on VarDecls until we see the corresponding
    // PatternBindingDecl.
  }

  void visitFuncDecl(FuncDecl *FD) {
    if (IsSecondPass)
      return;

    FuncExpr *body = FD->getBody();

    // Before anything else, set up the 'this' argument correctly.
    GenericParamList *outerGenericParams = nullptr;
    if (Type thisType = FD->computeThisType(&outerGenericParams)) {
      TC.validateTypeSimple(thisType);
      TypedPattern *thisPattern =
        cast<TypedPattern>(body->getArgParamPatterns()[0]);
      if (thisPattern->hasType()) {
        assert(thisPattern->getType().getPointer() == thisType.getPointer());
      } else {
        thisPattern->setType(thisType);
      }
    }

    if (auto gp = FD->getGenericParams()) {
      gp->setOuterParameters(outerGenericParams);
      checkGenericParams(gp);
    }

    TC.semaFuncExpr(body, IsFirstPass, /*allowUnknownTypes*/false);
    FD->setType(body->getType());

    validateAttributes(FD);
  }

  void visitOneOfElementDecl(OneOfElementDecl *ED) {
    if (IsSecondPass)
      return;

    OneOfDecl *OOD = cast<OneOfDecl>(ED->getDeclContext());
    Type ElemTy = OOD->getDeclaredTypeInContext();

    // If we have a simple element, just set the type.
    if (ED->getArgumentType().isNull()) {
      Type argTy = MetaTypeType::get(ElemTy, TC.Context);
      Type fnTy;
      if (auto gp = OOD->getGenericParamsOfContext())
        fnTy = PolymorphicFunctionType::get(argTy, ElemTy, gp, TC.Context);
      else
        fnTy = FunctionType::get(argTy, ElemTy, TC.Context);
      ED->setType(fnTy);
      return;
    }

    // We have an element with an argument type; validate the argument,
    // then compute a function type.
    if (TC.validateType(ED->getArgumentTypeLoc(), IsFirstPass))
      return;

    Type fnTy = FunctionType::get(ED->getArgumentType(), ElemTy, TC.Context);
    if (auto gp = OOD->getGenericParamsOfContext())
      fnTy = PolymorphicFunctionType::get(MetaTypeType::get(ElemTy, TC.Context),
                                          fnTy, gp, TC.Context);
    else
      fnTy = FunctionType::get(MetaTypeType::get(ElemTy, TC.Context), fnTy,
                               TC.Context);
    ED->setType(fnTy);

    // Require the carried type to be materializable.
    if (!ED->getArgumentType()->isMaterializable()) {
      TC.diagnose(ED->getLoc(),
                  diag::oneof_element_not_materializable);
    }
  }

  void visitExtensionDecl(ExtensionDecl *ED) {
    if (!IsSecondPass) {
      TC.validateType(ED->getExtendedTypeLoc(), IsFirstPass);

      Type ExtendedTy = ED->getExtendedType();
      if (!ExtendedTy->is<OneOfType>() && !ExtendedTy->is<StructType>() &&
          !ExtendedTy->is<ClassType>() && !ExtendedTy->is<ErrorType>() &&
          !ExtendedTy->is<UnboundGenericType>()) {
        TC.diagnose(ED->getStartLoc(), diag::non_nominal_extension,
                    ExtendedTy->is<ProtocolType>(), ExtendedTy);
        // FIXME: It would be nice to point out where we found the named type
        // declaration, if any.
      }
    
      checkInherited(ED, ED->getInherited());
    }

    for (Decl *Member : ED->getMembers())
      visit(Member);

    if (!IsFirstPass)
      checkExplicitConformance(ED, ED->getExtendedType(),
                               ED->getInherited());
  }

  void visitTopLevelCodeDecl(TopLevelCodeDecl *TLCD) {
    // See swift::performTypeChecking for TopLevelCodeDecl handling.
    llvm_unreachable("TopLevelCodeDecls are handled elsewhere");
  }

  void visitConstructorDecl(ConstructorDecl *CD) {
    if (IsSecondPass)
      return;

    if (!CD->getDeclContext()->isTypeContext())
      TC.diagnose(CD->getStartLoc(), diag::constructor_not_member);

    GenericParamList *outerGenericParams = nullptr;
    Type ThisTy = CD->computeThisType(&outerGenericParams);
    TC.validateTypeSimple(ThisTy);
    CD->getImplicitThisDecl()->setType(ThisTy);

    if (auto gp = CD->getGenericParams()) {
      gp->setOuterParameters(outerGenericParams);
      checkGenericParams(gp);
    }

    if (TC.typeCheckPattern(CD->getArguments(), IsFirstPass,
                            /*allowUnknownTypes*/false)) {
      CD->setType(ErrorType::get(TC.Context));
    } else {
      Type FnTy;
      if (GenericParamList *innerGenericParams = CD->getGenericParams()) {
        innerGenericParams->setOuterParameters(outerGenericParams);
        FnTy = PolymorphicFunctionType::get(CD->getArguments()->getType(),
                                            ThisTy, innerGenericParams,
                                            TC.Context);
      } else
        FnTy = FunctionType::get(CD->getArguments()->getType(),
                                 ThisTy, TC.Context);
      Type ThisMetaTy = MetaTypeType::get(ThisTy, TC.Context);
      if (outerGenericParams) {
        FnTy = PolymorphicFunctionType::get(ThisMetaTy, FnTy, 
                                            outerGenericParams, TC.Context);
      } else {
        FnTy = FunctionType::get(ThisMetaTy, FnTy, TC.Context);
      }
      CD->setType(FnTy);
    }

    validateAttributes(CD);
  }

  void visitDestructorDecl(DestructorDecl *DD) {
    if (IsSecondPass)
      return;

    if (!isa<ClassDecl>(DD->getDeclContext()))
      TC.diagnose(DD->getStartLoc(), diag::destructor_not_member);

    GenericParamList *outerGenericParams = nullptr;
    Type ThisTy = DD->computeThisType(&outerGenericParams);
    TC.validateTypeSimple(ThisTy);
    Type FnTy;
    if (outerGenericParams)
      FnTy = PolymorphicFunctionType::get(ThisTy,
                                          TupleType::getEmpty(TC.Context),
                                          outerGenericParams, TC.Context);
    else
      FnTy = FunctionType::get(ThisTy, TupleType::getEmpty(TC.Context),
                               TC.Context);
    
    DD->setType(FnTy);
    DD->getImplicitThisDecl()->setType(ThisTy);

    validateAttributes(DD);
  }
};
}; // end anonymous namespace.


void TypeChecker::typeCheckDecl(Decl *D, bool isFirstPass) {
  bool isSecondPass = !isFirstPass && D->getDeclContext()->isModuleContext();
  DeclChecker(*this, isFirstPass, isSecondPass).visit(D);
}

void TypeChecker::preCheckProtocol(ProtocolDecl *D) {
  DeclChecker checker(*this, /*isFirstPass=*/true, /*isSecondPass=*/false);
  checker.checkInherited(D, D->getInherited());
  for (auto member : D->getMembers()) {
    if (auto assocType = dyn_cast<TypeAliasDecl>(member))
      checker.checkInherited(assocType, assocType->getInherited());
  }
}

/// validateAttributes - Check that the func/var declaration attributes are ok.
void DeclChecker::validateAttributes(ValueDecl *VD) {
  const DeclAttributes &Attrs = VD->getAttrs();
  Type Ty = VD->getType();
  
  // Get the number of lexical arguments, for semantic checks below.
  int NumArguments = -1;
  if (FuncDecl *FD = dyn_cast<FuncDecl>(VD)) {
    if (AnyFunctionType *FT = Ty->getAs<AnyFunctionType>()) {
      if (FD->getDeclContext()->isTypeContext() && FD->isStatic())
        FT = FT->getResult()->castTo<AnyFunctionType>();
      if (TupleType *TT = FT->getInput()->getAs<TupleType>())
        NumArguments = TT->getFields().size();
    }
  }

  bool isOperator = VD->isOperator();

  // Operators must be declared with 'func', not 'var'.
  if (isOperator) {
    if (!isa<FuncDecl>(VD)) {
      TC.diagnose(VD->getStartLoc(), diag::operator_not_func);
      // FIXME: Set the 'isError' bit on the decl.
      return;
    }
  
    if (NumArguments == 0 || NumArguments > 2) {
      TC.diagnose(VD->getStartLoc(), diag::invalid_arg_count_for_operator);
      VD->getMutableAttrs().Infix = InfixData();
      // FIXME: Set the 'isError' bit on the decl.
      return;
    }

    // The unary operator '&' cannot be overloaded.  In an expression,
    // the parser never interprets this as a normal unary operator
    // anyway.
    if (NumArguments == 1 && VD->getName().str() == "&") {
      TC.diagnose(VD->getStartLoc(), diag::custom_operator_addressof);
      return;
    }
  }

  if (Attrs.isObjC()) {
    // Only classes and methods can be ObjC.
    bool isLegal = false;
    if (isa<ClassDecl>(VD)) {
      isLegal = true;
    } else if (isa<FuncDecl>(VD) && isa<ClassDecl>(VD->getDeclContext())) {
      isLegal = !isOperator;
    }
    if (!isLegal) {
      TC.diagnose(VD->getStartLoc(), diag::invalid_objc_decl);
      VD->getMutableAttrs().ObjC = false;
      return;
    }
  }
  
  if (Attrs.isInfix()) {
    // Only operator functions can be infix.
    if (!isOperator) {
      TC.diagnose(VD->getStartLoc(), diag::infix_not_an_operator);
      VD->getMutableAttrs().Infix = InfixData();
      // FIXME: Set the 'isError' bit on the decl.
      return;
    }

    // Only binary operators can be infix.
    if (NumArguments != 2) {
      TC.diagnose(Attrs.LSquareLoc, diag::invalid_infix_left_input);
      VD->getMutableAttrs().Infix = InfixData();
      // FIXME: Set the 'isError' bit on the decl.
      return;
    }
  }

  if (Attrs.isPostfix()) {
    // Only operator functions can be postfix.
    if (!isOperator) {
      TC.diagnose(VD->getStartLoc(), diag::postfix_not_an_operator);
      VD->getMutableAttrs().Postfix = false;
      // FIXME: Set the 'isError' bit on the decl.
      return;
    }

    // Only unary operators can be postfix.
    if (NumArguments != 1) {
      TC.diagnose(VD->getStartLoc(), diag::invalid_postfix_input);
      VD->getMutableAttrs().Postfix = false;
      // FIXME: Set the 'isError' bit on the decl.
      return;
    }
  }

  if (Attrs.isAssignment()) {
    // Only function declarations can be assignments.
    if (!isa<FuncDecl>(VD) || !VD->isOperator()) {
      TC.diagnose(VD->getStartLoc(), diag::invalid_decl_attribute,"assignment");
      VD->getMutableAttrs().Assignment = false;
    } else if (NumArguments < 1) {
      TC.diagnose(VD->getStartLoc(), diag::assignment_without_byref);
      VD->getMutableAttrs().Assignment = false;
    } else {
      auto FT = VD->getType()->castTo<AnyFunctionType>();
      Type ParamType = FT->getInput();
      TupleType *ParamTT = ParamType->getAs<TupleType>();
      if (ParamTT)
        ParamType = ParamTT->getElementType(0);
      
      if (!ParamType->is<LValueType>()) {
        TC.diagnose(VD->getStartLoc(), diag::assignment_without_byref);
        VD->getMutableAttrs().Assignment = false;
      } else if (!FT->getResult()->isEqual(TupleType::getEmpty(TC.Context))) {
        TC.diagnose(VD->getStartLoc(), diag::assignment_nonvoid,
                    FT->getResult());
      }
    }
  }

  if (Attrs.isConversion()) {
    // Only instance members with no non-defaulted parameters can be
    // conversions.
    if (!isa<FuncDecl>(VD) || !VD->isInstanceMember()) {
      TC.diagnose(VD->getStartLoc(), diag::conversion_not_instance_method,
                  VD->getName());
      VD->getMutableAttrs().Conversion = false;
    } else if (!VD->getType()->is<ErrorType>()) {
      AnyFunctionType *BoundMethodTy
        = VD->getType()->castTo<AnyFunctionType>()->getResult()
            ->castTo<AnyFunctionType>();
      
      bool AcceptsEmptyParamList = false;
      Type InputTy = BoundMethodTy->getInput();
      if (const TupleType *Tuple = InputTy->getAs<TupleType>()) {
        bool AllDefaulted = true;
        for (auto Elt : Tuple->getFields()) {
          if (!Elt.hasInit()) {
            AllDefaulted = false;
            break;
          }
        }
        
        AcceptsEmptyParamList = AllDefaulted;
      }
      
      if (!AcceptsEmptyParamList) {
        TC.diagnose(VD->getStartLoc(), diag::conversion_params,
                    VD->getName());
        VD->getMutableAttrs().Conversion = false;
      }
    }
  }
  
  if (VD->isOperator() && !VD->getAttrs().isInfix() && NumArguments != 1) {
    // If this declaration is defined in the translation unit, check whether
    // there are any other operators in this scope with the same name that are
    // infix. If so, inherit that infix.
    // FIXME: This is a hack in so many ways. We may eventually want to separate
    // the declaration of an operator name + precedence from a new operator
    // function, or at the very least check the consistency of operator
    // associativity and precedence within a given scope.
    if (TranslationUnit *TU = dyn_cast<TranslationUnit>(VD->getDeclContext())) {
      // Look in the translation unit.
      for (Decl *D : TU->Decls) {
        if (ValueDecl *Existing = dyn_cast<ValueDecl>(D)) {
          if (Existing->getName() == VD->getName() &&
              Existing->getAttrs().isInfix()) {
            VD->getMutableAttrs().Infix = Existing->getAttrs().Infix;
            break;
          }
        }
      }
      
      // Look in imported modules.
      if (!VD->getAttrs().isInfix()) {
        for (auto &ModPath : TU->getImportedModules()) {
          if (Module *Mod = ModPath.second) {
            SmallVector<ValueDecl *, 4> Found;
            Mod->lookupValue(Module::AccessPathTy(), VD->getName(),
                             NLKind::QualifiedLookup, Found);
            for (ValueDecl *Existing : Found) {
              if (Existing->getName() == VD->getName() &&
                  Existing->getAttrs().isInfix()) {
                VD->getMutableAttrs().Infix = Existing->getAttrs().Infix;
                break;              
              }

            if (VD->getAttrs().isInfix())
              break;
            }
          }
        }
      }
    }
    
    if (!VD->getAttrs().isInfix())
      TC.diagnose(VD->getStartLoc(), diag::binops_infix_left);
  }

  if (Attrs.isByref()) {
    TC.diagnose(VD->getStartLoc(), diag::invalid_decl_attribute, "byref");
    VD->getMutableAttrs().Byref = false;
  }

  if (Attrs.isAutoClosure()) {
    TC.diagnose(VD->getStartLoc(), diag::invalid_decl_attribute, "auto_closure");
    VD->getMutableAttrs().AutoClosure = false;
  }  
}

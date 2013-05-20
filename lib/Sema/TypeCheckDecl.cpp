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

/// \brief Describes the kind of implicit constructor that will be
/// generated.
enum class ImplicitConstructorKind {
  /// \brief The default constructor, which default-initializes each
  /// of the instance variables.
  Default,
  /// \brief The memberwise constructor, which initializes each of
  /// the instance variables from a parameter of the same type and
  /// name.
  Memberwise
};

/// \brief Determine whether the given pattern contains only a single variable
/// that is a property.
static bool isPatternProperty(Pattern *pattern) {
  pattern = pattern->getSemanticsProvidingPattern();
  if (auto named = dyn_cast<NamedPattern>(pattern))
    return named->getDecl()->isProperty();

  return false;
}

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

      // FIXME: TypeAliasDecl check here is bogus.
      if (!Inherited[i].getType()->isExistentialType() &&
          !Inherited[i].getType()->is<ErrorType>() &&
          !(Inherited[i].getType()->getClassOrBoundGenericClass() &&
            isa<TypeAliasDecl>(D))) {
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
        if (TC.validateType(Req.getConstraintLoc(), IsFirstPass)) {
          Req.setInvalid();
          continue;
        }

        if (!Req.getConstraint()->isExistentialType() &&
            !Req.getConstraint()->getClassOrBoundGenericClass()) {
          TC.diagnose(GenericParams->getRequiresLoc(),
                      diag::requires_conformance_nonprotocol,
                      Req.getSubject(), Req.getConstraint());
          Req.getConstraintLoc().setInvalidType(TC.Context);
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

  ArrayRef<Substitution> buildForwardingSubstitutions(GenericParamList *gp) {
    ArrayRef<ArchetypeType*> params = gp->getAllArchetypes();
    
    size_t paramCount = params.size();
    Substitution *resultBuf = TC.Context.Allocate<Substitution>(paramCount);
    MutableArrayRef<Substitution> results{resultBuf, paramCount};
    
    for (size_t i = 0; i < paramCount; ++i) {
      // FIXME: better way to do this?
      ArchetypeType *archetype = params[i];
      // "Check conformance" on each declared protocol to build a
      // conformance map.
      SmallVector<ProtocolConformance*, 2> conformances;
      
      for (ProtocolDecl *conformsTo : archetype->getConformsTo()) {
        ProtocolConformance *conformance;
        bool x = TC.conformsToProtocol(archetype, conformsTo,
                                       &conformance);
        (void)x;
        assert(x && "archetype did not conform to protocol?!");
        
        conformances.push_back(conformance);
      }
      
      // Build an identity mapping with the derived conformances.
      auto replacement = SubstitutedType::get(archetype, archetype, TC.Context);
      results[i] = {archetype, replacement,
                    TC.Context.AllocateCopy(conformances)};
    }
    
    return results;
  }
  
  //===--------------------------------------------------------------------===//
  // Visit Methods.
  //===--------------------------------------------------------------------===//

  void visitImportDecl(ImportDecl *ID) {
    // Nothing to do.
  }

  void visitBoundVars(Pattern *P) {
    switch (P->getKind()) {
    // Recur into patterns.
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
      
      // The var requires ObjC interop if it has an [objc] or [iboutlet]
      // attribute or if it's a member of an ObjC class.
      DeclContext *dc = VD->getDeclContext();
      if (dc && dc->getDeclaredTypeInContext()) {
        ClassDecl *classContext = dc->getDeclaredTypeInContext()
          ->getClassOrBoundGenericClass();
        VD->setIsObjC(VD->getAttrs().isObjC()
                      || (classContext && classContext->isObjC()));
      }
      
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
            TC.diagnose(PBD, diag::while_converting_var_init,
                        DestTy);
        } else {
          PBD->setInit(Init);
        }
      }
      return;
    }

    // If there is no initializer and we are not in a type context,
    // create a default initializer.
    if (!PBD->getInit() && !IsFirstPass &&
        isa<TypedPattern>(PBD->getPattern()) &&
        !PBD->getDeclContext()->isTypeContext()) {
      // Type-check the pattern.
      if (TC.typeCheckPattern(PBD->getPattern(), /*isFirstPass*/false,
                              /*allowUnknownTypes*/false))
        return;

      Type ty = PBD->getPattern()->getType();
      Expr *initializer = nullptr;
      if (isPatternProperty(PBD->getPattern())) {
        // Properties don't have initializers.
      } else if (!TC.isDefaultInitializable(ty, &initializer)) {
        // FIXME: Better diagnostics here.
        TC.diagnose(PBD, diag::decl_no_default_init, ty);
        PBD->setInvalid();
      } else {
        if (TC.typeCheckExpression(initializer, ty)) {
          TC.diagnose(PBD, diag::while_converting_var_init, ty);
          return;
        }

        PBD->setInit(initializer);
      }
    } else if (PBD->getInit() && !IsFirstPass) {
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
          TC.diagnose(PBD, diag::while_converting_var_init,
                      DestTy);
        return;
      }
      if (!DestTy) {
        Expr *newInit = TC.coerceToMaterializable(Init);
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

    assert(SD->getDeclContext()->isTypeContext()
           && "Decl parsing must prevent subscripts outside of types!");

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

    // Visit each of the members.
    for (Decl *Member : SD->getMembers()) {
      visit(Member);
    }

    if (!IsSecondPass) {
      TC.addImplicitConstructors(SD);
    }

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
      
      ClassDecl *baseClassDecl = CD->getBaseClass()
        ? CD->getBaseClass()->getClassOrBoundGenericClass()
        : nullptr;
      
      CD->setIsObjC(CD->getAttrs().isObjC()
                    || (baseClassDecl && baseClassDecl->isObjC()));
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
    
    // A method is ObjC-compatible if it's explicitly [objc], a member of an
    // ObjC-compatible class, or an accessor for an ObjC property.
    DeclContext *dc = FD->getDeclContext();
    if (dc && dc->getDeclaredTypeInContext()) {
      ClassDecl *classContext = dc->getDeclaredTypeInContext()
        ->getClassOrBoundGenericClass();
      
      bool isObjC = FD->getAttrs().isObjC()
        || (classContext && classContext->isObjC());
      if (!isObjC && FD->isGetterOrSetter()) {
        // If the property decl is an instance property, its accessors will
        // be instance methods and the above condition will mark them ObjC.
        // The only additional condition we need to check is if the var decl
        // had an [objc] or [iboutlet] property. We don't use prop->isObjC()
        // because the property accessors may be visited before the VarDecl and
        // prop->isObjC() may not yet be set by typechecking.
        ValueDecl *prop = cast<ValueDecl>(FD->getGetterOrSetterDecl());
        isObjC = prop->getAttrs().isObjC() || prop->getAttrs().isIBOutlet();
      }
      
      FD->setIsObjC(isObjC);
    }
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
      TC.diagnose(ED->getLoc(), diag::oneof_element_not_materializable);
    }
  }

  void visitExtensionDecl(ExtensionDecl *ED) {
    if (!IsSecondPass) {
      TC.validateType(ED->getExtendedTypeLoc(), IsFirstPass);

      Type ExtendedTy = ED->getExtendedType()->getCanonicalType();
      if (!ExtendedTy->is<OneOfType>() && !ExtendedTy->is<StructType>() &&
          !ExtendedTy->is<ClassType>() && !ExtendedTy->is<ErrorType>() &&
          !ExtendedTy->is<UnboundGenericType>()) {
        TC.diagnose(ED->getStartLoc(), diag::non_nominal_extension,
                    ExtendedTy->is<ProtocolType>(), ExtendedTy);
        // FIXME: It would be nice to point out where we found the named type
        // declaration, if any.
      }

      // Add this extension to the list of extensions for the extended type.
      if (NominalTypeDecl *nominal
            = ExtendedTy->getNominalOrBoundGenericNominal()) {
        nominal->addExtension(ED);
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

    assert(CD->getDeclContext()->isTypeContext()
           && "Decl parsing must prevent constructors outside of types!");

    GenericParamList *outerGenericParams = nullptr;
    Type ThisTy = CD->computeThisType(&outerGenericParams);
    TC.validateTypeSimple(ThisTy);
    CD->getImplicitThisDecl()->setType(ThisTy);

    if (auto gp = CD->getGenericParams()) {
      gp->setOuterParameters(outerGenericParams);
      checkGenericParams(gp);
      CD->setForwardingSubstitutions(buildForwardingSubstitutions(gp));
    }

    if (TC.typeCheckPattern(CD->getArguments(), IsFirstPass,
                            /*allowUnknownTypes*/false)) {
      CD->setType(ErrorType::get(TC.Context));
    } else {
      Type FnTy;
      Type AllocFnTy;
      Type InitFnTy;
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
        AllocFnTy = PolymorphicFunctionType::get(ThisMetaTy, FnTy,
                                                outerGenericParams, TC.Context);
        InitFnTy = PolymorphicFunctionType::get(ThisTy, FnTy,
                                                outerGenericParams, TC.Context);
      } else {
        AllocFnTy = FunctionType::get(ThisMetaTy, FnTy, TC.Context);
        InitFnTy = FunctionType::get(ThisTy, FnTy, TC.Context);
      }
      CD->setType(AllocFnTy);
      CD->setInitializerType(InitFnTy);
    }

    validateAttributes(CD);
  }

  void visitDestructorDecl(DestructorDecl *DD) {
    if (IsSecondPass)
      return;

    assert(DD->getDeclContext()->isTypeContext()
           && "Decl parsing must prevent destructors outside of types!");

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

/// \brief Create an implicit struct constructor.
///
/// \param structDecl The struct for which a constructor will be created.
/// \param ICK The kind of implicit constructor to create.
///
/// \returns The newly-created constructor, which has already been type-checked
/// (but has not been added to the containing struct).
static ConstructorDecl *createImplicitConstructor(TypeChecker &tc,
                                                  StructDecl *structDecl,
                                                  ImplicitConstructorKind ICK) {
  ASTContext &context = tc.Context;
  // Determine the parameter type of the implicit constructor.
  SmallVector<TuplePatternElt, 8> patternElts;
  SmallVector<VarDecl *, 8> allArgs;
  if (ICK == ImplicitConstructorKind::Memberwise) {
    for (auto member : structDecl->getMembers()) {
      auto var = dyn_cast<VarDecl>(member);
      if (!var)
        continue;

      // Properties are computed, not initialized.
      if (var->isProperty())
        continue;

      // Create the parameter.
      auto *arg = new (context) VarDecl(structDecl->getLoc(),
                                        var->getName(),
                                        var->getType(), structDecl);
      allArgs.push_back(arg);
      Pattern *pattern = new (context) NamedPattern(arg);
      TypeLoc tyLoc = TypeLoc::withoutLoc(var->getType());
      pattern = new (context) TypedPattern(pattern, tyLoc);
      patternElts.push_back(TuplePatternElt(pattern));
    }
  }

  // Crate the onstructor.
  auto constructorID = context.getIdentifier("constructor");
  VarDecl *thisDecl
    = new (context) VarDecl(SourceLoc(),
                            context.getIdentifier("this"),
                            Type(), structDecl);
  ConstructorDecl *ctor
    = new (context) ConstructorDecl(constructorID, structDecl->getLoc(),
                                    nullptr, thisDecl, nullptr, structDecl);
  thisDecl->setDeclContext(ctor);
  for (auto var : allArgs) {
    var->setDeclContext(ctor);
  }
  
  // Set its arguments.
  auto pattern = TuplePattern::create(context, structDecl->getLoc(),
                                      patternElts, structDecl->getLoc());
  ctor->setArguments(pattern);

  // Mark implicit.
  ctor->setImplicit();

  // Type-check the constructor declaration.
  tc.typeCheckDecl(ctor, /*isFirstPass=*/true);

  // If the struct in which this constructor is being added was imported,
  // add it as an external definition.
  auto dc = structDecl->getDeclContext();
  while (dc->getParent())
    dc = dc->getParent();
  if (isa<ClangModule>(dc)) {
    tc.Context.ExternalDefinitions.insert(ctor);
  }

  return ctor;
}

void TypeChecker::addImplicitConstructors(StructDecl *structDecl) {
  // Check whether there is a user-declared constructor or,
  // failing that, an instance variable.
  bool FoundConstructor = false;
  bool FoundInstanceVar = false;
  for (auto member : structDecl->getMembers()) {
    if (isa<ConstructorDecl>(member)) {
      FoundConstructor = true;
      break;
    }

    if (auto var = dyn_cast<VarDecl>(member)) {
      if (!var->isProperty())
        FoundInstanceVar = true;
    }
  }

  // If we didn't find such a constructor, add the implicit one(s).
  if (!FoundConstructor) {
    // Copy the list of members, so we can add to it.
    // FIXME: Painfully inefficient to do the copy here.
    SmallVector<Decl *, 4> members(structDecl->getMembers().begin(),
                                   structDecl->getMembers().end());

    // Create the implicit memberwise constructor.
    auto ctor = createImplicitConstructor(*this, structDecl,
                                          ImplicitConstructorKind::Memberwise);
    members.push_back(ctor);

    // Set the members of the struct.
    structDecl->setMembers(Context.AllocateCopy(members),
                           structDecl->getBraces());

    // If we found any instance variables, the default constructor will be
    // different than the memberwise constructor. Whether this
    // constructor will actually be defined depends on whether all of
    // the instance variables can be default-initialized, which we
    // don't know yet. This will be determined lazily.
    if (FoundInstanceVar) {
      assert(!structsNeedingImplicitDefaultConstructor.count(structDecl));
      structsNeedingImplicitDefaultConstructor.insert(structDecl);
      structsWithImplicitDefaultConstructor.push_back(structDecl);
    }
  }
}

bool TypeChecker::isDefaultInitializable(Type ty, Expr **initializer) {
  CanType canTy = ty->getCanonicalType();
  switch (canTy->getKind()) {
  case TypeKind::Archetype:
  case TypeKind::BoundGenericStruct:
  case TypeKind::BoundGenericOneOf:
  case TypeKind::OneOf:
  case TypeKind::Struct:
    // Break to look for constructors.
    break;

  case TypeKind::Array:
    // Arrays are default-initializable if their element types are.
    // FIXME: We don't implement this rule yet, so just fail.
    return false;

  case TypeKind::BoundGenericClass:
  case TypeKind::Class:
    // Classes are default-initializable (with 0).
    // FIXME: This may not be what we want in the long term.
    if (initializer) {
      *initializer = new (Context) ZeroValueExpr(ty);
    }
    return true;

  case TypeKind::Protocol:
  case TypeKind::ProtocolComposition:
    // Existentials are not default-initializable.
    return false;

  case TypeKind::BuiltinFloat:
  case TypeKind::BuiltinInteger:
  case TypeKind::BuiltinObjCPointer:
  case TypeKind::BuiltinObjectPointer:
  case TypeKind::BuiltinOpaquePointer:
  case TypeKind::BuiltinRawPointer:
    // Built-in types are default-initializable.
    if (initializer) {
      *initializer = new (Context) ZeroValueExpr(ty);
    }
    return true;

  case TypeKind::Tuple: {
    // Check whether all fields either have an initializer or have
    // default-initializable types.
    llvm::SmallVector<Expr *, 4> eltInits;
    llvm::SmallVector<Identifier, 4> eltNames;
    for (auto &elt : ty->castTo<TupleType>()->getFields()) {
      // If the element has an initializer, we're all set.
      if (elt.getInit()) {
        // FIXME: Add a DefaultTupleArgumentExpr node here?
        continue;
      }

      // Check whether the element is default-initializable.
      Expr *eltInit = nullptr;
      if (!isDefaultInitializable(elt.getType(),
                                  initializer? &eltInit : nullptr))
        return false;

      // If we need to produce an initializer, add this element.
      if (initializer) {
        assert(eltInit && "Missing initializer?");
        eltInits.push_back(eltInit);
        eltNames.push_back(elt.getName());
      }
    }

    // If we need to build an initializer, build a TupleExpr or use the
    // sole initializer (if all others are unnamed).
    if (initializer) {
      if (eltInits.size() == 1 && eltNames[0].empty())
        *initializer = eltInits[0];
      else
        *initializer
          = new (Context) TupleExpr(SourceLoc(),
                                    Context.AllocateCopy(eltInits),
                                    Context.AllocateCopy(eltNames).data(),
                                    SourceLoc(),
                                    /*hasTrailingClosure=*/false);
    }
    return true;
  }
  
  case TypeKind::Function:
  case TypeKind::LValue:
  case TypeKind::PolymorphicFunction:
  case TypeKind::MetaType:
  case TypeKind::Module:
      return false;

  // Sugar types.
#define TYPE(Id, Parent)
#define SUGARED_TYPE(Id, Parent) case TypeKind::Id:
#include "swift/AST/TypeNodes.def"
    llvm_unreachable("Not using the canonical type?");

#define TYPE(Id, Parent)
#define UNCHECKED_TYPE(Id, Parent) case TypeKind::Id:
#include "swift/AST/TypeNodes.def"
  case TypeKind::DeducibleGenericParam:
  case TypeKind::UnboundGeneric:
    // Error cases.
    return false;
  }

  // We need to look for a default constructor.
  llvm::SmallVector<ValueDecl *, 4> ctors;
  if (!lookupConstructors(ty, ctors))
    return false;

  // Check whether we have a constructor that can be called with an empty
  // tuple.
  bool foundDefaultConstructor = false;
  for (auto member : ctors) {
    // Dig out the parameter tuple for this constructor.
    auto ctor = dyn_cast<ConstructorDecl>(member);
    if (!ctor)
      continue;

    auto paramTuple = ctor->getArgumentType()->getAs<TupleType>();
    if (!paramTuple)
      continue;

    // Check whether any of the tuple elements are missing an initializer.
    bool missingInit = false;
    for (auto &elt : paramTuple->getFields()) {
      if (elt.getInit())
        continue;

      missingInit = true;
      break;
    }
    if (missingInit)
      continue;

    // We found a constructor that can be invoked with an empty tuple.
    if (foundDefaultConstructor) {
      // We found two constructors that can be invoked with an empty tuple.
      return false;
    }

    foundDefaultConstructor = true;
  }

  if (!foundDefaultConstructor || !initializer)
    return foundDefaultConstructor;

  // We found a default constructor. Construct the initializer expression.
  // FIXME: As an optimization, we could build a fully type-checked AST here.
  Expr *arg = new (Context) TupleExpr(SourceLoc(), { }, nullptr, SourceLoc(),
                                      /*hasTrailingClosure=*/false);
  Expr *metatype = new (Context) MetatypeExpr(nullptr, SourceLoc(),
                                              MetaTypeType::get(ty, Context));
  *initializer = new (Context) CallExpr(metatype, arg);

  return true;
}

void TypeChecker::defineDefaultConstructor(StructDecl *structDecl) {
  // Erase this from the set of structs that need an implicit default
  // constructor.
  assert(structsNeedingImplicitDefaultConstructor.count(structDecl));
  structsNeedingImplicitDefaultConstructor.erase(structDecl);

  // Verify that all of the instance variables of this struct have default
  // constructors.
  for (auto member : structDecl->getMembers()) {
    // We only care about pattern bindings.
    auto patternBind = dyn_cast<PatternBindingDecl>(member);
    if (!patternBind)
      continue;

    // If the pattern has an initializer, we don't need any default
    // initialization for its variables.
    if (patternBind->getInit())
      continue;

    // Find the variables in the pattern. They'll each need to be
    // default-initialized.
    SmallVector<VarDecl *, 4> variables;
    patternBind->getPattern()->collectVariables(variables);

    for (auto var : variables) {
      if (var->isProperty())
        continue;

      // If this variable is not default-initializable, we're done: we can't
      // add the default constructor because it will be ill-formed.
      if (!isDefaultInitializable(var->getType(), nullptr))
        return;
    }
  }

  // Create the default constructor.
  auto ctor = createImplicitConstructor(
                *this, structDecl, ImplicitConstructorKind::Default);

  // Copy the list of members, so we can add to it.
  // FIXME: Painfully inefficient to do the copy here.
  SmallVector<Decl *, 4> members(structDecl->getMembers().begin(),
                                 structDecl->getMembers().end());

  // Create the implicit memberwise constructor.
  members.push_back(ctor);

  // Set the members of the struct.
  structDecl->setMembers(Context.AllocateCopy(members),
                         structDecl->getBraces());

  // Create an empty body for the default constructor. The type-check of the
  // constructor body will introduce default initializations of the members.
  ctor->setBody(BraceStmt::create(Context, SourceLoc(), { }, SourceLoc()));

  // Add this to the list of implicitly-defined functions.
  implicitlyDefinedFunctions.push_back(ctor);
}

void TypeChecker::definePendingImplicitDecls() {
  // Default any implicit default constructors.
  for (auto structDecl : structsWithImplicitDefaultConstructor) {
    if (structsNeedingImplicitDefaultConstructor.count(structDecl))
      defineDefaultConstructor(structDecl);
  }
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
  FuncDecl *FDOrNull = dyn_cast<FuncDecl>(VD);
  if (FDOrNull) {
    if (AnyFunctionType *FT = Ty->getAs<AnyFunctionType>()) {
      if (FDOrNull->getDeclContext()->isTypeContext() && FDOrNull->isStatic())
        FT = FT->getResult()->castTo<AnyFunctionType>();
      if (TupleType *TT = FT->getInput()->getAs<TupleType>())
        NumArguments = TT->getFields().size();
    }
  }

  bool isOperator = VD->isOperator();

  // Operators must be declared with 'func', not 'var'.
  if (isOperator) {
    if (!FDOrNull) {
      TC.diagnose(VD->getLoc(), diag::operator_not_func);
      // FIXME: Set the 'isError' bit on the decl.
      return;
    }
  
    // The unary prefix operator '&' is reserved and cannot be overloaded.
    if (FDOrNull->isUnaryOperator() && VD->getName().str() == "&"
        && !Attrs.isPostfix()) {
      TC.diagnose(VD->getStartLoc(), diag::custom_operator_addressof);
      return;
    }
  }

  auto isInClassContext = [](ValueDecl *vd) {
    return bool(vd->getDeclContext()->getDeclaredTypeOfContext()
                  ->getClassOrBoundGenericClass());
  };
  
  if (Attrs.isObjC()) {
    // Only classes, instance properties, and methods can be ObjC.
    bool isLegal = false;
    if (isa<ClassDecl>(VD)) {
      isLegal = true;
    } else if (isa<FuncDecl>(VD) && isInClassContext(VD)) {
      isLegal = !isOperator;
    } else if (isa<VarDecl>(VD) && isInClassContext(VD)) {
      isLegal = true;
    }
    if (!isLegal) {
      TC.diagnose(VD->getStartLoc(), diag::invalid_objc_decl);
      VD->getMutableAttrs().ObjC = false;
      return;
    }
  }

  if (Attrs.isIBOutlet()) {
    // Only instance properties can be IBOutlets.
    // FIXME: This could do some type validation as well (all IBOutlets refer
    // to objects).
    if (!(isa<VarDecl>(VD) && isInClassContext(VD))) {
      TC.diagnose(VD->getStartLoc(), diag::invalid_iboutlet);
      VD->getMutableAttrs().IBOutlet = false;
      return;
    }
  }

  if (Attrs.isIBAction()) {
    // Only instance methods returning () can be IBActions.
    const FuncDecl *FD = dyn_cast<FuncDecl>(VD);
    if (!FD || !isa<ClassDecl>(VD->getDeclContext()) || FD->isStatic() ||
        FD->isGetterOrSetter()) {
      TC.diagnose(VD->getStartLoc(), diag::invalid_ibaction_decl);
      VD->getMutableAttrs().IBAction = false;
      return;
    }

    // IBActions instance methods must have type Class -> (...) -> ().
    // FIXME: This could do some argument type validation as well (only certain
    // method signatures are allowed for IBActions).
    Type CurriedTy = VD->getType()->castTo<AnyFunctionType>()->getResult();
    Type ResultTy = CurriedTy->castTo<AnyFunctionType>()->getResult();
    if (!ResultTy->isEqual(TupleType::getEmpty(TC.Context))) {
      TC.diagnose(VD->getStartLoc(), diag::invalid_ibaction_result, ResultTy);
      VD->getMutableAttrs().IBAction = false;
      return;
    }
  }

  if (Attrs.isInfix()) {
    // Only operator functions can be infix.
    if (!isOperator) {
      TC.diagnose(VD->getStartLoc(), diag::infix_not_an_operator);
      // FIXME: Set the 'isError' bit on the decl.
      return;
    }

    // Only binary operators can be infix.
    if (!FDOrNull || !FDOrNull->isBinaryOperator()) {
      TC.diagnose(Attrs.LSquareLoc, diag::invalid_infix_input);
      // FIXME: Set the 'isError' bit on the decl.
      return;
    }
  }

  if (Attrs.isPostfix()) {
    // Only operator functions can be postfix.
    if (!isOperator) {
      TC.diagnose(VD->getStartLoc(), diag::postfix_not_an_operator);
      VD->getMutableAttrs().ExplicitPostfix = false;
      // FIXME: Set the 'isError' bit on the decl.
      return;
    }

    // Only unary operators can be postfix.
    if (!FDOrNull || !FDOrNull->isUnaryOperator()) {
      TC.diagnose(VD->getStartLoc(), diag::invalid_postfix_input);
      VD->getMutableAttrs().ExplicitPostfix = false;
      // FIXME: Set the 'isError' bit on the decl.
      return;
    }
  }
  
  if (Attrs.isPrefix()) {
    // Only operator functions can be postfix.
    if (!isOperator) {
      TC.diagnose(VD->getStartLoc(), diag::prefix_not_an_operator);
      VD->getMutableAttrs().ExplicitPostfix = false;
      // FIXME: Set the 'isError' bit on the decl.
      return;
    }
    
    // Only unary operators can be postfix.
    if (!FDOrNull || !FDOrNull->isUnaryOperator()) {
      TC.diagnose(VD->getStartLoc(), diag::invalid_prefix_input);
      VD->getMutableAttrs().ExplicitPostfix = false;
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
  
  if (Attrs.isForceInline()) {
    // Only functions can be force_inline.
    auto *FD = dyn_cast<FuncDecl>(VD);
    if (!FD) {
      TC.diagnose(VD->getStartLoc(), diag::force_inline_not_function);
      VD->getMutableAttrs().ForceInline = false;
    } else if (FD->getBody()->getNumParamPatterns() > 1) {
      // We don't yet support force_inline of curried functions.
      TC.diagnose(VD->getStartLoc(), diag::force_inline_curry_not_supported);
      VD->getMutableAttrs().ForceInline = false;
    } else if (FD->getGenericParams()) {
      // We don't yet support force_inline of generic functions.
      TC.diagnose(VD->getStartLoc(), diag::force_inline_generic_not_supported);
      VD->getMutableAttrs().ForceInline = false;      
    }
  }
  
  if (Attrs.isByref()) {
    TC.diagnose(VD->getStartLoc(), diag::invalid_decl_attribute, "byref");
    VD->getMutableAttrs().Byref = false;
  }

  if (Attrs.isAutoClosure()) {
    TC.diagnose(VD->getStartLoc(), diag::invalid_decl_attribute, "auto_closure");
    VD->getMutableAttrs().AutoClosure = false;
  }

  if (Attrs.isObjCBlock()) {
    TC.diagnose(VD->getStartLoc(), diag::invalid_decl_attribute, "objc_block");
    VD->getMutableAttrs().ObjCBlock = false;
  }
}

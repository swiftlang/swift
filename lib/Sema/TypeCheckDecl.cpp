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

  bool visitValueDecl(ValueDecl *VD);
  void validateAttributes(ValueDecl *VD);

  /// \brief Check the list of inherited protocols on the declaration D.
  void checkInherited(Decl *D, MutableArrayRef<Type> Inherited) {
    // Check the list of inherited protocols.
    for (unsigned i = 0, e = Inherited.size(); i != e; ++i) {
      if (TC.validateType(Inherited[i], IsFirstPass)) {
        Inherited[i] = ErrorType::get(TC.Context);
        continue;
      }

      if (!Inherited[i]->isExistentialType() && !Inherited[i]->is<ErrorType>()) {
        // FIXME: Terrible location information.
        TC.diagnose(D->getStartLoc(), diag::nonprotocol_inherit, Inherited[i]);
      }
    }
  }

  void checkExplicitConformance(Decl *D, Type T,
                                MutableArrayRef<Type> Inherited) {
    for (auto InheritedTy : Inherited) {
      // FIXME: Poor location info.
      SmallVector<ProtocolDecl *, 4> InheritedProtos;
      if (InheritedTy->isExistentialType(InheritedProtos))
        for (auto Proto : InheritedProtos)
          TC.conformsToProtocol(T, Proto, nullptr, D->getStartLoc());
    }
  }

  void checkGenericParams(GenericParamList *GenericParams) {
    if (!GenericParams)
      return;

    // Assign archetypes to each of the generic parameters.
    // FIXME: Actually solve the various same-type constraints, written and
    // implied, to compute the set of archetypes we need and the requirements
    // on those archetypes.
    unsigned Index = 0;
    for (auto GP : *GenericParams) {
      auto TypeParam = GP.getAsTypeParam();

      // Check the constraints on the type parameter.
      checkInherited(TypeParam, TypeParam->getInherited());

      // Create the archetype for this type parameter.
      ArchetypeType *Archetype
        = ArchetypeType::getNew(TC.Context, TypeParam->getName().str(),
                                TypeParam->getInherited(), Index++);
      TypeParam->setUnderlyingType(Archetype);

      // Create archetypes for each of the associated types in each protocol.
      // FIXME: This should also be subject to same-type constraints, which
      // come from either a protocol or are implied by the presence of
      // same-named associated types in different protocols that our type
      // parameter conforms to.
      for (auto Inherited : TypeParam->getInherited()) {
        SmallVector<ProtocolDecl *, 4> Protocols;
        if (Inherited->isExistentialType(Protocols)) {
          for (auto P : Protocols) {
            for (auto Member : P->getMembers()) {
              TypeAliasDecl *AssocType = dyn_cast<TypeAliasDecl>(Member);
              if (!AssocType)
                continue;

              ArchetypeType *AssocArchetype
                = AssocType->getDeclaredType()->getAs<ArchetypeType>();
              if (!AssocArchetype)
                continue;
              
              // FIXME: Identify 'This' in some sane manner.
              Type MappedTo;
              if (AssocType->getName().str() == "This") {
                MappedTo = Archetype;
              } else {
                MappedTo
                  = ArchetypeType::getNew(TC.Context,
                                          AssocType->getName().str(),
                                          AssocType->getInherited());
              }
              
              TC.Context.AssociatedTypeMap[Archetype][AssocArchetype]
                = MappedTo;
            }
          }
        }
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
    case PatternKind::Named:
      visitValueDecl(cast<NamedPattern>(P)->getDecl());
      return;

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
      if (PBD->getPattern()->hasType()) {
        DestTy = PBD->getPattern()->getType();
        if (TC.validateType(DestTy, IsFirstPass)) {
          DestTy = ErrorType::get(TC.Context);
          PBD->getPattern()->overwriteType(DestTy);
        }
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
      } else {
        if (TC.typeCheckPattern(PBD->getPattern(), /*isFirstPass*/false))
          return;
      }
    } else if (!IsFirstPass || !DelayCheckingPattern) {
      if (TC.typeCheckPattern(PBD->getPattern(), IsFirstPass))
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

    if (TC.validateType(SD->getElementType(), IsFirstPass))
      SD->overwriteElementType(ErrorType::get(TC.Context));

    if (!TC.typeCheckPattern(SD->getIndices(), IsFirstPass))  {
      SD->setType(FunctionType::get(SD->getIndices()->getType(),
                                    SD->getElementType(), TC.Context));
    }
  }
  
  void visitTypeAliasDecl(TypeAliasDecl *TAD) {
    if (!IsSecondPass) {
      TC.validateType(TAD->getAliasType(), IsFirstPass);
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
      checkGenericParams(OOD->getGenericParams());
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
      checkGenericParams(SD->getGenericParams());
    }

    for (Decl *Member : SD->getMembers()) {
      visit(Member);
    }

    if (!IsSecondPass) {
      // FIXME: We should come up with a better way to represent this implied
      // constructor.
      SmallVector<TupleTypeElt, 8> TupleElts;
      for (Decl *Member : SD->getMembers())
        if (VarDecl *VarD = dyn_cast<VarDecl>(Member))
          if (!VarD->isProperty())
            TupleElts.push_back(TupleTypeElt(VarD->getType(),
                                             VarD->getName()));
      TupleType *TT = TupleType::get(TupleElts, TC.Context);
      Type CreateTy = FunctionType::get(TT, SD->getDeclaredType(), TC.Context);
      cast<OneOfElementDecl>(SD->getMembers().back())->setType(CreateTy);
      cast<OneOfElementDecl>(SD->getMembers().back())->setArgumentType(TT);
    }

    if (!IsFirstPass)
      checkExplicitConformance(SD, SD->getDeclaredType(),
                               SD->getInherited());
  }

  void visitClassDecl(ClassDecl *CD) {
    if (!IsSecondPass) {
      checkInherited(CD, CD->getInherited());
      checkGenericParams(CD->getGenericParams());
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

    checkInherited(PD, PD->getInherited());
    
    // Assign archetypes each of the associated types.
    // FIXME: We need to build equivalence classes of associated types first,
    // then assign an archtype to each equivalence class.
    // FIXME: As part of building the equivalence class, find all of the
    // protocols that each archetype should conform to.
    for (auto Member : PD->getMembers()) {
      if (auto AssocType = dyn_cast<TypeAliasDecl>(Member)) {
        checkInherited(AssocType, AssocType->getInherited());

        Optional<unsigned> Index;
        // FIXME: Find a better way to identify the 'This' archetype.
        if (AssocType->getName().str().equals("This"))
          Index = 0;
        AssocType->setUnderlyingType(
          ArchetypeType::getNew(TC.Context, AssocType->getName().str(),
                                AssocType->getInherited(), Index));
      }
    }

    // Check the members.
    for (auto Member : PD->getMembers())
      visit(Member);
  }
  
  void visitVarDecl(VarDecl *VD) {
    // Delay type-checking on VarDecls until we see the corresponding
    // PatternBindingDecl.
  }

  void visitFuncDecl(FuncDecl *FD) {
    if (IsSecondPass)
      return;

    checkGenericParams(FD->getGenericParams());

    // Before anything else, set up the 'this' argument correctly.
    if (Type thisType = FD->computeThisType()) {
      FunctionType *fnType = cast<FunctionType>(FD->getType());
      FD->overwriteType(FunctionType::get(thisType, fnType->getResult(),
                                          TC.Context));

      if (FuncExpr *body = FD->getBody()) {
        body->setType(FD->getType());
        TypedPattern *thisPattern =
          cast<TypedPattern>(body->getParamPatterns()[0]);
        assert(thisPattern->getType()->isUnresolvedType());
        thisPattern->overwriteType(thisType);
      }
    }

    if (visitValueDecl(FD)) {
      if (FD->getBody())
        FD->getBody()->setType(FD->getType());
    }
  }

  void visitOneOfElementDecl(OneOfElementDecl *ED) {
    if (IsSecondPass)
      return;

    // Ignore element decls that carry no type.
    if (ED->getArgumentType().isNull()) return;
      
    // Validate the function type.
    if (TC.validateType(ED, IsFirstPass)) return;

    // Require the carried type to be materializable.
    if (!ED->getArgumentType()->isMaterializable()) {
      TC.diagnose(ED->getLoc(),
                  diag::oneof_element_not_materializable);
    }
  }

  void visitExtensionDecl(ExtensionDecl *ED) {
    if (!IsSecondPass) {
      if (TC.validateType(ED->getExtendedType(), IsFirstPass)) {
        ED->setExtendedType(ErrorType::get(TC.Context));
      } else {
        Type ExtendedTy = ED->getExtendedType();
        if (!ExtendedTy->is<OneOfType>() && !ExtendedTy->is<StructType>() &&
            !ExtendedTy->is<ClassType>() && !ExtendedTy->is<ErrorType>()) {
          TC.diagnose(ED->getStartLoc(), diag::non_nominal_extension,
                      ExtendedTy->is<ProtocolType>(), ExtendedTy);
          // FIXME: It would be nice to point out where we found the named type
          // declaration, if any.
        }
      
        checkInherited(ED, ED->getInherited());
      }
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

    visitValueDecl(CD);

    Type ThisTy = CD->computeThisType();
    Type FnTy = FunctionType::get(CD->getType(), 
                                 TupleType::getEmpty(TC.Context),
                                 TC.Context);
    FnTy = FunctionType::get(ThisTy, FnTy, TC.Context);
    CD->overwriteType(FnTy);
    CD->getImplicitThisDecl()->setType(ThisTy);
  }
};
}; // end anonymous namespace.


void TypeChecker::typeCheckDecl(Decl *D, bool isFirstPass) {
  bool isSecondPass = !isFirstPass && D->getDeclContext()->isModuleContext();
  DeclChecker(*this, isFirstPass, isSecondPass).visit(D);
}

bool DeclChecker::visitValueDecl(ValueDecl *VD) {
  if (TC.validateType(VD, IsFirstPass))
    return true;

  if (!VD->getType()->isMaterializable()) {
    TC.diagnose(VD->getStartLoc(), diag::var_type_not_materializable,
                VD->getType());
    VD->overwriteType(ErrorType::get(TC.Context));
  }

  validateAttributes(VD);
  return false;
}


/// validateAttributes - Check that the func/var declaration attributes are ok.
void DeclChecker::validateAttributes(ValueDecl *VD) {
  const DeclAttributes &Attrs = VD->getAttrs();
  Type Ty = VD->getType();
  
  // Get the number of lexical arguments, for semantic checks below.
  int NumArguments = -1;
  if (AnyFunctionType *FT = dyn_cast<AnyFunctionType>(Ty))
    if (TupleType *TT = dyn_cast<TupleType>(FT->getInput()))
      NumArguments = TT->getFields().size();

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
    } else {
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

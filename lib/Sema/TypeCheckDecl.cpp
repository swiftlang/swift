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
  
  DeclChecker(TypeChecker &TC) : TC(TC) {}

  //===--------------------------------------------------------------------===//
  // Helper Functions.
  //===--------------------------------------------------------------------===//

  bool visitValueDecl(ValueDecl *VD);
  void validateAttributes(ValueDecl *VD);

  /// \brief Check the list of inherited protocols on the declaration D (which
  /// declares or extends nominal type Ty).
  ///
  /// \param CheckConformance Will be set true if the caller needs to call
  /// checkExplicitConformance() for this declaration once its member have
  /// been type-checked. Otherwise, the conformance check can be delayed.
  void checkInherited(Decl *D, Type T, MutableArrayRef<Type> Inherited,
                      bool &CheckConformance) {
    // Check the list of inherited protocols.
    bool ConformsToProtocols = false;
    for (auto InheritedTy : Inherited) {
      if (!TC.validateType(InheritedTy)) {
        if (!InheritedTy->is<ProtocolType>()) {
          // FIXME: Terrible location information.
          TC.diagnose(D->getLocStart(), diag::nonprotocol_inherit, InheritedTy);
        } else {
          ConformsToProtocols = true;
        }
      }
    }
    
    CheckConformance = ConformsToProtocols &&
      !D->getDeclContext()->isModuleContext() &&
      !isa<ProtocolDecl>(D);
  }

  void checkExplicitConformance(Decl *D, Type T,
                                MutableArrayRef<Type> Inherited) {
    for (auto InheritedTy : Inherited) {
      // FIXME: Poor location info.
      if (auto Proto = InheritedTy->getAs<ProtocolType>())
        TC.conformsToProtocol(T, Proto->getDecl(), D->getLocStart());
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
    if (PBD->getInit() && ((TC.TU.Kind != TranslationUnit::Library) ||
                           PBD->getDeclContext()->isLocalContext())) {
      Type DestTy;
      if (PBD->getPattern()->hasType()) {
        DestTy = PBD->getPattern()->getType();
        if (TC.validateType(DestTy))
          return;
        if (DestTy->isDependentType())
          DestTy = Type();
      }
      Expr *Init = PBD->getInit();
      if (TC.typeCheckExpression(Init, DestTy)) {
        if (DestTy)
          TC.diagnose(PBD->getLocStart(), diag::while_converting_var_init,
                      DestTy);
        return;
      }
      if (!DestTy) {
        Expr *newInit = TC.convertToMaterializable(Init);
        if (newInit) Init = newInit;
      }
      PBD->setInit(Init);
      if (!DestTy) {
        if (TC.coerceToType(PBD->getPattern(), Init->getType()))
          return;
      } else {
        if (TC.typeCheckPattern(PBD->getPattern()))
          return;
      }
    } else {
      if (TC.typeCheckPattern(PBD->getPattern()))
        return;
    }
    visitBoundVars(PBD->getPattern());
  }

  void visitSubscriptDecl(SubscriptDecl *SD) {
    // The getter and setter functions will be type-checked separately.
    if (!SD->getDeclContext()->isTypeContext())
      TC.diagnose(SD->getLocStart(), diag::subscript_not_member);
    
    if (SD->getIndices()->hasType())
      SD->setType(FunctionType::get(SD->getIndices()->getType(),
                                    SD->getElementType(), TC.Context));
    else
      SD->setType(ErrorType::get(TC.Context));
  }
  
  void visitTypeAliasDecl(TypeAliasDecl *TAD) {
    TC.validateType(TAD->getAliasType());
  }
  
  void visitOneOfDecl(OneOfDecl *OOD) {
    bool CheckConformance;
    checkInherited(OOD, OOD->getDeclaredType(), OOD->getInherited(),
                   CheckConformance);
    
    for (auto elt : OOD->getElements())
      visitOneOfElementDecl(elt);
    
    if (CheckConformance)
      checkExplicitConformance(OOD, OOD->getDeclaredType(),
                               OOD->getInherited());
  }

  void visitStructDecl(StructDecl *SD) {
    bool CheckConformance;
    checkInherited(SD, SD->getDeclaredType(), SD->getInherited(),
                   CheckConformance);
    
    for (Decl *Member : SD->getMembers()) {
      visit(Member);
    }
    
    // FIXME: We should come up with a better way to represent this implied
    // constructor.
    SmallVector<TupleTypeElt, 8> TupleElts;
    for (Decl *Member : SD->getMembers())
      if (VarDecl *VarD = dyn_cast<VarDecl>(Member))
        if (!VarD->isProperty())
          TupleElts.push_back(TupleTypeElt(VarD->getType(), VarD->getName()));
    TupleType *TT = TupleType::get(TupleElts, TC.Context);
    Type CreateTy = FunctionType::get(TT, SD->getDeclaredType(), TC.Context);
    cast<OneOfElementDecl>(SD->getMembers().back())->setType(CreateTy);
    cast<OneOfElementDecl>(SD->getMembers().back())->setArgumentType(TT);
    
    if (CheckConformance)
      checkExplicitConformance(SD, SD->getDeclaredType(),
                               SD->getInherited());
  }

  void visitClassDecl(ClassDecl *CD) {
    bool CheckConformance;
    checkInherited(CD, CD->getDeclaredType(), CD->getInherited(),
                   CheckConformance);

    for (Decl *Member : CD->getMembers()) {
      visit(Member);
    }
    
    if (CheckConformance)
      checkExplicitConformance(CD, CD->getDeclaredType(),
                               CD->getInherited());
  }

  void visitProtocolDecl(ProtocolDecl *PD) {
    bool CheckConformance;
    checkInherited(PD, PD->getDeclaredType(), PD->getInherited(),
                   CheckConformance);
    
    // Check the members.
    for (auto Member : PD->getMembers())
      visit(Member);
  }
  
  void visitVarDecl(VarDecl *VD) {
    // Delay type-checking on VarDecls until we see the corresponding
    // PatternBindingDecl.
  }

  void visitFuncDecl(FuncDecl *FD) {
    // Before anything else, set up the 'this' argument correctly.
    if (Type thisType = FD->computeThisType()) {
      FunctionType *fnType = cast<FunctionType>(FD->getType());
      FD->overwriteType(FunctionType::get(thisType, fnType->getResult(),
                                          TC.Context));

      if (FuncExpr *body = FD->getBody()) {
        body->setType(FD->getType());
        TypedPattern *thisPattern =
          cast<TypedPattern>(body->getParamPatterns()[0]);
        assert(thisPattern->getType()->isDependentType());
        thisPattern->overwriteType(thisType);
      }
    }

    visitValueDecl(FD);

    assert((!FD->getBody() ||
            FD->getType()->isEqual(FD->getBody()->getType())) &&
           "Unexpected func body");
  }
  void visitOneOfElementDecl(OneOfElementDecl *ED) {
    // Ignore element decls that carry no type.
    if (ED->getArgumentType().isNull()) return;
      
    // Validate the function type.
    if (TC.validateType(ED)) return;

    // Require the carried type to be materializable.
    if (!ED->getArgumentType()->isMaterializable()) {
      TC.diagnose(ED->getIdentifierLoc(),
                  diag::oneof_element_not_materializable);
    }
  }
  void visitExtensionDecl(ExtensionDecl *ED) {
    bool CheckConformance;
    if (!TC.validateType(ED->getExtendedType()))
      checkInherited(ED, ED->getExtendedType(), ED->getInherited(),
                     CheckConformance);

    for (Decl *Member : ED->getMembers()) {
      // First recursively type check each thing in the extension.
      visit(Member);
    }
  }

  void visitTopLevelCodeDecl(TopLevelCodeDecl *TLCD) {
    // See swift::performTypeChecking for TopLevelCodeDecl handling.
    llvm_unreachable("TopLevelCodeDecls are handled elsewhere");
  }
};
}; // end anonymous namespace.


void TypeChecker::typeCheckDecl(Decl *D) {
  DeclChecker(*this).visit(D);
}

bool DeclChecker::visitValueDecl(ValueDecl *VD) {
  if (TC.validateType(VD))
    return true;

  if (!VD->getType()->isMaterializable()) {
    TC.diagnose(VD->getLocStart(), diag::var_type_not_materializable,
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
  if (FunctionType *FT = dyn_cast<FunctionType>(Ty))
    if (TupleType *TT = dyn_cast<TupleType>(FT->getInput()))
      NumArguments = TT->getFields().size();

  // Operators must be declared with 'func', not 'var'.
  if (VD->isOperator() && !isa<FuncDecl>(VD)) {
    TC.diagnose(VD->getLocStart(), diag::operator_not_func);
    // FIXME: Set the 'isError' bit on the decl.
    return;
  }
  
  if (VD->isOperator() && (NumArguments == 0 || NumArguments > 2)) {
    TC.diagnose(VD->getLocStart(), diag::invalid_arg_count_for_operator);
    VD->getMutableAttrs().Infix = InfixData();
    // FIXME: Set the 'isError' bit on the decl.
    return;
  }

  // The unary operator '&' cannot be overloaded.  In an expression,
  // the parser never interprets this as a normal unary operator
  // anyway.
  if (VD->isOperator() && NumArguments == 1 &&
      VD->getName().str() == "&") {
    TC.diagnose(VD->getLocStart(), diag::custom_operator_addressof);
    return;
  }
  
  // If the decl has an infix precedence specified, then it must be a function
  // whose input is a two element tuple.
  if (Attrs.isInfix() && NumArguments != 2) {
    TC.diagnose(Attrs.LSquareLoc, diag::invalid_infix_left_input);
    VD->getMutableAttrs().Infix = InfixData();
    // FIXME: Set the 'isError' bit on the decl.
    return;
  }

  if (Attrs.isInfix() && !VD->isOperator()) {
    TC.diagnose(VD->getLocStart(), diag::infix_left_not_an_operator);
    VD->getMutableAttrs().Infix = InfixData();
    // FIXME: Set the 'isError' bit on the decl.
    return;
  }

  // Only var and func decls can be infix.
  if (Attrs.isInfix() && !isa<VarDecl>(VD) && !isa<FuncDecl>(VD)) {
    TC.diagnose(VD->getLocStart(), diag::infix_left_invalid_on_decls);
    VD->getMutableAttrs().Infix = InfixData();
  }

  if (Attrs.isAssignment()) {
    // Only function declarations can be assignments.
    if (!isa<FuncDecl>(VD) || !VD->isOperator()) {
      TC.diagnose(VD->getLocStart(), diag::invalid_decl_attribute,"assignment");
      VD->getMutableAttrs().Assignment = false;
    } else if (NumArguments < 1) {
      TC.diagnose(VD->getLocStart(), diag::assignment_without_byref);
      VD->getMutableAttrs().Assignment = false;
    } else {
      auto FT = VD->getType()->castTo<FunctionType>();
      Type ParamType = FT->getInput();
      TupleType *ParamTT = ParamType->getAs<TupleType>();
      if (ParamTT)
        ParamType = ParamTT->getElementType(0);
      
      if (!ParamType->is<LValueType>()) {
        TC.diagnose(VD->getLocStart(), diag::assignment_without_byref);
        VD->getMutableAttrs().Assignment = false;
      } else if (!FT->getResult()->isEqual(TupleType::getEmpty(TC.Context))) {
        TC.diagnose(VD->getLocStart(), diag::assignment_nonvoid,
                    FT->getResult());
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
      TC.diagnose(VD->getLocStart(), diag::binops_infix_left);
  }

  if (Attrs.isByref()) {
    TC.diagnose(VD->getLocStart(), diag::invalid_decl_attribute, "byref");
    VD->getMutableAttrs().Byref = false;
  }

  if (Attrs.isAutoClosure()) {
    TC.diagnose(VD->getLocStart(), diag::invalid_decl_attribute, "auto_closure");
    VD->getMutableAttrs().AutoClosure = false;
  }  
}

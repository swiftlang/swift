//===--- TypeCheckAttr.cpp - Type Checking for Attributes -----------------===//
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
// This file implements semantic analysis for attributes.
//
//===----------------------------------------------------------------------===//

#include "TypeChecker.h"
#include "swift/AST/ASTVisitor.h"

using namespace swift;

namespace {
class AttributeEarlyChecker : public AttributeVisitor<AttributeEarlyChecker> {
  TypeChecker &TC;
  Decl *D;

public:
  AttributeEarlyChecker(TypeChecker &TC, Decl *D) : TC(TC), D(D) {}

  /// Deleting this ensures that all attributes are covered by the visitor
  /// below.
  void visitDeclAttribute(DeclAttribute *A) = delete;

  void visitAsmnameAttr(AsmnameAttr *attr) {}

  void visitAssignmentAttr(AssignmentAttr *attr);

  void visitAvailabilityAttr(AvailabilityAttr *attr) {}

  void visitClassProtocolAttr(ClassProtocolAttr *attr) {}

  void visitExportedAttr(ExportedAttr *attr);

  void visitFinalAttr(FinalAttr *attr) {}

  void visitNSCopyingAttr(NSCopyingAttr *attr) {}

  void visitNoReturnAttr(NoReturnAttr *attr) {}

  void visitObjCAttr(ObjCAttr *attr) {}

  void visitOverrideAttr(OverrideAttr *attr);

  void visitRequiredAttr(RequiredAttr *attr) {}
};
} // end anonymous namespace

void AttributeEarlyChecker::visitAssignmentAttr(AssignmentAttr *attr) {
  // Only function declarations can be assignments.
  auto *FD = dyn_cast<FuncDecl>(D);
  if (!FD || !FD->isOperator()) {
    TC.diagnose(attr->getLocation(), diag::invalid_decl_attribute,
                attr->getKind())
        .fixItRemove(attr->getRange());
    attr->setInvalid();
    return;
  }
}

void AttributeEarlyChecker::visitExportedAttr(ExportedAttr *attr) {
  if (!isa<ImportDecl>(D)) {
    TC.diagnose(attr->getLocation(), diag::invalid_decl_attribute,
                attr->getKind())
        .fixItRemove(attr->getRange());
    attr->setInvalid();
  }
}

void AttributeEarlyChecker::visitOverrideAttr(OverrideAttr *attr) {
  if (!isa<ClassDecl>(D->getDeclContext()) &&
      !isa<ExtensionDecl>(D->getDeclContext())) {
    TC.diagnose(D, diag::override_nonclass_decl)
        .fixItRemove(attr->getRange());
    attr->setInvalid();
  }
}

void TypeChecker::checkDeclAttributesEarly(Decl *D) {
  AttributeEarlyChecker Checker(*this, D);

  for (auto attr : D->getMutableAttrs()) {
    if (attr->isValid())
      Checker.visit(attr);
  }
}

namespace {
class AttributeChecker : public AttributeVisitor<AttributeChecker> {
  TypeChecker &TC;
  Decl *D;

public:
  AttributeChecker(TypeChecker &TC, Decl *D) : TC(TC), D(D) {}

  /// Deleting this ensures that all attributes are covered by the visitor
  /// below.
  void visitDeclAttribute(DeclAttribute *A) = delete;

#define UNINTERESTING_ATTR(CLASS)                                              \
    void visit##CLASS##Attr(CLASS##Attr *) {}

    UNINTERESTING_ATTR(Asmname)
    UNINTERESTING_ATTR(Exported)
    UNINTERESTING_ATTR(ObjC)
    UNINTERESTING_ATTR(Override)

#undef UNINTERESTING_ATTR

  void visitAvailabilityAttr(AvailabilityAttr *attr) {
    // FIXME: Check that this declaration is at least as available as the
    // one it overrides.
  }

  void visitAssignmentAttr(AssignmentAttr *attr);

  void visitClassProtocolAttr(ClassProtocolAttr *attr);

  void visitFinalAttr(FinalAttr *attr);

  void visitNSCopyingAttr(NSCopyingAttr *attr);

  void visitNoReturnAttr(NoReturnAttr *attr);

  void visitRequiredAttr(RequiredAttr *attr);
};
} // end anonymous namespace

void AttributeChecker::visitAssignmentAttr(AssignmentAttr *attr) {
  auto *FD = cast<FuncDecl>(D);
  auto *FT = FD->getType()->castTo<AnyFunctionType>();

  int NumArguments = -1;
  if (FD->getDeclContext()->isTypeContext() && FD->isStatic())
    FT = FT->getResult()->castTo<AnyFunctionType>();
  if (auto *TT = FT->getInput()->getAs<TupleType>())
    NumArguments = TT->getFields().size();

  if (NumArguments < 1) {
    TC.diagnose(attr->getLocation(), diag::assignment_without_inout);
    attr->setInvalid();
    return;
  }

  Type ParamType = FT->getInput();
  TupleType *ParamTT = ParamType->getAs<TupleType>();
  if (ParamTT)
    ParamType = ParamTT->getElementType(0);

  if (!ParamType->is<InOutType>()) {
    TC.diagnose(attr->getLocation(), diag::assignment_without_inout);
    attr->setInvalid();
  }
}

void AttributeChecker::visitClassProtocolAttr(ClassProtocolAttr *attr) {
  // Only protocols can have the @class_protocol attribute.
  if (!isa<ProtocolDecl>(D)) {
    TC.diagnose(attr->getLocation(),
                diag::class_protocol_not_protocol);
    attr->setInvalid();
  }
}

void AttributeChecker::visitFinalAttr(FinalAttr *attr) {
  // @final on classes marks all members with @final.
  if (isa<ClassDecl>(D))
    return;

  // The @final attribute only makes sense in the context of a class
  // declaration.  Reject it on global functions, structs, enums, etc.
  auto typeContext = D->getDeclContext()->getDeclaredTypeInContext();
  auto contextTypeDecl =
    typeContext ? typeContext->getNominalOrBoundGenericNominal() : nullptr;
  if (!contextTypeDecl || !isa<ClassDecl>(contextTypeDecl)) {
    TC.diagnose(attr->getLocation(), diag::member_cannot_be_final);
    return;
  }

  // We currently only support @final on var/let, func and subscript
  // declarations.
  if (!isa<VarDecl>(D) && !isa<FuncDecl>(D) && !isa<SubscriptDecl>(D)) {
    TC.diagnose(attr->getLocation(), diag::final_not_allowed_here);
    return;
  }

  if (auto *FD = dyn_cast<FuncDecl>(D)) {
    if (FD->isAccessor() && !attr->isImplicit()) {
      unsigned Kind = 2;
      if (auto *VD = dyn_cast<VarDecl>(FD->getAccessorStorageDecl()))
        Kind = VD->isLet() ? 1 : 0;
      TC.diagnose(attr->getLocation(), diag::final_not_on_accessors, Kind);
      return;
    }
  }
}

void AttributeChecker::visitNSCopyingAttr(NSCopyingAttr *attr) {
  // The @NSCopying attribute is only allowed on stored properties.
  auto *VD = dyn_cast<VarDecl>(D);
  if (!VD) {
    TC.diagnose(attr->getLocation(), diag::nscopying_only_on_class_properties);
    attr->setInvalid();
    return;
  }

  // It may only be used on class members.
  auto typeContext = D->getDeclContext()->getDeclaredTypeInContext();
  auto contextTypeDecl =
  typeContext ? typeContext->getNominalOrBoundGenericNominal() : nullptr;
  if (!contextTypeDecl || !isa<ClassDecl>(contextTypeDecl)) {
    TC.diagnose(attr->getLocation(), diag::nscopying_only_on_class_properties);
    attr->setInvalid();
    return;
  }

  if (!VD->isSettable(VD->getDeclContext())) {
    TC.diagnose(attr->getLocation(), diag::nscopying_only_mutable);
    attr->setInvalid();
    return;
  }

  if (!VD->hasStorage()) {
    TC.diagnose(attr->getLocation(), diag::nscopying_only_stored_property);
    attr->setInvalid();
    return;
  }

  assert(VD->getOverriddenDecl() == nullptr &&
         "Can't have value with storage that is an override");

  // Check the type.  It must be must be [unchecked]optional, weak, a normal
  // class, AnyObject, or classbound protocol.
  // must conform to the NSCopying protocol.
  
}


void AttributeChecker::visitNoReturnAttr(NoReturnAttr *attr) {
  auto *FD = dyn_cast<FuncDecl>(D);
  if (!FD) {
    TC.diagnose(attr->getLocation(), diag::invalid_decl_attribute,
                attr->getKind())
        .fixItRemove(attr->getRange());
    attr->setInvalid();
    return;
  }
}

void AttributeChecker::visitRequiredAttr(RequiredAttr *attr) {
  // The required attribute only applies to constructors.
  auto ctor = dyn_cast<ConstructorDecl>(D);
  if (!ctor) {
    TC.diagnose(attr->getLocation(), diag::required_non_initializer);
    attr->setInvalid();
    return;
  }
  auto parentTy = ctor->getExtensionType();
  if (!parentTy) {
    // Constructor outside of nominal type context; we've already complained
    // elsewhere.
    attr->setInvalid();
    return;
  }
  // Only classes can have required constructors.
  if (parentTy->getClassOrBoundGenericClass()) {
    // The constructor must be declared within the class itself.
    if (!isa<ClassDecl>(ctor->getDeclContext())) {
      TC.diagnose(ctor, diag::required_initializer_in_extension, parentTy)
        .highlight(attr->getLocation());
      attr->setInvalid();
      return;
    }
  } else {
    if (!parentTy->is<ErrorType>()) {
      TC.diagnose(ctor, diag::required_initializer_nonclass, parentTy)
        .highlight(attr->getLocation());
    }
    attr->setInvalid();
    return;
  }
}

// FIXME: Merge validateAttributes into this.
void TypeChecker::checkDeclAttributes(Decl *D) {
  AttributeChecker Checker(*this, D);

  for (auto attr : D->getMutableAttrs()) {
    if (attr->isValid())
      Checker.visit(attr);
  }
}

void TypeChecker::checkOwnershipAttr(VarDecl *var, Ownership ownershipKind) {
  Type type = var->getType();

  // Just stop if we've already processed this declaration.
  if (type->is<ReferenceStorageType>())
    return;

  // A @weak variable must have type R?, possibly @unchecked, for
  // some ownership-capable type R.
  if (ownershipKind == Ownership::Weak) {
    Type objType = type->getAnyOptionalObjectType();

    // Use this special diagnostic if it's actually a reference type
    // but just isn't Optional.
    if (!objType && type->allowsOwnership()) {
      diagnose(var->getStartLoc(),
               diag::invalid_weak_ownership_not_optional,
               OptionalType::get(type));
      var->getMutableAttrs().clearOwnership();
      return;
    } else if (objType) {
      type = objType;
    }
  }

  if (!type->allowsOwnership()) {
    // If we have an opaque type, suggest the possibility of adding
    // a class bound.
    if (type->isExistentialType() || type->is<ArchetypeType>()) {
      diagnose(var->getStartLoc(), diag::invalid_ownership_opaque_type,
               (unsigned) ownershipKind, type);
    } else {
      diagnose(var->getStartLoc(), diag::invalid_ownership_type,
               (unsigned) ownershipKind, type);
    }
    var->getMutableAttrs().clearOwnership();
    return;
  }

  // Change the type to the appropriate reference storage type.
  var->overwriteType(ReferenceStorageType::get(type, ownershipKind, Context));
}

void TypeChecker::checkIBOutlet(VarDecl *VD) {
  const DeclAttributes &Attrs = VD->getAttrs();
  assert(Attrs.isIBOutlet() && "Only call when @IBOutlet is set");

  auto isInClassContext = [](Decl *vd) {
   Type ContextTy = vd->getDeclContext()->getDeclaredTypeInContext();
    if (!ContextTy)
      return false;
    return bool(ContextTy->getClassOrBoundGenericClass());
  };

  // Only instance properties can be IBOutlets.
  if (!isInClassContext(VD) || VD->isStatic()) {
    diagnose(Attrs.getLoc(AK_IBOutlet), diag::invalid_iboutlet);
    VD->getMutableAttrs().clearAttribute(AK_IBOutlet);
    return;
  }
  
  // If the variable has no type yet, we can't perform any validation.
  if (!VD->hasType())
    return;

  // Validate the type of the @IBOutlet.
  auto type = VD->getType();
  bool isOptional = false;
  Ownership ownership = Ownership::Strong;
  if (auto refStorageType = type->getAs<ReferenceStorageType>()) {
    ownership = refStorageType->getOwnership();
    type = refStorageType->getReferentType();
  }
  if (auto optObjectType = type->getAnyOptionalObjectType()) {
    isOptional = true;
    type = optObjectType;
  }
  
  if (auto classDecl = type->getClassOrBoundGenericClass()) {
    // @objc class types are okay.
    if (!classDecl->isObjC()) {
      diagnose(VD->getLoc(), diag::iboutlet_nonobjc_class, type);
      VD->getMutableAttrs().clearAttribute(AK_IBOutlet);
      return;
    }
  } else if (type->isObjCExistentialType()) {
    // @objc existential types are okay
    // Nothing to do.
  } else if (type->getAnyNominal() == Context.getStringDecl()) {
    // String is okay because it is bridged to NSString.
    // FIXME: BridgesTypes.def is almost sufficient for this.
  } else {
    // No other types are permitted.
    diagnose(VD->getLoc(), diag::iboutlet_nonobject_type, type);
    VD->getMutableAttrs().clearAttribute(AK_IBOutlet);
    return;
  }
  
  // If the type wasn't optional before, turn it into an @unchecked optional
  // now.
  if (!isOptional) {
    type = UncheckedOptionalType::get(type);
    if (ownership != Ownership::Strong)
      type = ReferenceStorageType::get(type, ownership, Context);
    VD->overwriteType(type);
  }
}

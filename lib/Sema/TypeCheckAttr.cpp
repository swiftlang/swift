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

static bool isInClassContext(const Decl *D) {
  Type ContextTy = D->getDeclContext()->getDeclaredTypeInContext();
  if (!ContextTy)
    return false;
  return bool(ContextTy->getClassOrBoundGenericClass());
}

namespace {
/// This visits each attribute on a decl early, before the majority of type
/// checking has been performed for the decl.  The visitor should return true if
/// the attribute is invalid and should be marked as such.
class AttributeEarlyChecker : public AttributeVisitor<AttributeEarlyChecker> {
  TypeChecker &TC;
  Decl *D;

public:
  AttributeEarlyChecker(TypeChecker &TC, Decl *D) : TC(TC), D(D) {}

  /// This emits a diagnostic with a fixit to remove the attribute.
  template<typename ...ArgTypes>
  void diagnoseAndRemoveAttr(DeclAttribute *attr, ArgTypes &&...Args) {
    TC.diagnose(attr->getLocation(), std::forward<ArgTypes>(Args)...)
      .fixItRemove(attr->getRange());
    attr->setInvalid();
  }

  /// Deleting this ensures that all attributes are covered by the visitor
  /// below.
  bool visitDeclAttribute(DeclAttribute *A) = delete;

#define IGNORED_ATTR(X) void visit##X##Attr(X##Attr *) {}
  IGNORED_ATTR(Asmname)
  IGNORED_ATTR(Availability)
  IGNORED_ATTR(ClassProtocol)
  IGNORED_ATTR(Final)
  IGNORED_ATTR(NSCopying)
  IGNORED_ATTR(NSManaged)
  IGNORED_ATTR(NoReturn)
  IGNORED_ATTR(ObjC)
  IGNORED_ATTR(RawDocComment)
  IGNORED_ATTR(Required)
#undef IGNORED_ATTR

  void visitIBActionAttr(IBActionAttr *attr);
  void visitLazyAttr(LazyAttr *attr);
  void visitIBDesignableAttr(IBDesignableAttr *attr);
  void visitIBInspectableAttr(IBInspectableAttr *attr);
  void visitIBOutletAttr(IBOutletAttr *attr);
  void visitAssignmentAttr(AssignmentAttr *attr);
  void visitExportedAttr(ExportedAttr *attr);
  void visitOverrideAttr(OverrideAttr *attr);
};
} // end anonymous namespace

void AttributeEarlyChecker::visitIBActionAttr(IBActionAttr *attr) {
  // Only instance methods returning () can be IBActions.
  const FuncDecl *FD = dyn_cast<FuncDecl>(D);
  if (!FD || !isInClassContext(D) || FD->isStatic() || FD->isAccessor())
    return diagnoseAndRemoveAttr(attr, diag::invalid_ibaction_decl);

}

void AttributeEarlyChecker::visitIBDesignableAttr(IBDesignableAttr *attr) {
  // Only classes can be marked with 'IBDesignable'.
  if (!isa<ClassDecl>(D))
    return diagnoseAndRemoveAttr(attr, diag::invalid_ibdesignable_decl);

}

void AttributeEarlyChecker::visitIBInspectableAttr(IBInspectableAttr *attr) {
  // Only instance properties can be 'IBInspectable'.
  auto *VD = dyn_cast<VarDecl>(D);
  if (!VD || !isInClassContext(VD) || VD->isStatic())
    return diagnoseAndRemoveAttr(attr, diag::invalid_ibinspectable);

}

void AttributeEarlyChecker::visitIBOutletAttr(IBOutletAttr *attr) {
  // Only instance properties can be 'IBOutlet'.
  auto *VD = dyn_cast<VarDecl>(D);
  if (!VD || !isInClassContext(VD) || VD->isStatic())
    return diagnoseAndRemoveAttr(attr, diag::invalid_iboutlet);

}

void AttributeEarlyChecker::visitAssignmentAttr(AssignmentAttr *attr) {
  // Only function declarations can be assignments.
  auto *FD = dyn_cast<FuncDecl>(D);
  if (!FD || !FD->isOperator())
    return diagnoseAndRemoveAttr(attr, diag::invalid_decl_attribute,
                                 attr->getKind());
}

void AttributeEarlyChecker::visitExportedAttr(ExportedAttr *attr) {
  if (!isa<ImportDecl>(D))
    return diagnoseAndRemoveAttr(attr, diag::invalid_decl_attribute,
                attr->getKind());

}

void AttributeEarlyChecker::visitOverrideAttr(OverrideAttr *attr) {
  if (!isa<ClassDecl>(D->getDeclContext()) &&
      !isa<ExtensionDecl>(D->getDeclContext()))
    return diagnoseAndRemoveAttr(attr, diag::override_nonclass_decl);
}

void AttributeEarlyChecker::visitLazyAttr(LazyAttr *attr) {
  // @lazy may only be used on properties.
  auto *VD = dyn_cast<VarDecl>(D);
  if (!VD)
    return diagnoseAndRemoveAttr(attr, diag::lazy_not_on_var);

  // It cannot currently be used on let's since we don't have a mutability model
  // that supports it.
  if (VD->isLet())
    return diagnoseAndRemoveAttr(attr, diag::lazy_not_on_let);

  // It only works with stored properties.
  if (!VD->hasStorage())
    return diagnoseAndRemoveAttr(attr, diag::lazy_not_on_computed);

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
    UNINTERESTING_ATTR(RawDocComment)
    UNINTERESTING_ATTR(Lazy)  // checked early.

#undef UNINTERESTING_ATTR

  void visitIBActionAttr(IBActionAttr *attr);
  void visitIBDesignableAttr(IBDesignableAttr *attr) {}
  void visitIBInspectableAttr(IBInspectableAttr *attr) {}

  void visitIBOutletAttr(IBOutletAttr *attr);

  void visitAvailabilityAttr(AvailabilityAttr *attr) {
    // FIXME: Check that this declaration is at least as available as the
    // one it overrides.
  }

  void visitAssignmentAttr(AssignmentAttr *attr);

  void visitClassProtocolAttr(ClassProtocolAttr *attr);

  void visitFinalAttr(FinalAttr *attr);

  void visitNSCopyingAttr(NSCopyingAttr *attr);

  void visitNSManagedAttr(NSManagedAttr *attr);

  void visitNoReturnAttr(NoReturnAttr *attr);

  void visitRequiredAttr(RequiredAttr *attr);
};
} // end anonymous namespace

static bool checkObjectOrOptionalObjectType(TypeChecker &TC, Decl *D,
                                            const Pattern *argPattern) {
  Type ty = argPattern->getType();
  if (auto unwrapped = ty->getAnyOptionalObjectType())
    ty = unwrapped;

  if (auto classDecl = ty->getClassOrBoundGenericClass()) {
    // @objc class types are okay.
    if (!classDecl->isObjC()) {
      TC.diagnose(D, diag::ibaction_nonobjc_class_argument,
                  argPattern->getType())
        .highlight(argPattern->getSourceRange());
      return true;
    }
  } else if (ty->isObjCExistentialType()) {
    // @objc existential types are okay
    // Nothing to do.
  } else {
    // No other types are permitted.
    TC.diagnose(D, diag::ibaction_nonobject_argument,
                argPattern->getSemanticsProvidingPattern()->getType())
      .highlight(argPattern->getSourceRange());
    return true;
  }

  return false;
}

static bool isiOS(TypeChecker &TC) {
  // FIXME: This is a very ugly way of checking the OS.
  return TC.getLangOpts().getTargetConfigOption("os") == "iOS";
}

void AttributeChecker::visitIBActionAttr(IBActionAttr *attr) {
  // IBActions instance methods must have type Class -> (...) -> ().
  auto *FD = cast<FuncDecl>(D);
  Type CurriedTy = FD->getType()->castTo<AnyFunctionType>()->getResult();
  Type ResultTy = CurriedTy->castTo<AnyFunctionType>()->getResult();
  if (!ResultTy->isEqual(TupleType::getEmpty(TC.Context))) {
    TC.diagnose(D, diag::invalid_ibaction_result, ResultTy);
    attr->setInvalid();
    return;
  }

  auto Arguments = FD->getBodyParamPatterns()[1];
  auto ArgTuple = dyn_cast<TuplePattern>(Arguments);

  bool iOSOnlyUsedOnOSX = false;
  bool Valid = true;
  if (ArgTuple) {
    auto fields = ArgTuple->getFields();
    switch (ArgTuple->getNumFields()) {
    case 0:
      // (iOS only) No arguments.
      if (!isiOS(TC)) {
        iOSOnlyUsedOnOSX = true;
        break;
      }
      break;
    case 1:
      // One argument.
      if (checkObjectOrOptionalObjectType(TC, D, fields[0].getPattern()))
        Valid = false;
      break;
    case 2:
      // (iOS only) Two arguments, the second of which is a UIEvent.
      // We don't currently enforce the UIEvent part.
      if (!isiOS(TC)) {
        iOSOnlyUsedOnOSX = true;
        break;
      }
      if (checkObjectOrOptionalObjectType(TC, D, fields[0].getPattern()))
        Valid = false;
      if (checkObjectOrOptionalObjectType(TC, D, fields[1].getPattern()))
        Valid = false;
      break;
    default:
      // No platform allows an action signature with more than two arguments.
      TC.diagnose(D, diag::invalid_ibaction_argument_count, isiOS(TC));
      Valid = false;
      break;
    }
  } else {
    // One argument without a name.
    if (checkObjectOrOptionalObjectType(TC, D, Arguments))
      Valid = false;
  }

  if (iOSOnlyUsedOnOSX) {
    TC.diagnose(D, diag::invalid_ibaction_argument_count, /*iOS=*/false);
    Valid = false;
  }

  if (!Valid)
    attr->setInvalid();
}

void AttributeChecker::visitIBOutletAttr(IBOutletAttr *attr) {
  TC.checkIBOutlet(cast<VarDecl>(D));
}

void AttributeChecker::visitAssignmentAttr(AssignmentAttr *attr) {
  auto *FD = cast<FuncDecl>(D);
  auto *FT = FD->getType()->castTo<AnyFunctionType>();

  int NumArguments = 1;
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

void AttributeChecker::visitNSManagedAttr(NSManagedAttr *attr) {
  // NSManaged only applies to non-class properties within a class.
  if (!isa<VarDecl>(D)) {
    TC.diagnose(attr->getLocation(), diag::attr_NSManaged_not_property)
      .fixItRemove(attr->getRange());
    attr->setInvalid();
    return;
  }

  // Note: all other validation for @NSManaged occurs when the declaration is
  // type-checked.
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

  // A weak variable must have type R? for some ownership-capable
  // type R.
  if (ownershipKind == Ownership::Weak) {
    if (var->isLet()) {
      diagnose(var->getStartLoc(), diag::invalid_weak_let);
      var->getMutableAttrs().clearOwnership();
      return;
    }

    OptionalTypeKind kind;
    Type objType = type->getAnyOptionalObjectType(kind);
    
    // Use this special diagnostic if it's actually a reference type
    // but just isn't Optional.
    if (!objType && type->allowsOwnership()) {
      diagnose(var->getStartLoc(),
               diag::invalid_weak_ownership_not_optional,
               OptionalType::get(type));
      var->getMutableAttrs().clearOwnership();
      return;
    }

    if (objType) {
      type = objType;

      // Cannot use an implicitly unwrapped optional with @weak.
      if (kind == OTK_ImplicitlyUnwrappedOptional) {
        // Find the location of the '!'.
        SourceLoc bangLoc;
        auto *pattern = var->getParentPattern()->getPattern();
        if (auto *varPattern = dyn_cast<VarPattern>(pattern)) {
          pattern = varPattern->getSubPattern();
        }
        if (auto *typedPattern = dyn_cast<TypedPattern>(pattern)) {
          auto typeRepr = typedPattern->getTypeLoc().getTypeRepr();
          if (auto unchecked = dyn_cast<ImplicitlyUnwrappedOptionalTypeRepr>(typeRepr)) {
            bangLoc = unchecked->getExclamationLoc();
          }
        }

        SourceLoc weakLoc = var->getAttrs().getLoc(AK_weak);
        diagnose(var->getStartLoc(), 
                 diag::invalid_weak_ownership_unchecked_optional,
                 type)
          .fixItReplace(weakLoc, "unowned")
          .fixItRemove(bangLoc);
        var->getMutableAttrs().clearOwnership();

        // FIXME: Fix the AST here.
        return;
      }
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
  assert(VD->getMutableAttrs().hasAttribute<IBOutletAttr>() &&
         "Only call when @IBOutlet is set");
  checkDeclAttributesEarly(VD);

  auto *attr = VD->getMutableAttrs().getAttribute<IBOutletAttr>();
  if (!attr)
    return;

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
  } else if (VD->getAttrs().hasOwnership()) {
    ownership = Ownership::Weak;
  }

  if (auto optObjectType = type->getAnyOptionalObjectType()) {
    isOptional = true;
    type = optObjectType;
  }

  auto nominal = type->getAnyNominal();
  if (auto classDecl = dyn_cast_or_null<ClassDecl>(nominal)) {
    // @objc class types are okay.
    if (!classDecl->isObjC()) {
      diagnose(VD->getLoc(), diag::iboutlet_nonobjc_class,
               /*array=*/false, type);
      attr->setInvalid();
      return;
    }
  } else if (type->isObjCExistentialType()) {
    // @objc existential types are okay
    // Nothing to do.
  } else if (nominal == Context.getStringDecl()) {
    // String is okay because it is bridged to NSString.
    // FIXME: BridgesTypes.def is almost sufficient for this.
  } else if (nominal == Context.getArrayDecl()) {
    // Handle Array<T>. T must be an Objective-C class or protocol.
    auto boundTy = type->castTo<BoundGenericStructType>();
    auto boundArgs = boundTy->getGenericArgs();
    assert(boundArgs.size() == 1 && "invalid Array declaration");
    Type elementTy = boundArgs.front();
    if (auto classDecl = elementTy->getClassOrBoundGenericClass()) {
      if (!classDecl->isObjC()) {
        diagnose(VD->getLoc(), diag::iboutlet_nonobjc_class,
                 /*array=*/true, type);
        attr->setInvalid();
        return;
      }
    } else if (elementTy->isObjCExistentialType()) {
      // okay
    } else {
      // No other element types are permitted.
      diagnose(VD->getLoc(), diag::iboutlet_nonobject_type,
               /*array=*/true, type);
      attr->setInvalid();
      return;
    }
  } else {
    // No other types are permitted.
    diagnose(VD->getLoc(), diag::iboutlet_nonobject_type,
             /*array=*/false, type);
    attr->setInvalid();
    return;
  }

  // If the type wasn't optional before, turn it into an implicitly unwrapped
  // optional now.
  // FIXME: Should arrays start out empty instead?
  if (!isOptional) {
    if (ownership == Ownership::Weak)
      type = ReferenceStorageType::get(type, ownership, Context);
    else
      type = ImplicitlyUnwrappedOptionalType::get(type);
    VD->overwriteType(type);
  }
}

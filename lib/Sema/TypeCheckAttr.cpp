//===--- TypeCheckAttr.cpp - Type Checking for Attributes -----------------===//
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
// This file implements semantic analysis for attributes.
//
//===----------------------------------------------------------------------===//

#include "TypeChecker.h"
#include "MiscDiagnostics.h"
#include "swift/AST/GenericSignatureBuilder.h"
#include "swift/AST/ASTVisitor.h"
#include "swift/AST/GenericEnvironment.h"
#include "swift/AST/NameLookup.h"
#include "swift/AST/ParameterList.h"
#include "swift/AST/Types.h"
#include "swift/Parse/Lexer.h"
#include "swift/ClangImporter/ClangModule.h" // FIXME: SDK overlay semantics
#include "llvm/Support/Debug.h"

using namespace swift;

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
      .fixItRemove(attr->getRangeWithAt());
    attr->setInvalid();
  }

  /// Deleting this ensures that all attributes are covered by the visitor
  /// below.
  bool visitDeclAttribute(DeclAttribute *A) = delete;

#define IGNORED_ATTR(X) void visit##X##Attr(X##Attr *) {}
  IGNORED_ATTR(CDecl)
  IGNORED_ATTR(SILGenName)
  IGNORED_ATTR(Available)
  IGNORED_ATTR(Convenience)
  IGNORED_ATTR(Effects)
  IGNORED_ATTR(Exported)
  IGNORED_ATTR(FixedLayout)
  IGNORED_ATTR(Infix)
  IGNORED_ATTR(Inline)
  IGNORED_ATTR(Inlineable)
  IGNORED_ATTR(NSApplicationMain)
  IGNORED_ATTR(NSCopying)
  IGNORED_ATTR(NonObjC)
  IGNORED_ATTR(ObjC)
  IGNORED_ATTR(ObjCBridged)
  IGNORED_ATTR(ObjCNonLazyRealization)
  IGNORED_ATTR(Optional)
  IGNORED_ATTR(Postfix)
  IGNORED_ATTR(Prefix)
  IGNORED_ATTR(RawDocComment)
  IGNORED_ATTR(Required)
  IGNORED_ATTR(RequiresStoredPropertyInits)
  IGNORED_ATTR(Rethrows)
  IGNORED_ATTR(Semantics)
  IGNORED_ATTR(Specialize)
  IGNORED_ATTR(SwiftNativeObjCRuntimeBase)
  IGNORED_ATTR(SynthesizedProtocol)
  IGNORED_ATTR(Testable)
  IGNORED_ATTR(UIApplicationMain)
  IGNORED_ATTR(UnsafeNoObjCTaggedPointer)
  IGNORED_ATTR(Versioned)
  IGNORED_ATTR(ShowInInterface)
  IGNORED_ATTR(DiscardableResult)
#undef IGNORED_ATTR

  // @noreturn has been replaced with a 'Never' return type.
  void visitNoReturnAttr(NoReturnAttr *attr) {
    if (auto FD = dyn_cast<FuncDecl>(D)) {
      auto &SM = TC.Context.SourceMgr;

      auto diag = TC.diagnose(attr->getLocation(),
                              diag::noreturn_not_supported);
      auto range = attr->getRangeWithAt();
      if (range.isValid())
        range.End = range.End.getAdvancedLoc(1);
      diag.fixItRemove(range);

      auto *last = FD->getParameterList(FD->getNumParameterLists() - 1);

      // If the declaration already has a result type, we're going
      // to change it to 'Never'.
      bool hadResultType = false;
      bool isEndOfLine = false;
      SourceLoc resultLoc;
      if (FD->getBodyResultTypeLoc().hasLocation()) {
        const auto &typeLoc = FD->getBodyResultTypeLoc();
        hadResultType = true;
        resultLoc = typeLoc.getSourceRange().Start;

      // If the function 'throws', insert the result type after the
      // 'throws'.
      } else {
        if (FD->getThrowsLoc().isValid()) {
          resultLoc = FD->getThrowsLoc();

        // Otherwise, insert the result type after the final parameter
        // list.
        } else if (last->getRParenLoc().isValid()) {
          resultLoc = last->getRParenLoc();
        }

        if (Lexer::getLocForEndOfToken(SM, resultLoc).getAdvancedLoc(1) ==
            Lexer::getLocForEndOfLine(SM, resultLoc))
          isEndOfLine = true;

        resultLoc = Lexer::getLocForEndOfToken(SM, resultLoc);
      }

      if (hadResultType) {
        diag.fixItReplace(resultLoc, "Never");
      } else {
        std::string fix = " -> Never";

        if (!isEndOfLine)
          fix = fix + " ";

        diag.fixItInsert(resultLoc, fix);
      }

      FD->getBodyResultTypeLoc() = TypeLoc::withoutLoc(
          TC.Context.getNeverType());
    }
  }

  void visitAlignmentAttr(AlignmentAttr *attr) {
    // Alignment must be a power of two.
    unsigned value = attr->Value;
    if (value == 0 || (value & (value - 1)) != 0)
      TC.diagnose(attr->getLocation(), diag::alignment_not_power_of_two);
  }

  void visitAutoClosureAttr(AutoClosureAttr *attr) {
    TC.checkAutoClosureAttr(cast<ParamDecl>(D), attr);
  }
  void visitNoEscapeAttr(NoEscapeAttr *attr) {
    TC.checkNoEscapeAttr(cast<ParamDecl>(D), attr);
  }

  void visitTransparentAttr(TransparentAttr *attr);
  void visitMutationAttr(DeclAttribute *attr);
  void visitMutatingAttr(MutatingAttr *attr) { visitMutationAttr(attr); }
  void visitNonMutatingAttr(NonMutatingAttr *attr) { visitMutationAttr(attr); }
  void visitDynamicAttr(DynamicAttr *attr);

  void visitOwnershipAttr(OwnershipAttr *attr) {
    TC.checkOwnershipAttr(cast<VarDecl>(D), attr);
  }

  void visitFinalAttr(FinalAttr *attr) {
    // Reject combining 'final' with 'open'.
    if (auto accessibility = D->getAttrs().getAttribute<AccessibilityAttr>()) {
      if (accessibility->getAccess() == Accessibility::Open) {
        TC.diagnose(attr->getLocation(), diag::open_decl_cannot_be_final,
                    D->getDescriptiveKind());
        return;
      }
    }

    if (isa<ClassDecl>(D))
      return;

    // 'final' only makes sense in the context of a class declaration.
    // Reject it on global functions, protocols, structs, enums, etc.
    if (!D->getDeclContext()->getAsClassOrClassExtensionContext()) {
      if (D->getDeclContext()->getAsProtocolExtensionContext())
        TC.diagnose(attr->getLocation(), 
          diag::protocol_extension_cannot_be_final)
          .fixItRemove(attr->getRange());
      else
        TC.diagnose(attr->getLocation(), diag::member_cannot_be_final)
          .fixItRemove(attr->getRange());

      // Remove the attribute so child declarations are not flagged as final
      // and duplicate the error message.
      D->getAttrs().removeAttribute(attr);
      return;
    }
  }

  void visitIndirectAttr(IndirectAttr *attr) {
    if (auto caseDecl = dyn_cast<EnumElementDecl>(D)) {
      // An indirect case should have a payload.
      if (caseDecl->getArgumentTypeLoc().isNull())
        TC.diagnose(attr->getLocation(),
                    diag::indirect_case_without_payload, caseDecl->getName());
      // If the enum is already indirect, its cases don't need to be.
      else if (caseDecl->getParentEnum()->getAttrs()
                 .hasAttribute<IndirectAttr>())
        TC.diagnose(attr->getLocation(),
                    diag::indirect_case_in_indirect_enum);
    }
  }

  void visitWarnUnqualifiedAccessAttr(WarnUnqualifiedAccessAttr *attr) {
    if (!D->getDeclContext()->isTypeContext()) {
      diagnoseAndRemoveAttr(attr, diag::attr_methods_only, attr);
    }
  }

  void visitIBActionAttr(IBActionAttr *attr);
  void visitLazyAttr(LazyAttr *attr);
  void visitIBDesignableAttr(IBDesignableAttr *attr);
  void visitIBInspectableAttr(IBInspectableAttr *attr);
  void visitGKInspectableAttr(GKInspectableAttr *attr);
  void visitIBOutletAttr(IBOutletAttr *attr);
  void visitLLDBDebuggerFunctionAttr(LLDBDebuggerFunctionAttr *attr);
  void visitNSManagedAttr(NSManagedAttr *attr);
  void visitOverrideAttr(OverrideAttr *attr);
  void visitAccessibilityAttr(AccessibilityAttr *attr);
  void visitSetterAccessibilityAttr(SetterAccessibilityAttr *attr);
  bool visitAbstractAccessibilityAttr(AbstractAccessibilityAttr *attr);
  void visitSILStoredAttr(SILStoredAttr *attr);
};
} // end anonymous namespace

void AttributeEarlyChecker::visitTransparentAttr(TransparentAttr *attr) {
  DeclContext *Ctx = D->getDeclContext();
  // Protocol declarations cannot be transparent.
  if (isa<ProtocolDecl>(Ctx))
    return diagnoseAndRemoveAttr(attr,
                                 diag::transparent_in_protocols_not_supported);
  // Class declarations cannot be transparent.
  if (isa<ClassDecl>(Ctx)) {
    
    // @transparent is always ok on implicitly generated accessors: they can
    // be dispatched (even in classes) when the references are within the
    // class themself.
    if (!(isa<FuncDecl>(D) && cast<FuncDecl>(D)->isAccessor() &&
        D->isImplicit()))
      return diagnoseAndRemoveAttr(attr,
                                   diag::transparent_in_classes_not_supported);
  }
  
  if (auto *VD = dyn_cast<VarDecl>(D)) {
    // Stored properties and variables can't be transparent.
    if (VD->hasStorage())
      return diagnoseAndRemoveAttr(attr, diag::transparent_stored_property);
  }
}

void AttributeEarlyChecker::visitMutationAttr(DeclAttribute *attr) {
  FuncDecl *FD = cast<FuncDecl>(D);

  auto contextTy = FD->getDeclContext()->getDeclaredInterfaceType();
  if (!contextTy)
    return diagnoseAndRemoveAttr(attr, diag::mutating_invalid_global_scope);

  if (contextTy->hasReferenceSemantics())
    return diagnoseAndRemoveAttr(attr, diag::mutating_invalid_classes);
  
  // Verify we don't have both mutating and nonmutating.
  if (FD->getAttrs().hasAttribute<MutatingAttr>())
    if (auto *NMA = FD->getAttrs().getAttribute<NonMutatingAttr>()) {
      diagnoseAndRemoveAttr(NMA, diag::functions_mutating_and_not);
      if (NMA == attr) return;
    }
  
  // Verify that we don't have a static function.
  if (FD->isStatic())
    return diagnoseAndRemoveAttr(attr, diag::static_functions_not_mutating);
}

void AttributeEarlyChecker::visitDynamicAttr(DynamicAttr *attr) {
  // Only instance members of classes can be dynamic.
  auto classDecl = D->getDeclContext()->getAsClassOrClassExtensionContext();
  if (!classDecl)
    return diagnoseAndRemoveAttr(attr, diag::dynamic_not_in_class);
    
  // Members cannot be both dynamic and final.
  if (D->getAttrs().hasAttribute<FinalAttr>())
    return diagnoseAndRemoveAttr(attr, diag::dynamic_with_final);
}


void AttributeEarlyChecker::visitIBActionAttr(IBActionAttr *attr) {
  // Only instance methods returning () can be IBActions.
  const FuncDecl *FD = cast<FuncDecl>(D);
  if (!FD->getDeclContext()->getAsClassOrClassExtensionContext() ||
      FD->isStatic() || FD->isAccessor())
    return diagnoseAndRemoveAttr(attr, diag::invalid_ibaction_decl);

}

void AttributeEarlyChecker::visitIBDesignableAttr(IBDesignableAttr *attr) {
  if (auto *ED = dyn_cast<ExtensionDecl>(D)) {
    if (auto extendedType = ED->getExtendedType()) {
      if (auto *nominalDecl = extendedType->getAnyNominal()) {
        if (!isa<ClassDecl>(nominalDecl))
          return diagnoseAndRemoveAttr(attr, diag::invalid_ibdesignable_extension);
      }
    }
  }
}

void AttributeEarlyChecker::visitIBInspectableAttr(IBInspectableAttr *attr) {
  // Only instance properties can be 'IBInspectable'.
  auto *VD = cast<VarDecl>(D);
  if (!VD->getDeclContext()->getAsClassOrClassExtensionContext() ||
      VD->isStatic())
    return diagnoseAndRemoveAttr(attr, diag::invalid_ibinspectable,
                                 attr->getAttrName());
}

void AttributeEarlyChecker::visitGKInspectableAttr(GKInspectableAttr *attr) {
  // Only instance properties can be 'GKInspectable'.
  auto *VD = cast<VarDecl>(D);
  if (!VD->getDeclContext()->getAsClassOrClassExtensionContext() ||
      VD->isStatic())
    return diagnoseAndRemoveAttr(attr, diag::invalid_ibinspectable,
                                 attr->getAttrName());
}

void AttributeEarlyChecker::visitSILStoredAttr(SILStoredAttr *attr) {
  auto *VD = cast<VarDecl>(D);
  if (VD->getDeclContext()->getAsClassOrClassExtensionContext())
    return;
  auto nominalDecl = VD->getDeclContext()
      ->getAsNominalTypeOrNominalTypeExtensionContext();
  if (nominalDecl && isa<StructDecl>(nominalDecl))
    return;
  return diagnoseAndRemoveAttr(attr, diag::invalid_decl_attribute_simple);
}

static Optional<Diag<bool,Type>>
isAcceptableOutletType(Type type, bool &isArray, TypeChecker &TC) {
  if (type->isObjCExistentialType() || type->isAny())
    return None; // @objc existential types are okay

  auto nominal = type->getAnyNominal();

  if (auto classDecl = dyn_cast_or_null<ClassDecl>(nominal)) {
    if (classDecl->isObjC())
      return None; // @objc class types are okay.
    return diag::iboutlet_nonobjc_class;
  }

  if (nominal == TC.Context.getStringDecl()) {
    // String is okay because it is bridged to NSString.
    // FIXME: BridgesTypes.def is almost sufficient for this.
    return None;
  }

  if (nominal == TC.Context.getArrayDecl()) {
    // Arrays of arrays are not allowed.
    if (isArray)
      return diag::iboutlet_nonobject_type;

    isArray = true;

    // Handle Array<T>. T must be an Objective-C class or protocol.
    auto boundTy = type->castTo<BoundGenericStructType>();
    auto boundArgs = boundTy->getGenericArgs();
    assert(boundArgs.size() == 1 && "invalid Array declaration");
    Type elementTy = boundArgs.front();
    return isAcceptableOutletType(elementTy, isArray, TC);
  }

  if (type->isExistentialType())
    return diag::iboutlet_nonobjc_protocol;
  
  // No other types are permitted.
  return diag::iboutlet_nonobject_type;
}


void AttributeEarlyChecker::visitIBOutletAttr(IBOutletAttr *attr) {
  // Only instance properties can be 'IBOutlet'.
  auto *VD = cast<VarDecl>(D);
  if (!VD->getDeclContext()->getAsClassOrClassExtensionContext() ||
      VD->isStatic())
    return diagnoseAndRemoveAttr(attr, diag::invalid_iboutlet);

  if (!VD->isSettable(nullptr))
    return diagnoseAndRemoveAttr(attr, diag::iboutlet_only_mutable);

  // Verify that the field type is valid as an outlet.
  auto type = VD->getType();

  if (VD->isInvalid())
    return;

  // Look through ownership types, and optionals.
  type = type->getReferenceStorageReferent();
  bool wasOptional = false;
  if (Type underlying = type->getAnyOptionalObjectType()) {
    type = underlying;
    wasOptional = true;
  }

  bool isArray = false;
  if (auto isError = isAcceptableOutletType(type, isArray, TC))
    return diagnoseAndRemoveAttr(attr, isError.getValue(),
                                 /*array=*/isArray, type);

  // If the type wasn't optional, an array, or unowned, complain.
  if (!wasOptional && !isArray) {
    auto symbolLoc = Lexer::getLocForEndOfToken(
                       TC.Context.SourceMgr,
                       VD->getTypeSourceRangeForDiagnostics().End);
    TC.diagnose(attr->getLocation(), diag::iboutlet_non_optional,
                type);
    TC.diagnose(symbolLoc, diag::note_make_optional,
                OptionalType::get(type))
      .fixItInsert(symbolLoc, "?");
    TC.diagnose(symbolLoc, diag::note_make_implicitly_unwrapped_optional,
                ImplicitlyUnwrappedOptionalType::get(type))
      .fixItInsert(symbolLoc, "!");
  }
}

void AttributeEarlyChecker::visitNSManagedAttr(NSManagedAttr *attr) {
  // @NSManaged only applies to instance methods and properties within a class.
  if (cast<ValueDecl>(D)->isStatic() ||
      !D->getDeclContext()->getAsClassOrClassExtensionContext()) {
    return diagnoseAndRemoveAttr(attr,
                                 diag::attr_NSManaged_not_instance_member);
  }

  if (auto *method = dyn_cast<FuncDecl>(D)) {
    // Separate out the checks for methods.
    if (method->hasBody())
      return diagnoseAndRemoveAttr(attr, diag::attr_NSManaged_method_body);

    return;
  }

  // Everything below deals with restrictions on @NSManaged properties.
  auto *VD = cast<VarDecl>(D);

  if (VD->isLet())
    return diagnoseAndRemoveAttr(attr, diag::attr_NSManaged_let_property);

  auto diagnoseNotStored = [&](unsigned kind) {
    TC.diagnose(attr->getLocation(), diag::attr_NSManaged_not_stored, kind);
    return attr->setInvalid();
  };

  // @NSManaged properties must be written as stored.
  switch (VD->getStorageKind()) {
  case AbstractStorageDecl::Stored:
    // @NSManaged properties end up being computed; complain if there is
    // an initializer.
    if (VD->getParentInitializer()) {
      TC.diagnose(attr->getLocation(), diag::attr_NSManaged_initial_value)
        .highlight(VD->getParentInitializer()->getSourceRange());
      auto PBD = VD->getParentPatternBinding();
      PBD->setInit(PBD->getPatternEntryIndexForVarDecl(VD), nullptr);
    }
    // Otherwise, ok.
    break;

  case AbstractStorageDecl::StoredWithTrivialAccessors:
    llvm_unreachable("Already created accessors?");

  case AbstractStorageDecl::ComputedWithMutableAddress:
  case AbstractStorageDecl::Computed:
    return diagnoseNotStored(/*computed*/ 0);
  case AbstractStorageDecl::StoredWithObservers:
  case AbstractStorageDecl::InheritedWithObservers:
    return diagnoseNotStored(/*observing*/ 1);
  case AbstractStorageDecl::Addressed:
  case AbstractStorageDecl::AddressedWithTrivialAccessors:
  case AbstractStorageDecl::AddressedWithObservers:
    return diagnoseNotStored(/*addressed*/ 2);
  }

  // @NSManaged properties cannot be @NSCopying
  if (auto *NSCopy = VD->getAttrs().getAttribute<NSCopyingAttr>())
    return diagnoseAndRemoveAttr(NSCopy, diag::attr_NSManaged_NSCopying);

}

void AttributeEarlyChecker::
visitLLDBDebuggerFunctionAttr(LLDBDebuggerFunctionAttr *attr) {
  // This is only legal when debugger support is on.
  if (!D->getASTContext().LangOpts.DebuggerSupport)
    return diagnoseAndRemoveAttr(attr, diag::attr_for_debugger_support_only);
}

void AttributeEarlyChecker::visitOverrideAttr(OverrideAttr *attr) {
  if (!isa<ClassDecl>(D->getDeclContext()) &&
      !isa<ExtensionDecl>(D->getDeclContext()))
    return diagnoseAndRemoveAttr(attr, diag::override_nonclass_decl);
}

void AttributeEarlyChecker::visitLazyAttr(LazyAttr *attr) {
  // lazy may only be used on properties.
  auto *VD = cast<VarDecl>(D);

  // It cannot currently be used on let's since we don't have a mutability model
  // that supports it.
  if (VD->isLet())
    return diagnoseAndRemoveAttr(attr, diag::lazy_not_on_let);

  // lazy is not allowed on a protocol requirement.
  auto varDC = VD->getDeclContext();
  if (isa<ProtocolDecl>(varDC))
    return diagnoseAndRemoveAttr(attr, diag::lazy_not_in_protocol);

  // It only works with stored properties.
  if (!VD->hasStorage())
    return diagnoseAndRemoveAttr(attr, diag::lazy_not_on_computed);

  // lazy is not allowed on a lazily initialized global variable or on a
  // static property (which is already lazily initialized).
  if (VD->isStatic() ||
      (varDC->isModuleScopeContext() &&
       !varDC->getParentSourceFile()->isScriptMode()))
    return diagnoseAndRemoveAttr(attr, diag::lazy_on_already_lazy_global);

  // lazy must have an initializer, and the pattern binding must be a simple
  // one.
  if (!VD->getParentInitializer())
    return diagnoseAndRemoveAttr(attr, diag::lazy_requires_initializer);

  if (!VD->getParentPatternBinding()->getSingleVar())
    return diagnoseAndRemoveAttr(attr, diag::lazy_requires_single_var);

  // TODO: we can't currently support lazy properties on non-type-contexts.
  if (!VD->getDeclContext()->isTypeContext())
    return diagnoseAndRemoveAttr(attr, diag::lazy_must_be_property);

  // TODO: Lazy properties can't yet be observed.
  switch (VD->getStorageKind()) {
  case AbstractStorageDecl::Stored:
  case AbstractStorageDecl::StoredWithTrivialAccessors:
    break;

  case AbstractStorageDecl::StoredWithObservers:
    return diagnoseAndRemoveAttr(attr, diag::lazy_not_observable);

  case AbstractStorageDecl::InheritedWithObservers:
  case AbstractStorageDecl::ComputedWithMutableAddress:
  case AbstractStorageDecl::Computed:
  case AbstractStorageDecl::Addressed:
  case AbstractStorageDecl::AddressedWithTrivialAccessors:
  case AbstractStorageDecl::AddressedWithObservers:
    llvm_unreachable("non-stored variable not filtered out?");
  }
}

bool AttributeEarlyChecker::visitAbstractAccessibilityAttr(
    AbstractAccessibilityAttr *attr) {
  // Accessibility attr may only be used on value decls and extensions.
  if (!isa<ValueDecl>(D) && !isa<ExtensionDecl>(D)) {
    diagnoseAndRemoveAttr(attr, diag::invalid_decl_modifier, attr);
    return true;
  }

  if (auto extension = dyn_cast<ExtensionDecl>(D)) {
    if (!extension->getInherited().empty()) {
      diagnoseAndRemoveAttr(attr, diag::extension_access_with_conformances,
                            attr);
      return true;
    }
  }

  // And not on certain value decls.
  if (isa<DestructorDecl>(D) || isa<EnumElementDecl>(D)) {
    diagnoseAndRemoveAttr(attr, diag::invalid_decl_modifier, attr);
    return true;
  }

  // Or within protocols.
  if (isa<ProtocolDecl>(D->getDeclContext())) {
    diagnoseAndRemoveAttr(attr, diag::access_control_in_protocol, attr);
    return true;
  }

  return false;
}

void AttributeEarlyChecker::visitAccessibilityAttr(AccessibilityAttr *attr) {
  visitAbstractAccessibilityAttr(attr);
}

void AttributeEarlyChecker::visitSetterAccessibilityAttr(
    SetterAccessibilityAttr *attr) {
  auto storage = dyn_cast<AbstractStorageDecl>(D);
  if (!storage)
    return diagnoseAndRemoveAttr(attr, diag::access_control_setter,
                                 attr->getAccess());

  if (visitAbstractAccessibilityAttr(attr))
    return;

  if (!storage->isSettable(storage->getDeclContext())) {
    // This must stay in sync with diag::access_control_setter_read_only.
    enum {
      SK_Constant = 0,
      SK_Variable,
      SK_Property,
      SK_Subscript
    } storageKind;
    if (isa<SubscriptDecl>(storage))
      storageKind = SK_Subscript;
    else if (storage->getDeclContext()->isTypeContext())
      storageKind = SK_Property;
    else if (cast<VarDecl>(storage)->isLet())
      storageKind = SK_Constant;
    else
      storageKind = SK_Variable;
    return diagnoseAndRemoveAttr(attr, diag::access_control_setter_read_only,
                                 attr->getAccess(), storageKind);
  }
}


void TypeChecker::checkDeclAttributesEarly(Decl *D) {
  // Don't perform early attribute validation more than once.
  // FIXME: Crummy way to get idempotency.
  if (D->didEarlyAttrValidation())
    return;

  D->setEarlyAttrValidation();

  AttributeEarlyChecker Checker(*this, D);
  for (auto attr : D->getAttrs()) {
    if (!attr->isValid()) continue;

    // If Attr.def says that the attribute cannot appear on this kind of
    // declaration, diagnose it and disable it.
    if (attr->canAppearOnDecl(D)) {
      // Otherwise, check it.
      Checker.visit(attr);
      continue;
    }

    // Otherwise, this attribute cannot be applied to this declaration.  If the
    // attribute is only valid on one kind of declaration (which is pretty
    // common) give a specific helpful error.
    unsigned PossibleDeclKinds = attr->getOptions() & DeclAttribute::OnAnyDecl;
    StringRef OnlyKind;
    switch (PossibleDeclKinds) {
    case DeclAttribute::OnImport:
      OnlyKind = "import";
      break;
    case DeclAttribute::OnVar:
      OnlyKind = "var";
      break;
    case DeclAttribute::OnFunc:
      OnlyKind = "func";
      break;
    case DeclAttribute::OnClass:
      OnlyKind = "class";
      break;
    case DeclAttribute::OnStruct:
      OnlyKind = "struct";
      break;
    case DeclAttribute::OnConstructor:
      OnlyKind = "init";
      break;
    case DeclAttribute::OnProtocol:
      OnlyKind = "protocol";
      break;
    case DeclAttribute::OnParam:
      OnlyKind = "parameter";
      break;
    default:
      break;
    }

    if (!OnlyKind.empty())
      Checker.diagnoseAndRemoveAttr(attr, diag::attr_only_one_decl_kind,
                                    attr, OnlyKind);
    else if (attr->isDeclModifier())
      Checker.diagnoseAndRemoveAttr(attr, diag::invalid_decl_modifier, attr);
    else
      Checker.diagnoseAndRemoveAttr(attr, diag::invalid_decl_attribute, attr);
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

#define IGNORED_ATTR(CLASS)                                              \
    void visit##CLASS##Attr(CLASS##Attr *) {}

    IGNORED_ATTR(AutoClosure)
    IGNORED_ATTR(Alignment)
    IGNORED_ATTR(Convenience)
    IGNORED_ATTR(Dynamic)
    IGNORED_ATTR(Effects)
    IGNORED_ATTR(Exported)
    IGNORED_ATTR(GKInspectable)
    IGNORED_ATTR(IBDesignable)
    IGNORED_ATTR(IBInspectable)
    IGNORED_ATTR(IBOutlet) // checked early.
    IGNORED_ATTR(Indirect)
    IGNORED_ATTR(Inline)
    IGNORED_ATTR(Lazy)      // checked early.
    IGNORED_ATTR(LLDBDebuggerFunction)
    IGNORED_ATTR(Mutating)
    IGNORED_ATTR(NoEscape)
    IGNORED_ATTR(NonMutating)
    IGNORED_ATTR(NonObjC)
    IGNORED_ATTR(NoReturn)
    IGNORED_ATTR(NSManaged) // checked early.
    IGNORED_ATTR(ObjC)
    IGNORED_ATTR(ObjCBridged)
    IGNORED_ATTR(ObjCNonLazyRealization)
    IGNORED_ATTR(Optional)
    IGNORED_ATTR(Ownership)
    IGNORED_ATTR(Override)
    IGNORED_ATTR(RawDocComment)
    IGNORED_ATTR(Semantics)
    IGNORED_ATTR(SILGenName)
    IGNORED_ATTR(Transparent)
    IGNORED_ATTR(SynthesizedProtocol)
    IGNORED_ATTR(RequiresStoredPropertyInits)
    IGNORED_ATTR(SILStored)
    IGNORED_ATTR(Testable)
    IGNORED_ATTR(WarnUnqualifiedAccess)
    IGNORED_ATTR(ShowInInterface)
#undef IGNORED_ATTR

  void visitAvailableAttr(AvailableAttr *attr);
  
  void visitCDeclAttr(CDeclAttr *attr);

  void visitFinalAttr(FinalAttr *attr);
  void visitIBActionAttr(IBActionAttr *attr);
  void visitNSCopyingAttr(NSCopyingAttr *attr);
  void visitRequiredAttr(RequiredAttr *attr);
  void visitRethrowsAttr(RethrowsAttr *attr);

  void visitAccessibilityAttr(AccessibilityAttr *attr);
  void visitSetterAccessibilityAttr(SetterAccessibilityAttr *attr);

  void checkApplicationMainAttribute(DeclAttribute *attr,
                                     Identifier Id_ApplicationDelegate,
                                     Identifier Id_Kit,
                                     Identifier Id_ApplicationMain);
  
  void visitNSApplicationMainAttr(NSApplicationMainAttr *attr);
  void visitUIApplicationMainAttr(UIApplicationMainAttr *attr);

  void visitUnsafeNoObjCTaggedPointerAttr(UnsafeNoObjCTaggedPointerAttr *attr);
  void visitSwiftNativeObjCRuntimeBaseAttr(
                                         SwiftNativeObjCRuntimeBaseAttr *attr);

  void checkOperatorAttribute(DeclAttribute *attr);

  void visitInfixAttr(InfixAttr *attr) { checkOperatorAttribute(attr); }
  void visitPostfixAttr(PostfixAttr *attr) { checkOperatorAttribute(attr); }
  void visitPrefixAttr(PrefixAttr *attr) { checkOperatorAttribute(attr); }

  void visitSpecializeAttr(SpecializeAttr *attr);

  void visitFixedLayoutAttr(FixedLayoutAttr *attr);
  void visitVersionedAttr(VersionedAttr *attr);
  void visitInlineableAttr(InlineableAttr *attr);
  
  void visitDiscardableResultAttr(DiscardableResultAttr *attr);
};
} // end anonymous namespace


static bool checkObjectOrOptionalObjectType(TypeChecker &TC, Decl *D,
                                            ParamDecl *param) {
  Type ty = param->getType();
  if (auto unwrapped = ty->getAnyOptionalObjectType())
    ty = unwrapped;

  if (auto classDecl = ty->getClassOrBoundGenericClass()) {
    // @objc class types are okay.
    if (!classDecl->isObjC()) {
      TC.diagnose(D, diag::ibaction_nonobjc_class_argument,
                  param->getType())
        .highlight(param->getSourceRange());
      return true;
    }
  } else if (ty->isObjCExistentialType() || ty->isAny()) {
    // @objc existential types are okay, as is Any.
    // Nothing to do.
  } else {
    // No other types are permitted.
    TC.diagnose(D, diag::ibaction_nonobject_argument,
                param->getType())
      .highlight(param->getSourceRange());
    return true;
  }

  return false;
}

static bool isiOS(TypeChecker &TC) {
  return TC.getLangOpts().Target.isiOS();
}

static bool iswatchOS(TypeChecker &TC) {
  return TC.getLangOpts().Target.isWatchOS();
}

static bool isRelaxedIBAction(TypeChecker &TC) {
  return isiOS(TC) || iswatchOS(TC);
}

void AttributeChecker::visitIBActionAttr(IBActionAttr *attr) {
  // IBActions instance methods must have type Class -> (...) -> ().
  auto *FD = cast<FuncDecl>(D);
  Type CurriedTy = FD->getInterfaceType()->castTo<AnyFunctionType>()->getResult();
  Type ResultTy = CurriedTy->castTo<AnyFunctionType>()->getResult();
  if (!ResultTy->isEqual(TupleType::getEmpty(TC.Context))) {
    TC.diagnose(D, diag::invalid_ibaction_result, ResultTy);
    attr->setInvalid();
    return;
  }

  auto paramList = FD->getParameterList(1);
  bool relaxedIBActionUsedOnOSX = false;
  bool Valid = true;
  switch (paramList->size()) {
  case 0:
    // (iOS only) No arguments.
    if (!isRelaxedIBAction(TC)) {
      relaxedIBActionUsedOnOSX = true;
      break;
    }
    break;
  case 1:
    // One argument. May be a scalar on iOS/watchOS (because of WatchKit).
    if (isRelaxedIBAction(TC)) {
      // Do a rough check to allow any ObjC-representable struct or enum type
      // on iOS.
      Type ty = paramList->get(0)->getType();
      if (auto nominal = ty->getAnyNominal())
        if (isa<StructDecl>(nominal) || isa<EnumDecl>(nominal))
          if (nominal->classifyAsOptionalType() == OTK_None)
            if (ty->isTriviallyRepresentableIn(ForeignLanguage::ObjectiveC,
                                               cast<FuncDecl>(D)))
              break;  // Looks ok.
    }
    if (checkObjectOrOptionalObjectType(TC, D, paramList->get(0)))
      Valid = false;
    break;
  case 2:
    // (iOS/watchOS only) Two arguments, the second of which is a UIEvent.
    // We don't currently enforce the UIEvent part.
    if (!isRelaxedIBAction(TC)) {
      relaxedIBActionUsedOnOSX = true;
      break;
    }
    if (checkObjectOrOptionalObjectType(TC, D, paramList->get(0)))
      Valid = false;
    if (checkObjectOrOptionalObjectType(TC, D, paramList->get(1)))
      Valid = false;
    break;
  default:
    // No platform allows an action signature with more than two arguments.
    TC.diagnose(D, diag::invalid_ibaction_argument_count,
                isRelaxedIBAction(TC));
    Valid = false;
    break;
  }

  if (relaxedIBActionUsedOnOSX) {
    TC.diagnose(D, diag::invalid_ibaction_argument_count,
                /*relaxedIBAction=*/false);
    Valid = false;
  }

  if (!Valid)
    attr->setInvalid();
}

/// Get the innermost enclosing declaration for a declaration.
static Decl *getEnclosingDeclForDecl(Decl *D) {
  // If the declaration is an accessor, treat its storage declaration
  // as the enclosing declaration.
  if (auto *FD = dyn_cast<FuncDecl>(D)) {
    if (FD->isAccessor()) {
      return FD->getAccessorStorageDecl();
    }
  }

  return D->getDeclContext()->getInnermostDeclarationDeclContext();
}

void AttributeChecker::visitAvailableAttr(AvailableAttr *attr) {
  if (TC.getLangOpts().DisableAvailabilityChecking)
    return;

  if (!attr->isActivePlatform(TC.Context) || !attr->Introduced.hasValue())
    return;

  SourceLoc attrLoc = attr->getLocation();

  Optional<Diag<>> MaybeNotAllowed =
      TC.diagnosticIfDeclCannotBePotentiallyUnavailable(D);
  if (MaybeNotAllowed.hasValue()) {
    TC.diagnose(attrLoc, MaybeNotAllowed.getValue());
  }

  // Find the innermost enclosing declaration with an availability
  // range annotation and ensure that this attribute's available version range
  // is fully contained within that declaration's range. If there is no such
  // enclosing declaration, then there is nothing to check.
  Optional<AvailabilityContext> EnclosingAnnotatedRange;
  Decl *EnclosingDecl = getEnclosingDeclForDecl(D);

  while (EnclosingDecl) {
    EnclosingAnnotatedRange =
        AvailabilityInference::annotatedAvailableRange(EnclosingDecl,
                                                       TC.Context);

    if (EnclosingAnnotatedRange.hasValue())
      break;

    EnclosingDecl = getEnclosingDeclForDecl(EnclosingDecl);
  }

  if (!EnclosingDecl)
    return;

  AvailabilityContext AttrRange{
      VersionRange::allGTE(attr->Introduced.getValue())};

  if (!AttrRange.isContainedIn(EnclosingAnnotatedRange.getValue())) {
    TC.diagnose(attr->getLocation(),
                diag::availability_decl_more_than_enclosing);
    TC.diagnose(EnclosingDecl->getLoc(),
                diag::availability_decl_more_than_enclosing_enclosing_here);
  }
}

void AttributeChecker::visitCDeclAttr(CDeclAttr *attr) {
  // Only top-level func decls are currently supported.
  if (D->getDeclContext()->isTypeContext())
    TC.diagnose(attr->getLocation(),
                diag::cdecl_not_at_top_level);
  
  // The name must not be empty.
  if (attr->Name.empty())
    TC.diagnose(attr->getLocation(),
                diag::cdecl_empty_name);
}

void AttributeChecker::visitUnsafeNoObjCTaggedPointerAttr(
                                          UnsafeNoObjCTaggedPointerAttr *attr) {
  // Only class protocols can have the attribute.
  auto proto = dyn_cast<ProtocolDecl>(D);
  if (!proto) {
    TC.diagnose(attr->getLocation(),
                diag::no_objc_tagged_pointer_not_class_protocol);
    attr->setInvalid();
  }
  
  if (!proto->requiresClass()
      && !proto->getAttrs().hasAttribute<ObjCAttr>()) {
    TC.diagnose(attr->getLocation(),
                diag::no_objc_tagged_pointer_not_class_protocol);
    attr->setInvalid();    
  }
}

void AttributeChecker::visitSwiftNativeObjCRuntimeBaseAttr(
                                         SwiftNativeObjCRuntimeBaseAttr *attr) {
  // Only root classes can have the attribute.
  auto theClass = dyn_cast<ClassDecl>(D);
  if (!theClass) {
    TC.diagnose(attr->getLocation(),
                diag::swift_native_objc_runtime_base_not_on_root_class);
    attr->setInvalid();
    return;
  }
  
  if (theClass->hasSuperclass()) {
    TC.diagnose(attr->getLocation(),
                diag::swift_native_objc_runtime_base_not_on_root_class);
    attr->setInvalid();
    return;
  }
}

void AttributeChecker::visitFinalAttr(FinalAttr *attr) {
  // final on classes marks all members with final.
  if (isa<ClassDecl>(D))
    return;

  // We currently only support final on var/let, func and subscript
  // declarations.
  if (!isa<VarDecl>(D) && !isa<FuncDecl>(D) && !isa<SubscriptDecl>(D)) {
    TC.diagnose(attr->getLocation(), diag::final_not_allowed_here)
      .fixItRemove(attr->getRange());
    return;
  }

  if (auto *FD = dyn_cast<FuncDecl>(D)) {
    if (FD->isAccessor() && !attr->isImplicit()) {
      unsigned Kind = 2;
      if (auto *VD = dyn_cast<VarDecl>(FD->getAccessorStorageDecl()))
        Kind = VD->isLet() ? 1 : 0;
      TC.diagnose(attr->getLocation(), diag::final_not_on_accessors, Kind)
        .fixItRemove(attr->getRange());
      return;
    }
  }
}

/// Return true if this is a builtin operator that cannot be defined in user
/// code.
static bool isBuiltinOperator(StringRef name, DeclAttribute *attr) {
  return ((isa<PrefixAttr>(attr)  && name == "&") ||   // lvalue to inout
          (isa<PostfixAttr>(attr) && name == "!") ||   // optional unwrapping
          (isa<PostfixAttr>(attr) && name == "?") ||   // optional chaining
          (isa<InfixAttr>(attr) && name == "?") ||     // ternary operator
          (isa<PostfixAttr>(attr) && name == ">") ||   // generic argument list
          (isa<PrefixAttr>(attr)  && name == "<"));    // generic argument list
}

void AttributeChecker::checkOperatorAttribute(DeclAttribute *attr) {
  // Check out the operator attributes.  They may be attached to an operator
  // declaration or a function.
  if (auto *OD = dyn_cast<OperatorDecl>(D)) {
    // Reject attempts to define builtin operators.
    if (isBuiltinOperator(OD->getName().str(), attr)) {
      TC.diagnose(D->getStartLoc(), diag::redefining_builtin_operator,
                  attr->getAttrName(), OD->getName().str());
      attr->setInvalid();
      return;
    }

    // Otherwise, the attribute is always ok on an operator.
    return;
  }

  // Operators implementations may only be defined as functions.
  auto *FD = dyn_cast<FuncDecl>(D);
  if (!FD) {
    TC.diagnose(D->getLoc(), diag::operator_not_func);
    attr->setInvalid();
    return;
  }

  // Only functions with an operator identifier can be declared with as an
  // operator.
  if (!FD->isOperator()) {
    TC.diagnose(D->getStartLoc(), diag::attribute_requires_operator_identifier,
                attr->getAttrName());
    attr->setInvalid();
    return;
  }

  // Reject attempts to define builtin operators.
  if (isBuiltinOperator(FD->getName().str(), attr)) {
    TC.diagnose(D->getStartLoc(), diag::redefining_builtin_operator,
                attr->getAttrName(), FD->getName().str());
    attr->setInvalid();
    return;
  }

  // Otherwise, must be unary.
  if (!FD->isUnaryOperator()) {
    TC.diagnose(attr->getLocation(), diag::attribute_requires_single_argument,
                attr->getAttrName());
    attr->setInvalid();
    return;
  }
}

void AttributeChecker::visitNSCopyingAttr(NSCopyingAttr *attr) {
  // The @NSCopying attribute is only allowed on stored properties.
  auto *VD = cast<VarDecl>(D);

  // It may only be used on class members.
  auto classDecl = D->getDeclContext()->getAsClassOrClassExtensionContext();
  if (!classDecl) {
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

void AttributeChecker::checkApplicationMainAttribute(DeclAttribute *attr,
                                             Identifier Id_ApplicationDelegate,
                                             Identifier Id_Kit,
                                             Identifier Id_ApplicationMain) {
  // %select indexes for ApplicationMain diagnostics.
  enum : unsigned {
    UIApplicationMainClass,
    NSApplicationMainClass,
  };
  
  unsigned applicationMainKind;
  if (isa<UIApplicationMainAttr>(attr))
    applicationMainKind = UIApplicationMainClass;
  else if (isa<NSApplicationMainAttr>(attr))
    applicationMainKind = NSApplicationMainClass;
  else
    llvm_unreachable("not an ApplicationMain attr");
  
  auto *CD = dyn_cast<ClassDecl>(D);
  
  // The applicant not being a class should have been diagnosed by the early
  // checker.
  if (!CD) return;

  // The class cannot be generic.
  if (CD->isGenericContext()) {
    TC.diagnose(attr->getLocation(),
                diag::attr_generic_ApplicationMain_not_supported,
                applicationMainKind);
    attr->setInvalid();
    return;
  }
  
  // @XXApplicationMain classes must conform to the XXApplicationDelegate
  // protocol.
  auto &C = D->getASTContext();

  auto KitModule = C.getLoadedModule(Id_Kit);
  ProtocolDecl *ApplicationDelegateProto = nullptr;
  if (KitModule) {
    auto lookupOptions = defaultUnqualifiedLookupOptions;
    lookupOptions |= NameLookupFlags::KnownPrivate;

    auto lookup = TC.lookupUnqualifiedType(KitModule, Id_ApplicationDelegate,
                                           SourceLoc(),
                                           lookupOptions);
    if (lookup.size() == 1)
      ApplicationDelegateProto = dyn_cast<ProtocolDecl>(lookup[0]);
  }

  if (!ApplicationDelegateProto ||
      !TC.conformsToProtocol(CD->getDeclaredType(), ApplicationDelegateProto,
                             CD, None)) {
    TC.diagnose(attr->getLocation(),
                diag::attr_ApplicationMain_not_ApplicationDelegate,
                applicationMainKind);
    attr->setInvalid();
  }

  if (attr->isInvalid())
    return;
  
  // Register the class as the main class in the module. If there are multiples
  // they will be diagnosed.
  auto *SF = cast<SourceFile>(CD->getModuleScopeContext());
  if (SF->registerMainClass(CD, attr->getLocation()))
    attr->setInvalid();
  
  // Check that we have the needed symbols in the frameworks.
  auto lookupOptions = defaultUnqualifiedLookupOptions;
  lookupOptions |= NameLookupFlags::KnownPrivate;
  auto lookupMain = TC.lookupUnqualified(KitModule, Id_ApplicationMain,
                                         SourceLoc(), lookupOptions);

  for (const auto &result : lookupMain) {
    TC.validateDecl(result.Decl);
  }
  auto Foundation = TC.Context.getLoadedModule(C.Id_Foundation);
  if (Foundation) {
    auto lookupString = TC.lookupUnqualified(
                          Foundation,
                          C.getIdentifier("NSStringFromClass"),
                          SourceLoc(),
                          lookupOptions);
    for (const auto &result : lookupString) {
      TC.validateDecl(result.Decl);
    }
  }
}

void AttributeChecker::visitNSApplicationMainAttr(NSApplicationMainAttr *attr) {
  auto &C = D->getASTContext();
  checkApplicationMainAttribute(attr,
                                C.getIdentifier("NSApplicationDelegate"),
                                C.getIdentifier("AppKit"),
                                C.getIdentifier("NSApplicationMain"));
}
void AttributeChecker::visitUIApplicationMainAttr(UIApplicationMainAttr *attr) {
  auto &C = D->getASTContext();
  checkApplicationMainAttribute(attr,
                                C.getIdentifier("UIApplicationDelegate"),
                                C.getIdentifier("UIKit"),
                                C.getIdentifier("UIApplicationMain"));
}

/// Determine whether the given context is an extension to an Objective-C class
/// where the class is defined in the Objective-C module and the extension is
/// defined within its module.
static bool isObjCClassExtensionInOverlay(DeclContext *dc) {
  // Check whether we have an extension.
  auto ext = dyn_cast<ExtensionDecl>(dc);
  if (!ext)
    return false;

  // Find the extended class.
  auto classDecl = ext->getExtendedType()->getClassOrBoundGenericClass();
  if (!classDecl)
    return false;

  // The class must be defined in Objective-C.
  if (!classDecl->hasClangNode())
    return false;

  // Find the Clang module unit that stores the class.
  auto classModuleUnit
    = dyn_cast<ClangModuleUnit>(classDecl->getModuleScopeContext());
  if (!classModuleUnit)
    return false;

  // Check whether the extension is in the overlay.
  auto extModule = ext->getDeclContext()->getParentModule();
  return extModule == classModuleUnit->getAdapterModule();
}

void AttributeChecker::visitRequiredAttr(RequiredAttr *attr) {
  // The required attribute only applies to constructors.
  auto ctor = cast<ConstructorDecl>(D);
  auto parentTy = ctor->getDeclContext()->getDeclaredInterfaceType();
  if (!parentTy) {
    // Constructor outside of nominal type context; we've already complained
    // elsewhere.
    attr->setInvalid();
    return;
  }
  // Only classes can have required constructors.
  if (parentTy->getClassOrBoundGenericClass()) {
    // The constructor must be declared within the class itself.
    // FIXME: Allow an SDK overlay to add a required initializer to a class
    // defined in Objective-C
    if (!isa<ClassDecl>(ctor->getDeclContext()) &&
        !isObjCClassExtensionInOverlay(ctor->getDeclContext())) {
      TC.diagnose(ctor, diag::required_initializer_in_extension, parentTy)
        .highlight(attr->getLocation());
      attr->setInvalid();
      return;
    }
  } else {
    if (!parentTy->hasError()) {
      TC.diagnose(ctor, diag::required_initializer_nonclass, parentTy)
        .highlight(attr->getLocation());
    }
    attr->setInvalid();
    return;
  }
}

static bool hasThrowingFunctionParameter(CanType type) {
  // Only consider throwing function types.
  if (auto fnType = dyn_cast<AnyFunctionType>(type)) {
    return fnType->getExtInfo().throws();
  }

  // Look through tuples.
  if (auto tuple = dyn_cast<TupleType>(type)) {
    for (auto eltType : tuple.getElementTypes()) {
      if (hasThrowingFunctionParameter(eltType))
        return true;
    }
    return false;
  }

  // Suppress diagnostics in the presence of errors.
  if (type->hasError()) {
    return true;
  }

  return false;
}

void AttributeChecker::visitRethrowsAttr(RethrowsAttr *attr) {
  // 'rethrows' only applies to functions that take throwing functions
  // as parameters.
  auto fn = cast<AbstractFunctionDecl>(D);
  for (auto paramList : fn->getParameterLists()) {
    for (auto param : *paramList)
      if (hasThrowingFunctionParameter(param->getType()
              ->lookThroughAllAnyOptionalTypes()
              ->getCanonicalType()))
        return;
  }

  TC.diagnose(attr->getLocation(), diag::rethrows_without_throwing_parameter);
  attr->setInvalid();
}

void AttributeChecker::visitAccessibilityAttr(AccessibilityAttr *attr) {
  if (auto extension = dyn_cast<ExtensionDecl>(D)) {
    if (attr->getAccess() == Accessibility::Open) {
      TC.diagnose(attr->getLocation(), diag::access_control_extension_open)
        .fixItReplace(attr->getRange(), "public");
      attr->setInvalid();
      return;
    }

    Type extendedTy = extension->getExtendedType();
    Accessibility typeAccess = extendedTy->getAnyNominal()->getFormalAccess();
    if (attr->getAccess() > typeAccess) {
      TC.diagnose(attr->getLocation(), diag::access_control_extension_more,
                  typeAccess,
                  extendedTy->getAnyNominal()->getDescriptiveKind(),
                  attr->getAccess())
        .fixItRemove(attr->getRange());
      attr->setInvalid();
      return;
    }

  } else if (auto extension = dyn_cast<ExtensionDecl>(D->getDeclContext())) {
    TC.computeDefaultAccessibility(extension);
    Accessibility maxAccess = extension->getMaxAccessibility();
    if (std::min(attr->getAccess(), Accessibility::Public) > maxAccess) {
      // FIXME: It would be nice to say what part of the requirements actually
      // end up being problematic.
      auto diag =
          TC.diagnose(attr->getLocation(),
                      diag::access_control_ext_requirement_member_more,
                      attr->getAccess(),
                      D->getDescriptiveKind(),
                      maxAccess);
      swift::fixItAccessibility(diag, cast<ValueDecl>(D), maxAccess);
      return;
    }

    auto extAttr = extension->getAttrs().getAttribute<AccessibilityAttr>();
    if (extAttr && attr->getAccess() > extAttr->getAccess()) {
      auto diag = TC.diagnose(attr->getLocation(),
                              diag::access_control_ext_member_more,
                              attr->getAccess(),
                              D->getDescriptiveKind(),
                              extAttr->getAccess());
      swift::fixItAccessibility(diag, cast<ValueDecl>(D), extAttr->getAccess());
      return;
    }
  }

  if (attr->getAccess() == Accessibility::Open) {
    if (!isa<ClassDecl>(D) && !D->isPotentiallyOverridable() &&
        !attr->isInvalid()) {
      TC.diagnose(attr->getLocation(), diag::access_control_open_bad_decl)
        .fixItReplace(attr->getRange(), "public");
      attr->setInvalid();
    }
  }
}

void
AttributeChecker::visitSetterAccessibilityAttr(SetterAccessibilityAttr *attr) {
  auto getterAccess = cast<ValueDecl>(D)->getFormalAccess();
  if (attr->getAccess() > getterAccess) {
    // This must stay in sync with diag::access_control_setter_more.
    enum {
      SK_Variable = 0,
      SK_Property,
      SK_Subscript
    } storageKind;
    if (isa<SubscriptDecl>(D))
      storageKind = SK_Subscript;
    else if (D->getDeclContext()->isTypeContext())
      storageKind = SK_Property;
    else
      storageKind = SK_Variable;
    TC.diagnose(attr->getLocation(), diag::access_control_setter_more,
                getterAccess, storageKind, attr->getAccess());
    attr->setInvalid();
    return;
  }
}

/// Collect all used generic parameter types from a given type.
static void collectUsedGenericParameters(
    Type Ty, SmallPtrSet<TypeBase *, 4> &ConstrainedGenericParams) {
  if (!Ty)
    return;

  if (!Ty->hasTypeParameter())
    return;

  // Add used generic parameters/archetypes.
  Ty.visit([&](Type Ty) {
    if (auto GP = dyn_cast<GenericTypeParamType>(Ty->getCanonicalType())) {
      ConstrainedGenericParams.insert(GP);
    }
  });
}

/// Perform some sanity checks for the requirements provided by
/// the @_specialize attribute.
static void checkSpecializeAttrRequirements(
    SpecializeAttr *attr,
    AbstractFunctionDecl *FD,
    ArrayRef<RequirementRepr> requirements,
    SmallPtrSet<TypeBase *, 4> constrainedGenericParams,
    TypeChecker &TC) {
  auto genericSig = FD->getGenericSignature();
  bool isInvalidAttr = false;

  // Check that requirements are defined only on
  // generic parameter types and not on their associated or dependent types or
  // other generic types using these parameters.
  for (auto &req : requirements) {
    if (req.getKind() == RequirementReprKind::SameType) {
      auto firstType = req.getFirstType();
      auto secondType = req.getSecondType();

      if (!firstType || firstType->is<ErrorType>() ||
          firstType->hasArchetype()) {
        isInvalidAttr = true;
        continue;
      }

      if (!secondType || secondType->is<ErrorType>() ||
          secondType->hasArchetype()) {
        isInvalidAttr = true;
        continue;
      }

      collectUsedGenericParameters(firstType, constrainedGenericParams);
      collectUsedGenericParameters(secondType, constrainedGenericParams);

      // One of the types should be concrete and the other one should not.
      bool isFirstTypeNonConcrete = firstType->hasTypeParameter();
      bool isSecondTypeNonConcrete = secondType->hasTypeParameter();

      if (isFirstTypeNonConcrete && isSecondTypeNonConcrete) {
        TC.diagnose(attr->getLocation(),
                    diag::specialize_attr_non_concrete_same_type_req)
            .highlight(req.getSourceRange());
        continue;
      }
      if (!(isFirstTypeNonConcrete ^ isSecondTypeNonConcrete)) {
        TC.diagnose(attr->getLocation(),
                    diag::specialize_attr_only_one_concrete_same_type_req)
            .highlight(req.getSourceRange());
        continue;
      }
      if (isFirstTypeNonConcrete) {
        if (!isa<GenericTypeParamType>(firstType->getCanonicalType())) {
          TC.diagnose(attr->getLocation(),
                      diag::specialize_attr_only_generic_param_req)
              .highlight(req.getFirstTypeRepr()->getSourceRange());
        }
      }
      if (isSecondTypeNonConcrete) {
        if (!isa<GenericTypeParamType>(secondType->getCanonicalType())) {
          TC.diagnose(attr->getLocation(),
                      diag::specialize_attr_only_generic_param_req)
              .highlight(req.getSecondTypeRepr()->getSourceRange());
        }
      }
      continue;
    }

    if (req.getKind() == RequirementReprKind::LayoutConstraint ||
        req.getKind() == RequirementReprKind::TypeConstraint) {
      auto subjectType = req.getSubject();

      // Skip any unknown or error types.
      if (!subjectType || subjectType->is<ErrorType>() ||
          subjectType->hasArchetype()) {
        isInvalidAttr = true;
        continue;
      }

      if (req.getKind() == RequirementReprKind::TypeConstraint) {
        auto constraint = req.getConstraint();

        if (!constraint || constraint->hasError() ||
            constraint->hasArchetype()) {
          isInvalidAttr = true;
          continue;
        }

        auto nominalTy = constraint->getNominalOrBoundGenericNominal();
        if (!nominalTy) {
          TC.diagnose(attr->getLocation(),
                      diag::specialize_attr_non_nominal_type_constraint_req)
              .highlight(req.getSourceRange());
          continue;
        }

        auto proto = dyn_cast<ProtocolDecl>(nominalTy);
        if (!proto) {
          TC.diagnose(attr->getLocation(),
                      diag::specialize_attr_non_protocol_type_constraint_req)
              .highlight(req.getSourceRange());
        }
      }

      bool isSubjectNonConcrete = subjectType->hasTypeParameter();

      if (isSubjectNonConcrete) {
        collectUsedGenericParameters(subjectType, constrainedGenericParams);
        if (!isa<GenericTypeParamType>(subjectType->getCanonicalType())) {
          TC.diagnose(attr->getLocation(),
                      diag::specialize_attr_only_generic_param_req)
              .highlight(req.getSubjectRepr()->getSourceRange());
        }
      }

      if (req.getKind() == RequirementReprKind::TypeConstraint) {
        TC.diagnose(attr->getLocation(),
                    diag::specialize_attr_unsupported_kind_of_req)
            .highlight(req.getSourceRange());
      }
      continue;
    }

    TC.diagnose(attr->getLocation(),
                diag::specialize_attr_unsupported_kind_of_req)
        .highlight(req.getSourceRange());
  }

  if (isInvalidAttr) {
    attr->setInvalid();
  }

  if (!attr->isFullSpecialization())
    return;

  if (constrainedGenericParams.size() == genericSig->getGenericParams().size())
    return;

  TC.diagnose(
      attr->getLocation(), diag::specialize_attr_type_parameter_count_mismatch,
      genericSig->getGenericParams().size(), constrainedGenericParams.size(),
      constrainedGenericParams.size() < genericSig->getGenericParams().size());

  if (constrainedGenericParams.size() < genericSig->getGenericParams().size()) {
    // Figure out which archetypes are not constrained.
    for (auto gp : genericSig->getGenericParams()) {
      if (constrainedGenericParams.count(gp->getCanonicalType().getPointer()))
        continue;
      auto gpDecl = gp->getDecl();
      if (gpDecl) {
        TC.diagnose(attr->getLocation(),
                    diag::specialize_attr_missing_constraint,
                    gpDecl->getFullName());
      }
    }
  }
}

/// Type check that a set of requirements provided by @_specialize.
/// Store the set of requirements in the attribute.
void AttributeChecker::visitSpecializeAttr(SpecializeAttr *attr) {
  DeclContext *DC = D->getDeclContext();
  auto *FD = cast<AbstractFunctionDecl>(D);
  auto *genericSig = FD->getGenericSignature();
  auto *genericEnv = FD->getGenericEnvironment();
  auto *trailingWhereClause = attr->getTrailingWhereClause();

  if (!trailingWhereClause) {
    // Report a missing "where" clause.
    TC.diagnose(attr->getLocation(), diag::specialize_missing_where_clause);
    return;
  }

  if (trailingWhereClause->getRequirements().empty()) {
    // Report an empty "where" clause.
    TC.diagnose(attr->getLocation(), diag::specialize_empty_where_clause);
    return;
  }

  if (!genericSig) {
    // Only generic functions are permitted to have trailing where clauses.
    TC.diagnose(attr->getLocation(),
                diag::specialize_attr_nongeneric_trailing_where, FD->getName())
        .highlight(trailingWhereClause->getSourceRange());
    return;
  }

  // Form a new generic signature based on the old one.
  GenericSignatureBuilder Builder(D->getASTContext(),
                           LookUpConformanceInModule(DC->getParentModule()));

  // First, add the old generic signature.
  Builder.addGenericSignature(genericSig);

  SmallVector<Requirement, 4> convertedRequirements;
  SmallVector<RequirementRepr, 4> resolvedRequirements;

  // Add all requirements from the "where" clause to the old signature
  // to check if there are any inconsistencies.
  auto options = TypeResolutionOptions();

  // Set of generic parameters being constrained. It is used to
  // determine if a full specialization misses requirements for
  // some of the generic parameters.
  SmallPtrSet<TypeBase *, 4> constrainedGenericParams;

  // Go over the set of requirements and resolve their types.
  for (auto &req : trailingWhereClause->getRequirements()) {
    if (req.getKind() == RequirementReprKind::SameType) {
      auto firstType = TC.resolveType(req.getFirstTypeRepr(), FD, options);
      auto secondType = TC.resolveType(req.getSecondTypeRepr(), FD, options);
      Type interfaceFirstType;
      Type interfaceSecondType;

      // Map types to their interface types.
      if (firstType)
        interfaceFirstType = genericEnv->mapTypeOutOfContext(firstType);
      if (secondType)
        interfaceSecondType = genericEnv->mapTypeOutOfContext(secondType);

      collectUsedGenericParameters(interfaceFirstType,
                                   constrainedGenericParams);
      collectUsedGenericParameters(interfaceSecondType,
                                   constrainedGenericParams);

      // Skip any unknown or error types.
      if (!firstType || firstType->is<ErrorType>() || !secondType ||
          secondType->is<ErrorType>())
        continue;

      Type genericType;
      Type concreteType;
      if (interfaceFirstType->hasTypeParameter()) {
        genericType = interfaceFirstType;
        concreteType = interfaceSecondType;
      } else {
        genericType = interfaceSecondType;
        concreteType = interfaceFirstType;
      }
      // Add a resolved requirement.
      if (interfaceFirstType->hasTypeParameter()) {
        resolvedRequirements.push_back(RequirementRepr::getSameType(
            TypeLoc(req.getFirstTypeRepr(), genericType), req.getEqualLoc(),
            TypeLoc(req.getSecondTypeRepr(), concreteType)));
      } else {
        resolvedRequirements.push_back(RequirementRepr::getSameType(
            TypeLoc(req.getFirstTypeRepr(), concreteType), req.getEqualLoc(),
            TypeLoc(req.getSecondTypeRepr(), genericType)));
      }

      // Convert the requirement into a form which uses canonical interface
      // types.
      Requirement convertedRequirement(RequirementKind::SameType,
                                       genericType->getCanonicalType(),
                                       concreteType->getCanonicalType());
      convertedRequirements.push_back(convertedRequirement);
      continue;
    }

    if (req.getKind() == RequirementReprKind::LayoutConstraint) {
      auto subjectType = TC.resolveType(req.getSubjectRepr(), FD, options);
      Type interfaceSubjectType;

      // Map types to their interface types.
      if (subjectType)
        interfaceSubjectType = genericEnv->mapTypeOutOfContext(subjectType);

      collectUsedGenericParameters(interfaceSubjectType,
                                   constrainedGenericParams);

      // Skip any unknown or error types.
      if (!subjectType || subjectType->is<ErrorType>() ||
          !req.getLayoutConstraint() ||
          !req.getLayoutConstraint()->isKnownLayout())
        continue;

      // Re-create a requirement using the resolved interface types.
      auto resolvedReq = RequirementRepr::getLayoutConstraint(
          TypeLoc(req.getSubjectRepr(), interfaceSubjectType),
          req.getColonLoc(),
          req.getLayoutConstraintLoc());

      // Add a resolved requirement.
      resolvedRequirements.push_back(resolvedReq);

      // Convert the requirement into a form which uses canonical interface
      // types.
      Requirement convertedRequirement(
          RequirementKind::Layout,
          interfaceSubjectType->getCanonicalType(),
          req.getLayoutConstraint());
      convertedRequirements.push_back(convertedRequirement);
      continue;
    }

    if (req.getKind() == RequirementReprKind::TypeConstraint) {
      auto subjectType = TC.resolveType(req.getSubjectRepr(), FD, options);
      auto constraint = TC.resolveType(
          req.getConstraintLoc().getTypeRepr(), FD, options);

      Type interfaceSubjectType;

      // Map types to their interface types.
      if (subjectType)
        interfaceSubjectType = genericEnv->mapTypeOutOfContext(subjectType);

      collectUsedGenericParameters(interfaceSubjectType,
                                   constrainedGenericParams);

      // Skip any unknown or error types.
      if (!subjectType || subjectType->hasError() ||
          !constraint || constraint->hasError())
        continue;


      auto interfaceLayoutConstraint =
          genericEnv->mapTypeOutOfContext(constraint);

      // Re-create a requirement using the resolved interface types.
      auto resolvedReq = RequirementRepr::getTypeConstraint(
          TypeLoc(req.getSubjectRepr(), interfaceSubjectType),
          req.getColonLoc(),
          TypeLoc(req.getConstraintRepr(), interfaceLayoutConstraint));

      // Add a resolved requirement.
      resolvedRequirements.push_back(resolvedReq);

      // Convert the requirement into a form which uses canonical interface
      // types.
      Requirement convertedRequirement(
          RequirementKind::Conformance,
          interfaceSubjectType->getCanonicalType(),
          interfaceLayoutConstraint->getCanonicalType());
      convertedRequirements.push_back(convertedRequirement);
      continue;
    }
  }

  // Check the validity of provided requirements.
  checkSpecializeAttrRequirements(attr, FD, resolvedRequirements,
                                  constrainedGenericParams, TC);

  // Store converted requirements in the attribute so that they are
  // serialized later.
  attr->setRequirements(DC->getASTContext(), convertedRequirements);

  // Add the requirements to the builder.
  for (auto &req : resolvedRequirements)
    Builder.addRequirement(&req);

  // Check the result.
  Builder.finalize(attr->getLocation(), genericSig->getGenericParams(),
                   /*allowConcreteGenericParams=*/true);
}

static Accessibility getAccessForDiagnostics(const ValueDecl *D) {
  return std::min(D->getFormalAccess(),
                  D->getEffectiveAccess());
}

void AttributeChecker::visitFixedLayoutAttr(FixedLayoutAttr *attr) {
  auto *VD = cast<ValueDecl>(D);

  if (VD->getEffectiveAccess() < Accessibility::Public) {
    TC.diagnose(attr->getLocation(),
                diag::fixed_layout_attr_on_internal_type,
                VD->getName(),
                getAccessForDiagnostics(VD))
        .fixItRemove(attr->getRangeWithAt());
    attr->setInvalid();
  }
}

void AttributeChecker::visitVersionedAttr(VersionedAttr *attr) {
  auto *VD = cast<ValueDecl>(D);

  // FIXME: Once protocols can contain nominal types, do we want to allow
  // these nominal types to have accessibility (and also @_versioned)?
  if (isa<ProtocolDecl>(VD->getDeclContext())) {
    TC.diagnose(attr->getLocation(),
                diag::versioned_attr_in_protocol)
        .fixItRemove(attr->getRangeWithAt());
    attr->setInvalid();
    return;
  }

  // @_versioned can only be applied to internal declarations.
  if (VD->getFormalAccess() != Accessibility::Internal) {
    TC.diagnose(attr->getLocation(),
                diag::versioned_attr_with_explicit_accessibility,
                VD->getName(),
                getAccessForDiagnostics(VD))
        .fixItRemove(attr->getRangeWithAt());
    attr->setInvalid();
    return;
  }
}

void AttributeChecker::visitInlineableAttr(InlineableAttr *attr) {
  // @_inlineable cannot be applied to stored properties.
  //
  // If the type is fixed-layout, the accessors are inlineable anyway;
  // if the type is resilient, the accessors cannot be inlineable
  // because clients cannot directly access storage.
  if (auto *VD = dyn_cast<VarDecl>(D)) {
    if (VD->hasStorage() || VD->getAttrs().hasAttribute<LazyAttr>()) {
      TC.diagnose(attr->getLocation(),
                  diag::inlineable_stored_property)
        .fixItRemove(attr->getRangeWithAt());
      attr->setInvalid();
    }
  }

  auto *VD = cast<ValueDecl>(D);

  // @_inlineable can only be applied to public or @_versioned
  // declarations.
  if (VD->getFormalAccess() < Accessibility::Internal ||
      (!VD->getAttrs().hasAttribute<VersionedAttr>() &&
       VD->getFormalAccess() < Accessibility::Public)) {
    TC.diagnose(attr->getLocation(),
                diag::inlineable_decl_not_public,
                VD->getName(),
                getAccessForDiagnostics(VD))
        .fixItRemove(attr->getRangeWithAt());
    attr->setInvalid();
    return;
  }
}

void AttributeChecker::visitDiscardableResultAttr(DiscardableResultAttr *attr) {
  if (auto *FD = dyn_cast<FuncDecl>(D)) {
    if (auto result = FD->getResultInterfaceType()) {
      auto resultIsVoid = result->isVoid();
      if (resultIsVoid || result->isUninhabited()) {
        auto warn = diag::discardable_result_on_void_never_function;
        auto diagnostic = TC.diagnose(D->getStartLoc(), warn, resultIsVoid);
        diagnostic.fixItRemove(attr->getRangeWithAt());
        attr->setInvalid();
      }
    }
  }
}

void TypeChecker::checkDeclAttributes(Decl *D) {
  AttributeChecker Checker(*this, D);

  for (auto attr : D->getAttrs()) {
    if (attr->isValid())
      Checker.visit(attr);
  }
}

void TypeChecker::checkTypeModifyingDeclAttributes(VarDecl *var) {
  if (!var->hasType())
    return;

  if (auto *attr = var->getAttrs().getAttribute<OwnershipAttr>())
    checkOwnershipAttr(var, attr);
  if (auto *attr = var->getAttrs().getAttribute<AutoClosureAttr>()) {
    if (auto *pd = dyn_cast<ParamDecl>(var))
      checkAutoClosureAttr(pd, attr);
    else {
      AttributeEarlyChecker Checker(*this, var);
      Checker.diagnoseAndRemoveAttr(attr, diag::attr_only_one_decl_kind,
                                    attr, "parameter");
    }
  }
  if (auto *attr = var->getAttrs().getAttribute<NoEscapeAttr>()) {
    if (auto *pd = dyn_cast<ParamDecl>(var))
      checkNoEscapeAttr(pd, attr);
    else {
      AttributeEarlyChecker Checker(*this, var);
      Checker.diagnoseAndRemoveAttr(attr, diag::attr_only_one_decl_kind,
                                    attr, "parameter");
    }
  }
}



void TypeChecker::checkAutoClosureAttr(ParamDecl *PD, AutoClosureAttr *attr) {
  // The paramdecl should have function type, and we restrict it to functions
  // taking ().
  auto *FTy = PD->getInterfaceType()->getAs<FunctionType>();
  if (!FTy) {
    diagnose(attr->getLocation(), diag::autoclosure_function_type);
    attr->setInvalid();
    return;
  }

  // Just stop if we've already applied this attribute.
  if (FTy->isAutoClosure())
    return;

  // This decl attribute has been moved to being a type attribute.
  auto text = attr->isEscaping() ? "@autoclosure @escaping " : "@autoclosure ";
  diagnose(attr->getLocation(), diag::attr_decl_attr_now_on_type,
           "@autoclosure")
    .fixItRemove(attr->getRangeWithAt())
    .fixItInsert(PD->getTypeLoc().getSourceRange().Start, text);

  auto *FuncTyInput = FTy->getInput()->getAs<TupleType>();
  if (!FuncTyInput || FuncTyInput->getNumElements() != 0) {
    diagnose(attr->getLocation(), diag::autoclosure_function_input_nonunit);
    attr->setInvalid();
    return;
  }

  // Change the type to include the autoclosure bit.
  PD->setType(
      FTy->withExtInfo(FTy->getExtInfo().withIsAutoClosure(true)));

  // And the interface type.
  auto *IfaceFTy = PD->getInterfaceType()->getAs<FunctionType>();
  PD->setInterfaceType(
      IfaceFTy->withExtInfo(IfaceFTy->getExtInfo().withIsAutoClosure(true)));

  // Autoclosure may imply noescape, so add a noescape attribute if this is a
  // function parameter.
  if (auto *NEAttr = PD->getAttrs().getAttribute<NoEscapeAttr>()) {
    // If the parameter has both @noescape and @autoclosure, reject the
    // explicit @noescape.
    if (!NEAttr->isImplicit())
      diagnose(NEAttr->getLocation(),
               attr->isEscaping()
                 ? diag::noescape_conflicts_escaping_autoclosure
                 : diag::noescape_implied_by_autoclosure)
        .fixItRemove(NEAttr->getRange());
  } else if (!attr->isEscaping()) {
    auto *newAttr = new (Context) NoEscapeAttr(/*isImplicit*/true);
    PD->getAttrs().add(newAttr);
    checkNoEscapeAttr(PD, newAttr);
  }
}

void TypeChecker::checkNoEscapeAttr(ParamDecl *PD, NoEscapeAttr *attr) {
  // The paramdecl should have function type.
  auto *FTy = PD->getInterfaceType()->getAs<FunctionType>();
  if (FTy == nullptr) {
    diagnose(attr->getLocation(), diag::noescape_function_type);
    attr->setInvalid();
    return;
  }

  // This range can be implicit e.g. if we're in the middle of diagnosing
  // @autoclosure.
  auto attrRemovalRange = attr->getRangeWithAt();
  if (attrRemovalRange.isValid()) {
    // Take the attribute, the '@', and the trailing space.
    attrRemovalRange.End = attrRemovalRange.End.getAdvancedLoc(1);
  }
  
  // This decl attribute has been moved to being a type attribute.
  if (!attr->isImplicit()) {
    diagnose(attr->getLocation(), diag::attr_decl_attr_now_on_type, "@noescape")
        .fixItRemove(attrRemovalRange)
        .fixItInsert(PD->getTypeLoc().getSourceRange().Start, "@noescape ");
  }

  // Stop if we've already applied this attribute.
  if (FTy->isNoEscape())
    return;

  // Change the type to include the noescape bit.
  PD->setType(FunctionType::get(FTy->getInput(), FTy->getResult(),
                                FTy->getExtInfo().withNoEscape(true)));
}


void TypeChecker::checkOwnershipAttr(VarDecl *var, OwnershipAttr *attr) {
  Type type = var->getType();
  Type interfaceType = var->getInterfaceType();

  // Just stop if we've already processed this declaration.
  if (type->is<ReferenceStorageType>())
    return;

  auto ownershipKind = attr->get();
  assert(ownershipKind != Ownership::Strong &&
         "Cannot specify 'strong' in an ownership attribute");

  // A weak variable must have type R? or R! for some ownership-capable type R.
  Type underlyingType = type;
  if (ownershipKind == Ownership::Weak) {
    if (var->isLet()) {
      diagnose(var->getStartLoc(), diag::invalid_weak_let);
      attr->setInvalid();
      return;
    }

    if (Type objType = type->getAnyOptionalObjectType())
      underlyingType = objType;
    else if (type->allowsOwnership()) {
      // Use this special diagnostic if it's actually a reference type but just
      // isn't Optional.
      if (var->getAttrs().hasAttribute<IBOutletAttr>()) {
        // Let @IBOutlet complain about this; it's more specific.
        attr->setInvalid();
        return;
      }

      diagnose(var->getStartLoc(), diag::invalid_weak_ownership_not_optional,
               OptionalType::get(type));
      attr->setInvalid();

      return;
    } else {
      // This is also an error, but the code below will diagnose it.
    }
  } else if (ownershipKind == Ownership::Strong) {
    // We allow strong on optional-qualified reference types.
    if (Type objType = type->getAnyOptionalObjectType())
      underlyingType = objType;
  }

  if (!underlyingType->allowsOwnership()) {
    // If we have an opaque type, suggest the possibility of adding
    // a class bound.
    if (type->isExistentialType() || type->is<ArchetypeType>()) {
      diagnose(var->getStartLoc(), diag::invalid_ownership_protocol_type,
               (unsigned) ownershipKind, underlyingType);
    } else {
      diagnose(var->getStartLoc(), diag::invalid_ownership_type,
               (unsigned) ownershipKind, underlyingType);
    }
    attr->setInvalid();
    return;
  }

  // Change the type to the appropriate reference storage type.
  var->setType(ReferenceStorageType::get(
      type, ownershipKind, Context));
  var->setInterfaceType(ReferenceStorageType::get(
      interfaceType, ownershipKind, Context));
}

Optional<Diag<>>
TypeChecker::diagnosticIfDeclCannotBePotentiallyUnavailable(const Decl *D) {
  DeclContext *DC = D->getDeclContext();
  // Do not permit potential availability of script-mode global variables;
  // their initializer expression is not lazily evaluated, so this would
  // not be safe.
  if (isa<VarDecl>(D) && DC->isModuleScopeContext() &&
      DC->getParentSourceFile()->isScriptMode()) {
    return diag::availability_global_script_no_potential;
  }

  // For now, we don't allow stored properties to be potentially unavailable.
  // We will want to support these eventually, but we haven't figured out how
  // this will interact with Definite Initialization, deinitializers and
  // resilience yet.
  if (auto *VD = dyn_cast<VarDecl>(D)) {
    // Globals and statics are lazily initialized, so they are safe
    // for potential unavailability. Note that if D is a global in script
    // mode (which are not lazy) then we will already have returned
    // a diagnosis above.
    bool lazilyInitializedStored = VD->isStatic() ||
                                   VD->getAttrs().hasAttribute<LazyAttr>() ||
                                   DC->isModuleScopeContext();

    if (VD->hasStorage() && !lazilyInitializedStored) {
      return diag::availability_stored_property_no_potential;
    }
  }

  return None;
}

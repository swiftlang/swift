//===--- TypeCheckDeclObjC.cpp - Type Checking for ObjC Declarations ------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2018 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
// This file implements semantic analysis for Objective-C-specific
// aspects of declarations.
//
//===----------------------------------------------------------------------===//
#include "TypeCheckObjC.h"
#include "TypeChecker.h"
#include "TypeCheckProtocol.h"
#include "swift/AST/ASTContext.h"
#include "swift/AST/Decl.h"
#include "swift/AST/ExistentialLayout.h"
#include "swift/AST/ForeignErrorConvention.h"
#include "swift/AST/ParameterList.h"
#include "swift/AST/TypeCheckRequests.h"
#include "swift/Basic/StringExtras.h"
using namespace swift;

#pragma mark Determine whether an entity is representable in Objective-C.

bool swift::shouldDiagnoseObjCReason(ObjCReason reason, ASTContext &ctx) {
  switch(reason) {
  case ObjCReason::ExplicitlyCDecl:
  case ObjCReason::ExplicitlyDynamic:
  case ObjCReason::ExplicitlyObjC:
  case ObjCReason::ExplicitlyIBOutlet:
  case ObjCReason::ExplicitlyIBAction:
  case ObjCReason::ExplicitlyNSManaged:
  case ObjCReason::MemberOfObjCProtocol:
  case ObjCReason::OverridesObjC:
  case ObjCReason::WitnessToObjC:
  case ObjCReason::ImplicitlyObjC:
  case ObjCReason::MemberOfObjCExtension:
    return true;

  case ObjCReason::ExplicitlyIBInspectable:
  case ObjCReason::ExplicitlyGKInspectable:
    return !ctx.LangOpts.EnableSwift3ObjCInference;

  case ObjCReason::MemberOfObjCSubclass:
  case ObjCReason::MemberOfObjCMembersClass:
  case ObjCReason::ElementOfObjCEnum:
  case ObjCReason::Accessor:
    return false;
  }
  llvm_unreachable("unhandled reason");
}

unsigned swift::getObjCDiagnosticAttrKind(ObjCReason reason) {
  switch (reason) {
  case ObjCReason::ExplicitlyCDecl:
  case ObjCReason::ExplicitlyDynamic:
  case ObjCReason::ExplicitlyObjC:
  case ObjCReason::ExplicitlyIBOutlet:
  case ObjCReason::ExplicitlyIBAction:
  case ObjCReason::ExplicitlyNSManaged:
  case ObjCReason::MemberOfObjCProtocol:
  case ObjCReason::OverridesObjC:
  case ObjCReason::WitnessToObjC:
  case ObjCReason::ImplicitlyObjC:
  case ObjCReason::ExplicitlyIBInspectable:
  case ObjCReason::ExplicitlyGKInspectable:
  case ObjCReason::MemberOfObjCExtension:
    return static_cast<unsigned>(reason);

  case ObjCReason::MemberOfObjCSubclass:
  case ObjCReason::MemberOfObjCMembersClass:
  case ObjCReason::ElementOfObjCEnum:
  case ObjCReason::Accessor:
    llvm_unreachable("should not diagnose this @objc reason");
  }
  llvm_unreachable("unhandled reason");
}

/// Emit an additional diagnostic describing why we are applying @objc to the
/// decl, if this is not obvious from the decl itself.
static void describeObjCReason(const ValueDecl *VD, ObjCReason Reason) {
  if (Reason == ObjCReason::MemberOfObjCProtocol) {
    VD->diagnose(diag::objc_inferring_on_objc_protocol_member);
  } else if (Reason == ObjCReason::OverridesObjC) {
    unsigned kind = isa<VarDecl>(VD) ? 0
                  : isa<SubscriptDecl>(VD) ? 1
                  : isa<ConstructorDecl>(VD) ? 2
                  : 3;

    auto overridden = VD->getOverriddenDecl();
    overridden->diagnose(diag::objc_overriding_objc_decl,
                         kind, VD->getOverriddenDecl()->getFullName());
  } else if (Reason == ObjCReason::WitnessToObjC) {
    auto requirement = Reason.getObjCRequirement();
    requirement->diagnose(diag::objc_witness_objc_requirement,
                VD->getDescriptiveKind(), requirement->getFullName(),
                cast<ProtocolDecl>(requirement->getDeclContext())
                  ->getFullName());
  }
}

static void diagnoseTypeNotRepresentableInObjC(const DeclContext *DC,
                                               Type T,
                                               SourceRange TypeRange) {
  auto &diags = DC->getASTContext().Diags;

  // Special diagnostic for tuples.
  if (T->is<TupleType>()) {
    if (T->isVoid())
      diags.diagnose(TypeRange.Start, diag::not_objc_empty_tuple)
          .highlight(TypeRange);
    else
      diags.diagnose(TypeRange.Start, diag::not_objc_tuple)
          .highlight(TypeRange);
    return;
  }

  // Special diagnostic for classes.
  if (auto *CD = T->getClassOrBoundGenericClass()) {
    if (!CD->isObjC())
      diags.diagnose(TypeRange.Start, diag::not_objc_swift_class)
          .highlight(TypeRange);
    return;
  }

  // Special diagnostic for structs.
  if (T->is<StructType>()) {
    diags.diagnose(TypeRange.Start, diag::not_objc_swift_struct)
        .highlight(TypeRange);
    return;
  }

  // Special diagnostic for enums.
  if (T->is<EnumType>()) {
    diags.diagnose(TypeRange.Start, diag::not_objc_swift_enum)
        .highlight(TypeRange);
    return;
  }

  // Special diagnostic for protocols and protocol compositions.
  if (T->isExistentialType()) {
    if (T->isAny()) {
      // Any is not @objc.
      diags.diagnose(TypeRange.Start,
                     diag::not_objc_empty_protocol_composition);
      return;
    }

    auto layout = T->getExistentialLayout();

    // See if the superclass is not @objc.
    if (auto superclass = layout.explicitSuperclass) {
      if (!superclass->getClassOrBoundGenericClass()->isObjC()) {
        diags.diagnose(TypeRange.Start, diag::not_objc_class_constraint,
                       superclass);
        return;
      }
    }

    // Find a protocol that is not @objc.
    bool sawErrorProtocol = false;
    for (auto P : layout.getProtocols()) {
      auto *PD = P->getDecl();

      if (PD->isSpecificProtocol(KnownProtocolKind::Error)) {
        sawErrorProtocol = true;
        break;
      }

      if (!PD->isObjC()) {
        diags.diagnose(TypeRange.Start, diag::not_objc_protocol,
                       PD->getDeclaredType());
        return;
      }
    }

    if (sawErrorProtocol) {
      diags.diagnose(TypeRange.Start,
                     diag::not_objc_error_protocol_composition);
      return;
    }

    return;
  }

  if (T->is<ArchetypeType>() || T->isTypeParameter()) {
    diags.diagnose(TypeRange.Start, diag::not_objc_generic_type_param)
        .highlight(TypeRange);
    return;
  }

  if (auto fnTy = T->getAs<FunctionType>()) {
    if (fnTy->getExtInfo().throws() ) {
      diags.diagnose(TypeRange.Start, diag::not_objc_function_type_throwing)
        .highlight(TypeRange);
      return;
    }

    diags.diagnose(TypeRange.Start, diag::not_objc_function_type_param)
      .highlight(TypeRange);
    return;
  }
}

static void diagnoseFunctionParamNotRepresentable(
    const AbstractFunctionDecl *AFD, unsigned NumParams,
    unsigned ParamIndex, const ParamDecl *P, ObjCReason Reason) {
  if (!shouldDiagnoseObjCReason(Reason, AFD->getASTContext()))
    return;

  if (NumParams == 1) {
    AFD->diagnose(diag::objc_invalid_on_func_single_param_type,
                  getObjCDiagnosticAttrKind(Reason));
  } else {
    AFD->diagnose(diag::objc_invalid_on_func_param_type,
                  ParamIndex + 1, getObjCDiagnosticAttrKind(Reason));
  }
  if (P->hasType()) {
    Type ParamTy = P->getType();
    SourceRange SR;
    if (auto typeRepr = P->getTypeLoc().getTypeRepr())
      SR = typeRepr->getSourceRange();
    diagnoseTypeNotRepresentableInObjC(AFD, ParamTy, SR);
  }
  describeObjCReason(AFD, Reason);
}

static bool isParamListRepresentableInObjC(const AbstractFunctionDecl *AFD,
                                           const ParameterList *PL,
                                           ObjCReason Reason) {
  // If you change this function, you must add or modify a test in PrintAsObjC.
  ASTContext &ctx = AFD->getASTContext();
  auto &diags = ctx.Diags;

  if (!AFD->hasInterfaceType()) {
    ctx.getLazyResolver()->resolveDeclSignature(
                                      const_cast<AbstractFunctionDecl *>(AFD));
  }

  bool Diagnose = shouldDiagnoseObjCReason(Reason, ctx);
  bool IsObjC = true;
  unsigned NumParams = PL->size();
  for (unsigned ParamIndex = 0; ParamIndex != NumParams; ParamIndex++) {
    auto param = PL->get(ParamIndex);

    // Swift Varargs are not representable in Objective-C.
    if (param->isVariadic()) {
      if (Diagnose && shouldDiagnoseObjCReason(Reason, ctx)) {
        diags.diagnose(param->getStartLoc(), diag::objc_invalid_on_func_variadic,
                       getObjCDiagnosticAttrKind(Reason))
          .highlight(param->getSourceRange());
        describeObjCReason(AFD, Reason);
      }

      return false;
    }

    // Swift inout parameters are not representable in Objective-C.
    if (param->isInOut()) {
      if (Diagnose && shouldDiagnoseObjCReason(Reason, ctx)) {
        diags.diagnose(param->getStartLoc(), diag::objc_invalid_on_func_inout,
                       getObjCDiagnosticAttrKind(Reason))
          .highlight(param->getSourceRange());
        describeObjCReason(AFD, Reason);
      }

      return false;
    }

    if (param->getType()->hasError())
      return false;
    
    if (param->getType()->isRepresentableIn(
          ForeignLanguage::ObjectiveC,
          const_cast<AbstractFunctionDecl *>(AFD)))
      continue;

    // Permit '()' when this method overrides a method with a
    // foreign error convention that replaces NSErrorPointer with ()
    // and this is the replaced parameter.
    AbstractFunctionDecl *overridden;
    if (param->getType()->isVoid() && AFD->hasThrows() &&
        (overridden = AFD->getOverriddenDecl())) {
      auto foreignError = overridden->getForeignErrorConvention();
      if (foreignError &&
          foreignError->isErrorParameterReplacedWithVoid() &&
          foreignError->getErrorParameterIndex() == ParamIndex) {
        continue;
      }
    }

    IsObjC = false;
    if (!Diagnose) {
      // Save some work and return as soon as possible if we are not
      // producing diagnostics.
      return IsObjC;
    }
    diagnoseFunctionParamNotRepresentable(AFD, NumParams, ParamIndex,
                                          param, Reason);
  }
  return IsObjC;
}

/// Check whether the given declaration contains its own generic parameters,
/// and therefore is not representable in Objective-C.
static bool checkObjCWithGenericParams(const AbstractFunctionDecl *AFD,
                                       ObjCReason Reason) {
  bool Diagnose = shouldDiagnoseObjCReason(Reason, AFD->getASTContext());

  if (AFD->getGenericParams()) {
    // Diagnose this problem, if asked to.
    if (Diagnose) {
      AFD->diagnose(diag::objc_invalid_with_generic_params,
                    getObjCDiagnosticAttrKind(Reason));
      describeObjCReason(AFD, Reason);
    }

    return true;
  }

  return false;
}

/// CF types cannot have @objc methods, because they don't have real class
/// objects.
static bool checkObjCInForeignClassContext(const ValueDecl *VD,
                                           ObjCReason Reason) {
  bool Diagnose = shouldDiagnoseObjCReason(Reason, VD->getASTContext());

  auto type = VD->getDeclContext()->getDeclaredInterfaceType();
  if (!type)
    return false;

  auto clas = type->getClassOrBoundGenericClass();
  if (!clas)
    return false;

  switch (clas->getForeignClassKind()) {
  case ClassDecl::ForeignKind::Normal:
    return false;

  case ClassDecl::ForeignKind::CFType:
    if (Diagnose) {
      VD->diagnose(diag::objc_invalid_on_foreign_class,
                   getObjCDiagnosticAttrKind(Reason));
      describeObjCReason(VD, Reason);
    }
    break;

  case ClassDecl::ForeignKind::RuntimeOnly:
    if (Diagnose) {
      VD->diagnose(diag::objc_in_objc_runtime_visible,
                   VD->getDescriptiveKind(), getObjCDiagnosticAttrKind(Reason),
                   clas->getName());
      describeObjCReason(VD, Reason);
    }
    break;
  }

  return true;
}

/// Check whether the given declaration occurs within a constrained
/// extension, or an extension of a generic class, or an
/// extension of an Objective-C runtime visible class, and
/// therefore is not representable in Objective-C.
static bool checkObjCInExtensionContext(const ValueDecl *value,
                                        bool diagnose) {
  auto DC = value->getDeclContext();

  if (auto ED = dyn_cast<ExtensionDecl>(DC)) {
    if (ED->getTrailingWhereClause()) {
      if (diagnose) {
        value->diagnose(diag::objc_in_extension_context);
      }
      return true;
    }

    if (auto classDecl = ED->getSelfClassDecl()) {
      if (classDecl->isGenericContext()) {
        if (!classDecl->usesObjCGenericsModel()) {
          if (diagnose) {
            value->diagnose(diag::objc_in_generic_extension,
                            classDecl->isGeneric());
          }
          return true;
        }
      }
    }
  }

  return false;
}

/// Determines whether the given type is bridged to an Objective-C class type.
static bool isBridgedToObjectiveCClass(DeclContext *dc, Type type) {
  switch (type->getForeignRepresentableIn(ForeignLanguage::ObjectiveC, dc)
            .first) {
  case ForeignRepresentableKind::Trivial:
  case ForeignRepresentableKind::None:
    return false;

  case ForeignRepresentableKind::Object:
  case ForeignRepresentableKind::Bridged:
  case ForeignRepresentableKind::BridgedError:
  case ForeignRepresentableKind::StaticBridged:
    return true;
  }

  llvm_unreachable("Unhandled ForeignRepresentableKind in switch.");
}

bool swift::isRepresentableInObjC(
       const AbstractFunctionDecl *AFD,
       ObjCReason Reason,
       Optional<ForeignErrorConvention> &errorConvention) {
  // Clear out the error convention. It will be added later if needed.
  errorConvention = None;

  // If you change this function, you must add or modify a test in PrintAsObjC.
  ASTContext &ctx = AFD->getASTContext();
  bool Diagnose = shouldDiagnoseObjCReason(Reason, ctx);

  if (checkObjCInForeignClassContext(AFD, Reason))
    return false;
  if (checkObjCWithGenericParams(AFD, Reason))
    return false;
  if (checkObjCInExtensionContext(AFD, Diagnose))
    return false;

  if (AFD->isOperator()) {
    AFD->diagnose((isa<ProtocolDecl>(AFD->getDeclContext())
                    ? diag::objc_operator_proto
                    : diag::objc_operator));
    return false;
  }

  if (auto accessor = dyn_cast<AccessorDecl>(AFD)) {
    // Accessors can only be @objc if the storage declaration is.
    // Global computed properties may however @_cdecl their accessors.
    auto storage = accessor->getStorage();
    if (!storage->isObjC() && Reason != ObjCReason::ExplicitlyCDecl &&
        Reason != ObjCReason::WitnessToObjC &&
        Reason != ObjCReason::MemberOfObjCProtocol) {
      if (Diagnose) {
        auto error = accessor->isGetter()
                  ? (isa<VarDecl>(storage)
                       ? diag::objc_getter_for_nonobjc_property
                       : diag::objc_getter_for_nonobjc_subscript)
                  : (isa<VarDecl>(storage)
                       ? diag::objc_setter_for_nonobjc_property
                       : diag::objc_setter_for_nonobjc_subscript);

        accessor->diagnose(error);
        describeObjCReason(accessor, Reason);
      }
      return false;
    }

    switch (accessor->getAccessorKind()) {
    case AccessorKind::DidSet:
    case AccessorKind::WillSet:
        // willSet/didSet implementations are never exposed to objc, they are
        // always directly dispatched from the synthesized setter.
      if (Diagnose) {
        accessor->diagnose(diag::objc_observing_accessor);
        describeObjCReason(accessor, Reason);
      }
      return false;

    case AccessorKind::Get:
    case AccessorKind::Set:
      return true;

    case AccessorKind::Address:
    case AccessorKind::MutableAddress:
      if (Diagnose) {
        accessor->diagnose(diag::objc_addressor);
        describeObjCReason(accessor, Reason);
      }
      return false;

    case AccessorKind::Read:
    case AccessorKind::Modify:
      if (Diagnose) {
        accessor->diagnose(diag::objc_coroutine_accessor);
        describeObjCReason(accessor, Reason);
      }
      return false;
    }
    llvm_unreachable("bad kind");
  }

  // As a special case, an initializer with a single, named parameter of type
  // '()' is always representable in Objective-C. This allows us to cope with
  // zero-parameter methods with selectors that are longer than "init". For
  // example, this allows:
  //
  // \code
  // class Foo {
  //   @objc init(malice: ()) { } // selector is "initWithMalice"
  // }
  // \endcode
  bool isSpecialInit = false;
  if (auto init = dyn_cast<ConstructorDecl>(AFD))
    isSpecialInit = init->isObjCZeroParameterWithLongSelector();

  if (!isSpecialInit &&
      !isParamListRepresentableInObjC(AFD,
                                      AFD->getParameters(),
                                      Reason)) {
    return false;
  }

  if (auto FD = dyn_cast<FuncDecl>(AFD)) {
    Type ResultType = FD->mapTypeIntoContext(FD->getResultInterfaceType());
    if (!ResultType->hasError() &&
        !ResultType->isVoid() &&
        !ResultType->isUninhabited() &&
        !ResultType->isRepresentableIn(ForeignLanguage::ObjectiveC,
                                       const_cast<FuncDecl *>(FD))) {
      if (Diagnose) {
        AFD->diagnose(diag::objc_invalid_on_func_result_type,
                      getObjCDiagnosticAttrKind(Reason));
        SourceRange Range =
            FD->getBodyResultTypeLoc().getTypeRepr()->getSourceRange();
        diagnoseTypeNotRepresentableInObjC(FD, ResultType, Range);
        describeObjCReason(FD, Reason);
      }
      return false;
    }
  }

  // Throwing functions must map to a particular error convention.
  if (AFD->hasThrows()) {
    DeclContext *dc = const_cast<AbstractFunctionDecl *>(AFD);
    SourceLoc throwsLoc;
    Type resultType;

    const ConstructorDecl *ctor = nullptr;
    if (auto func = dyn_cast<FuncDecl>(AFD)) {
      resultType = func->getResultInterfaceType();
      throwsLoc = func->getThrowsLoc();
    } else {
      ctor = cast<ConstructorDecl>(AFD);
      throwsLoc = ctor->getThrowsLoc();
    }

    ForeignErrorConvention::Kind kind;
    CanType errorResultType;
    Type optOptionalType;
    if (ctor) {
      // Initializers always use the nil result convention.
      kind = ForeignErrorConvention::NilResult;

      // Only non-failing initializers can throw.
      if (ctor->getFailability() != OTK_None) {
        if (Diagnose) {
          AFD->diagnose(diag::objc_invalid_on_failing_init,
                        getObjCDiagnosticAttrKind(Reason))
            .highlight(throwsLoc);
          describeObjCReason(AFD, Reason);
        }

        return false;
      }
    } else if (resultType->isVoid()) {
      // Functions that return nothing (void) can be throwing; they indicate
      // failure with a 'false' result.
      kind = ForeignErrorConvention::ZeroResult;
      NominalTypeDecl *boolDecl = ctx.getObjCBoolDecl();
      // On Linux, we might still run @objc tests even though there's
      // no ObjectiveC Foundation, so use Swift.Bool instead of crapping
      // out.
      if (boolDecl == nullptr)
        boolDecl = ctx.getBoolDecl();

      if (boolDecl == nullptr) {
        AFD->diagnose(diag::broken_bool);
        return false;
      }

      errorResultType = boolDecl->getDeclaredType()->getCanonicalType();
    } else if (!resultType->getOptionalObjectType() &&
               isBridgedToObjectiveCClass(dc, resultType)) {
      // Functions that return a (non-optional) type bridged to Objective-C
      // can be throwing; they indicate failure with a nil result.
      kind = ForeignErrorConvention::NilResult;
    } else if ((optOptionalType = resultType->getOptionalObjectType()) &&
               isBridgedToObjectiveCClass(dc, optOptionalType)) {
      // Cannot return an optional bridged type, because 'nil' is reserved
      // to indicate failure. Call this out in a separate diagnostic.
      if (Diagnose) {
        AFD->diagnose(diag::objc_invalid_on_throwing_optional_result,
                      getObjCDiagnosticAttrKind(Reason),
                      resultType)
          .highlight(throwsLoc);
        describeObjCReason(AFD, Reason);
      }
      return false;
    } else {
      // Other result types are not permitted.
      if (Diagnose) {
        AFD->diagnose(diag::objc_invalid_on_throwing_result,
                      getObjCDiagnosticAttrKind(Reason),
                      resultType)
          .highlight(throwsLoc);
        describeObjCReason(AFD, Reason);
      }
      return false;
    }

    // The error type is always 'AutoreleasingUnsafeMutablePointer<NSError?>?'.
    auto nsError = ctx.getNSErrorDecl();
    Type errorParameterType;
    if (nsError) {
      if (!nsError->hasInterfaceType()) {
        auto resolver = ctx.getLazyResolver();
        assert(resolver);
        resolver->resolveDeclSignature(nsError);
      }
      errorParameterType = nsError->getDeclaredInterfaceType();
      errorParameterType = OptionalType::get(errorParameterType);
      errorParameterType
        = BoundGenericType::get(
            ctx.getAutoreleasingUnsafeMutablePointerDecl(),
            nullptr,
            errorParameterType);
      errorParameterType = OptionalType::get(errorParameterType);
    }

    // Determine the parameter index at which the error will go.
    unsigned errorParameterIndex;
    bool foundErrorParameterIndex = false;

    // If there is an explicit @objc attribute with a name, look for
    // the "error" selector piece.
    if (auto objc = AFD->getAttrs().getAttribute<ObjCAttr>()) {
      if (auto objcName = objc->getName()) {
        auto selectorPieces = objcName->getSelectorPieces();
        for (unsigned i = selectorPieces.size(); i > 0; --i) {
          // If the selector piece is "error", this is the location of
          // the error parameter.
          auto piece = selectorPieces[i-1];
          if (piece == ctx.Id_error) {
            errorParameterIndex = i-1;
            foundErrorParameterIndex = true;
            break;
          }

          // If the first selector piece ends with "Error", it's here.
          if (i == 1 && camel_case::getLastWord(piece.str()) == "Error") {
            errorParameterIndex = i-1;
            foundErrorParameterIndex = true;
            break;
          }
        }
      }
    }

    // If the selector did not provide an index for the error, find
    // the last parameter that is not a trailing closure.
    if (!foundErrorParameterIndex) {
      auto *paramList = AFD->getParameters();
      errorParameterIndex = paramList->size();

      // Note: the errorParameterIndex is actually a SIL function
      // parameter index, which means tuples are exploded. Normally
      // tuple types cannot be bridged to Objective-C, except for
      // one special case -- a constructor with a single named parameter
      // 'foo' of tuple type becomes a zero-argument selector named
      // 'initFoo'.
      if (auto *CD = dyn_cast<ConstructorDecl>(AFD))
        if (CD->isObjCZeroParameterWithLongSelector())
          errorParameterIndex--;

      while (errorParameterIndex > 0) {
        // Skip over trailing closures.
        auto type = paramList->get(errorParameterIndex - 1)->getType();

        // It can't be a trailing closure unless it has a specific form.
        // Only consider the rvalue type.
        type = type->getRValueType();

        // Look through one level of optionality.
        if (auto objectType = type->getOptionalObjectType())
          type = objectType;

        // Is it a function type?
        if (!type->is<AnyFunctionType>()) break;
        --errorParameterIndex;
      }
    }

    // Form the error convention.
    CanType canErrorParameterType;
    if (errorParameterType)
      canErrorParameterType = errorParameterType->getCanonicalType();
    switch (kind) {
    case ForeignErrorConvention::ZeroResult:
      errorConvention = ForeignErrorConvention::getZeroResult(
                          errorParameterIndex,
                          ForeignErrorConvention::IsNotOwned,
                          ForeignErrorConvention::IsNotReplaced,
                          canErrorParameterType,
                          errorResultType);
      break;

    case ForeignErrorConvention::NonZeroResult:
      errorConvention = ForeignErrorConvention::getNonZeroResult(
                          errorParameterIndex,
                          ForeignErrorConvention::IsNotOwned,
                          ForeignErrorConvention::IsNotReplaced,
                          canErrorParameterType,
                          errorResultType);
      break;

    case ForeignErrorConvention::ZeroPreservedResult:
      errorConvention = ForeignErrorConvention::getZeroPreservedResult(
                          errorParameterIndex,
                          ForeignErrorConvention::IsNotOwned,
                          ForeignErrorConvention::IsNotReplaced,
                          canErrorParameterType);
      break;

    case ForeignErrorConvention::NilResult:
      errorConvention = ForeignErrorConvention::getNilResult(
                          errorParameterIndex,
                          ForeignErrorConvention::IsNotOwned,
                          ForeignErrorConvention::IsNotReplaced,
                          canErrorParameterType);
      break;

    case ForeignErrorConvention::NonNilError:
      errorConvention = ForeignErrorConvention::getNilResult(
                          errorParameterIndex,
                          ForeignErrorConvention::IsNotOwned,
                          ForeignErrorConvention::IsNotReplaced,
                          canErrorParameterType);
      break;
    }
  }

  return true;
}

bool swift::isRepresentableInObjC(const VarDecl *VD, ObjCReason Reason) {
  // If you change this function, you must add or modify a test in PrintAsObjC.

  if (VD->isInvalid())
    return false;

  if (!VD->hasInterfaceType()) {
    VD->getASTContext().getLazyResolver()->resolveDeclSignature(
                                              const_cast<VarDecl *>(VD));
    if (!VD->hasInterfaceType()) {
      VD->diagnose(diag::recursive_decl_reference, VD->getDescriptiveKind(),
                   VD->getName());
      return false;
    }
  }

  Type T = VD->getDeclContext()->mapTypeIntoContext(VD->getInterfaceType());
  if (auto *RST = T->getAs<ReferenceStorageType>()) {
    // In-memory layout of @weak and @unowned does not correspond to anything
    // in Objective-C, but this does not really matter here, since Objective-C
    // uses getters and setters to operate on the property.
    // Because of this, look through @weak and @unowned.
    T = RST->getReferentType();
  }
  ASTContext &ctx = VD->getASTContext();
  bool Result = T->isRepresentableIn(ForeignLanguage::ObjectiveC,
                                     VD->getDeclContext());
  bool Diagnose = shouldDiagnoseObjCReason(Reason, ctx);

  if (Result && checkObjCInExtensionContext(VD, Diagnose))
    return false;

  if (checkObjCInForeignClassContext(VD, Reason))
    return false;

  if (!Diagnose || Result)
    return Result;

  SourceRange TypeRange = VD->getTypeSourceRangeForDiagnostics();
  // TypeRange can be invalid; e.g. '@objc let foo = SwiftType()'
  if (TypeRange.isInvalid())
    TypeRange = VD->getNameLoc();

  VD->diagnose(diag::objc_invalid_on_var, getObjCDiagnosticAttrKind(Reason))
      .highlight(TypeRange);
  diagnoseTypeNotRepresentableInObjC(VD->getDeclContext(),
                                     VD->getInterfaceType(),
                                     TypeRange);
  describeObjCReason(VD, Reason);

  return Result;
}

bool swift::isRepresentableInObjC(const SubscriptDecl *SD, ObjCReason Reason) {
  // If you change this function, you must add or modify a test in PrintAsObjC.
  ASTContext &ctx = SD->getASTContext();
  bool Diagnose = shouldDiagnoseObjCReason(Reason, ctx);

  if (checkObjCInForeignClassContext(SD, Reason))
    return false;

  if (!SD->hasInterfaceType()) {
    SD->getASTContext().getLazyResolver()->resolveDeclSignature(
                                              const_cast<SubscriptDecl *>(SD));
  }

  // Figure out the type of the indices.
  auto SubscriptType = SD->getInterfaceType()->getAs<AnyFunctionType>();
  if (!SubscriptType)
    return false;

  if (SubscriptType->getParams().size() != 1)
    return false;

  Type IndicesType = SubscriptType->getParams()[0].getOldType();
  if (IndicesType->hasError())
    return false;

  bool IndicesResult =
    IndicesType->isRepresentableIn(ForeignLanguage::ObjectiveC,
                                   SD->getDeclContext());

  Type ElementType = SD->getElementInterfaceType();
  bool ElementResult = ElementType->isRepresentableIn(
        ForeignLanguage::ObjectiveC, SD->getDeclContext());
  bool Result = IndicesResult && ElementResult;

  if (Result && checkObjCInExtensionContext(SD, Diagnose))
    return false;

  if (!Diagnose || Result)
    return Result;

  SourceRange TypeRange;
  if (!IndicesResult)
    TypeRange = SD->getIndices()->getSourceRange();
  else
    TypeRange = SD->getElementTypeLoc().getSourceRange();
  SD->diagnose(diag::objc_invalid_on_subscript,
               getObjCDiagnosticAttrKind(Reason))
    .highlight(TypeRange);

  diagnoseTypeNotRepresentableInObjC(SD->getDeclContext(),
                                     !IndicesResult ? IndicesType
                                                    : ElementType,
                                     TypeRange);
  describeObjCReason(SD, Reason);

  return Result;
}

bool swift::canBeRepresentedInObjC(const ValueDecl *decl) {
  ASTContext &ctx = decl->getASTContext();
  if (!ctx.LangOpts.EnableObjCInterop)
    return false;

  if (auto func = dyn_cast<AbstractFunctionDecl>(decl)) {
    Optional<ForeignErrorConvention> errorConvention;
    return isRepresentableInObjC(func, ObjCReason::MemberOfObjCMembersClass,
                                 errorConvention);
  }

  if (auto var = dyn_cast<VarDecl>(decl))
    return isRepresentableInObjC(var, ObjCReason::MemberOfObjCMembersClass);

  if (auto subscript = dyn_cast<SubscriptDecl>(decl))
    return isRepresentableInObjC(subscript,
                                 ObjCReason::MemberOfObjCMembersClass);

  return false;
}

static Type getObjectiveCNominalType(Type &cache,
                                     Identifier ModuleName,
                                     Identifier TypeName,
                                     DeclContext *dc) {
  if (cache)
    return cache;

  // FIXME: Does not respect visibility of the module.
  ASTContext &ctx = dc->getASTContext();
  ModuleDecl *module = ctx.getLoadedModule(ModuleName);
  if (!module)
    return nullptr;

  SmallVector<ValueDecl *, 4> decls;
  NLOptions options = NL_QualifiedDefault | NL_OnlyTypes;
  dc->lookupQualified(module, TypeName, options, decls);
  for (auto decl : decls) {
    if (auto nominal = dyn_cast<NominalTypeDecl>(decl)) {
      cache = nominal->getDeclaredType();
      return cache;
    }
  }

  return nullptr;
}

#pragma mark Objective-C-specific types

Type TypeChecker::getNSObjectType(DeclContext *dc) {
  return getObjectiveCNominalType(NSObjectType, Context.Id_ObjectiveC,
                                Context.getSwiftId(
                                  KnownFoundationEntity::NSObject),
                                dc);
}

Type TypeChecker::getObjCSelectorType(DeclContext *dc) {
  return getObjectiveCNominalType(ObjCSelectorType,
                                  Context.Id_ObjectiveC,
                                  Context.Id_Selector,
                                  dc);
}

#pragma mark Bridging support

/// Check runtime functions responsible for implicit bridging of Objective-C
/// types.
static void checkObjCBridgingFunctions(ModuleDecl *mod,
                                       StringRef bridgedTypeName,
                                       StringRef forwardConversion,
                                       StringRef reverseConversion) {
  assert(mod);
  ModuleDecl::AccessPathTy unscopedAccess = {};
  SmallVector<ValueDecl *, 4> results;

  auto &ctx = mod->getASTContext();
  mod->lookupValue(unscopedAccess, ctx.getIdentifier(bridgedTypeName),
                   NLKind::QualifiedLookup, results);
  mod->lookupValue(unscopedAccess, ctx.getIdentifier(forwardConversion),
                   NLKind::QualifiedLookup, results);
  mod->lookupValue(unscopedAccess, ctx.getIdentifier(reverseConversion),
                   NLKind::QualifiedLookup, results);

  for (auto D : results) {
    if (!D->hasInterfaceType()) {
      auto resolver = ctx.getLazyResolver();
      assert(resolver);
      resolver->resolveDeclSignature(D);
    }
  }
}

void swift::checkBridgedFunctions(ASTContext &ctx) {
  #define BRIDGE_TYPE(BRIDGED_MOD, BRIDGED_TYPE, _, NATIVE_TYPE, OPT) \
  Identifier ID_##BRIDGED_MOD = ctx.getIdentifier(#BRIDGED_MOD);\
  if (ModuleDecl *module = ctx.getLoadedModule(ID_##BRIDGED_MOD)) {\
    checkObjCBridgingFunctions(module, #BRIDGED_TYPE, \
    "_convert" #BRIDGED_TYPE "To" #NATIVE_TYPE, \
    "_convert" #NATIVE_TYPE "To" #BRIDGED_TYPE); \
  }
  #include "swift/SIL/BridgedTypes.def"

  if (ModuleDecl *module = ctx.getLoadedModule(ctx.Id_Foundation)) {
    checkObjCBridgingFunctions(module,
                               ctx.getSwiftName(
                                 KnownFoundationEntity::NSError),
                               "_convertNSErrorToError",
                               "_convertErrorToNSError");
  }
}

#pragma mark "@objc declaration handling"

/// Whether this declaration is a member of a class extension marked @objc.
static bool isMemberOfObjCClassExtension(const ValueDecl *VD) {
  auto ext = dyn_cast<ExtensionDecl>(VD->getDeclContext());
  if (!ext) return false;

  return ext->getSelfClassDecl() && ext->getAttrs().hasAttribute<ObjCAttr>();
}

/// Whether this declaration is a member of a class with the `@objcMembers`
/// attribute.
static bool isMemberOfObjCMembersClass(const ValueDecl *VD) {
  auto classDecl = VD->getDeclContext()->getSelfClassDecl();
  if (!classDecl) return false;

  return classDecl->getAttrs().hasAttribute<ObjCMembersAttr>();
}

// A class is @objc if it does not have generic ancestry, and it either has
// an explicit @objc attribute, or its superclass is @objc.
static Optional<ObjCReason> shouldMarkClassAsObjC(const ClassDecl *CD) {
  ASTContext &ctx = CD->getASTContext();
  ObjCClassKind kind = CD->checkObjCAncestry();

  if (auto attr = CD->getAttrs().getAttribute<ObjCAttr>()) {
    if (kind == ObjCClassKind::ObjCMembers) {
      if (attr->hasName() && !CD->isGenericContext()) {
        // @objc with a name on a non-generic subclass of a generic class is
        // just controlling the runtime name. Don't diagnose this case.
        const_cast<ClassDecl *>(CD)->getAttrs().add(
          new (ctx) ObjCRuntimeNameAttr(*attr));
        return None;
      }

      ctx.Diags.diagnose(attr->getLocation(), diag::objc_for_generic_class)
        .fixItRemove(attr->getRangeWithAt());
    }

    // Only allow ObjC-rooted classes to be @objc.
    // (Leave a hole for test cases.)
    if (kind == ObjCClassKind::ObjCWithSwiftRoot) {
      if (ctx.LangOpts.EnableObjCAttrRequiresFoundation)
        ctx.Diags.diagnose(attr->getLocation(),
                           diag::invalid_objc_swift_rooted_class)
          .fixItRemove(attr->getRangeWithAt());
      if (!ctx.LangOpts.EnableObjCInterop)
        ctx.Diags.diagnose(attr->getLocation(), diag::objc_interop_disabled)
          .fixItRemove(attr->getRangeWithAt());
    }

    return ObjCReason(ObjCReason::ExplicitlyObjC);
  }

  if (kind == ObjCClassKind::ObjCWithSwiftRoot ||
      kind == ObjCClassKind::ObjC)
    return ObjCReason(ObjCReason::ImplicitlyObjC);

  return None;
}

/// Figure out if a declaration should be exported to Objective-C.
Optional<ObjCReason> shouldMarkAsObjC(const ValueDecl *VD, bool allowImplicit) {
  // If Objective-C interoperability is disabled, nothing gets marked as @objc.
  if (!VD->getASTContext().LangOpts.EnableObjCInterop)
    return None;

  if (auto classDecl = dyn_cast<ClassDecl>(VD)) {
    return shouldMarkClassAsObjC(classDecl);
  }

  // Destructors are always @objc, with -dealloc as their entry point.
  if (isa<DestructorDecl>(VD))
    return ObjCReason(ObjCReason::ImplicitlyObjC);

  ProtocolDecl *protocolContext =
      dyn_cast<ProtocolDecl>(VD->getDeclContext());
  bool isMemberOfObjCProtocol =
      protocolContext && protocolContext->isObjC();

  // Local function to determine whether we can implicitly infer @objc.
  auto canInferImplicitObjC = [&] {
    if (VD->isInvalid())
      return false;
    if (VD->isOperator())
      return false;

    // Implicitly generated declarations are not @objc, except for constructors.
    if (!allowImplicit && VD->isImplicit())
      return false;

    if (VD->getFormalAccess() <= AccessLevel::FilePrivate)
      return false;

    return true;
  };

  // explicitly declared @objc.
  if (VD->getAttrs().hasAttribute<ObjCAttr>())
    return ObjCReason(ObjCReason::ExplicitlyObjC);
  // Getter or setter for an @objc property or subscript.
  if (auto accessor = dyn_cast<AccessorDecl>(VD)) {
    if (accessor->getAccessorKind() == AccessorKind::Get ||
        accessor->getAccessorKind() == AccessorKind::Set) {
      if (accessor->getStorage()->isObjC())
        return ObjCReason(ObjCReason::Accessor);

      return None;
    }
  }
  // @IBOutlet, @IBAction, @NSManaged, and @GKInspectable imply @objc.
  //
  // @IBInspectable and @GKInspectable imply @objc quietly in Swift 3
  // (where they warn on failure) and loudly in Swift 4 (error on failure).
  if (VD->getAttrs().hasAttribute<IBOutletAttr>())
    return ObjCReason(ObjCReason::ExplicitlyIBOutlet);
  if (VD->getAttrs().hasAttribute<IBActionAttr>())
    return ObjCReason(ObjCReason::ExplicitlyIBAction);
  if (VD->getAttrs().hasAttribute<IBInspectableAttr>())
    return ObjCReason(ObjCReason::ExplicitlyIBInspectable);
  if (VD->getAttrs().hasAttribute<GKInspectableAttr>())
    return ObjCReason(ObjCReason::ExplicitlyGKInspectable);
  if (VD->getAttrs().hasAttribute<NSManagedAttr>())
    return ObjCReason(ObjCReason::ExplicitlyNSManaged);
  // A member of an @objc protocol is implicitly @objc.
  if (isMemberOfObjCProtocol)
    return ObjCReason(ObjCReason::MemberOfObjCProtocol);
  // A @nonobjc is not @objc, even if it is an override of an @objc, so check
  // for @nonobjc first.
  if (VD->getAttrs().hasAttribute<NonObjCAttr>() ||
      (isa<ExtensionDecl>(VD->getDeclContext()) &&
       cast<ExtensionDecl>(VD->getDeclContext())->getAttrs()
        .hasAttribute<NonObjCAttr>()))
    return None;
  if (isMemberOfObjCClassExtension(VD))
    return ObjCReason(ObjCReason::MemberOfObjCExtension);
  if (isMemberOfObjCMembersClass(VD) && canInferImplicitObjC())
    return ObjCReason(ObjCReason::MemberOfObjCMembersClass);
  // An override of an @objc declaration is implicitly @objc.
  if (VD->getOverriddenDecl() && VD->getOverriddenDecl()->isObjC())
    return ObjCReason(ObjCReason::OverridesObjC);
  // A witness to an @objc protocol requirement is implicitly @objc.
  if (VD->getDeclContext()->getSelfClassDecl()) {
    auto requirements =
      findWitnessedObjCRequirements(VD, /*anySingleRequirement=*/true);
    if (!requirements.empty())
      return ObjCReason::witnessToObjC(requirements.front());
  }

  ASTContext &ctx = VD->getASTContext();

  // Under Swift 3's @objc inference rules, 'dynamic' infers '@objc'.
  if (auto attr = VD->getAttrs().getAttribute<DynamicAttr>()) {
    bool isGetterOrSetter =
      isa<AccessorDecl>(VD) && cast<AccessorDecl>(VD)->isGetterOrSetter();

    if (ctx.LangOpts.EnableSwift3ObjCInference) {
      // If we've been asked to warn about deprecated @objc inference, do so
      // now.
      if (ctx.LangOpts.WarnSwift3ObjCInference !=
            Swift3ObjCInferenceWarnings::None &&
          !isGetterOrSetter) {
        VD->diagnose(diag::objc_inference_swift3_dynamic)
          .highlight(attr->getLocation())
          .fixItInsert(VD->getAttributeInsertionLoc(/*forModifier=*/false),
                      "@objc ");
      }

      return ObjCReason(ObjCReason::ExplicitlyDynamic);
    }
    if (!ctx.isSwiftVersionAtLeast(5)) {
      // Complain that 'dynamic' requires '@objc', but (quietly) infer @objc
      // anyway for better recovery.
      VD->diagnose(diag::dynamic_requires_objc, VD->getDescriptiveKind(),
                   VD->getFullName())
          .fixItInsert(VD->getAttributeInsertionLoc(/*forModifier=*/false),
                       "@objc ");
      return ObjCReason(ObjCReason::ImplicitlyObjC);
    }
  }

  // If we aren't provided Swift 3's @objc inference rules, we're done.
  if (!ctx.LangOpts.EnableSwift3ObjCInference)
    return None;

  // Infer '@objc' for valid, non-implicit, non-operator, members of classes
  // (and extensions thereof) whose class hierarchies originate in Objective-C,
  // e.g., which derive from NSObject, so long as the members have internal
  // access or greater.
  if (!canInferImplicitObjC())
    return None;

  // If this declaration is part of a class with implicitly @objc members,
  // make it implicitly @objc. However, if the declaration cannot be represented
  // as @objc, don't diagnose.
  if (auto classDecl = VD->getDeclContext()->getSelfClassDecl()) {
    // One cannot define @objc members of any foreign classes.
    if (classDecl->isForeign())
      return None;

    if (classDecl->checkObjCAncestry() != ObjCClassKind::NonObjC) {
      return ObjCReason(ObjCReason::MemberOfObjCSubclass);
    }
  }

  return None;
}

/// Determine whether the given type is a C integer type.
static bool isCIntegerType(Type type) {
  auto nominal = type->getAnyNominal();
  if (!nominal) return false;

  ASTContext &ctx = nominal->getASTContext();
  auto stdlibModule = ctx.getStdlibModule();
  if (nominal->getParentModule() != stdlibModule)
    return false;

  // Check for each of the C integer type equivalents in the standard library.
  auto matchesStdlibTypeNamed = [&](StringRef name) {
    auto identifier = ctx.getIdentifier(name);
    SmallVector<ValueDecl *, 2> foundDecls;
    stdlibModule->lookupValue({ }, identifier, NLKind::UnqualifiedLookup,
                              foundDecls);
    for (auto found : foundDecls) {
      auto foundType = dyn_cast<TypeDecl>(found);
      if (!foundType) continue;

      if (!foundType->hasInterfaceType()) {
        auto resolver = ctx.getLazyResolver();
        assert(resolver);
        resolver->resolveDeclSignature(foundType);
      }

      if (foundType->getDeclaredInterfaceType()->isEqual(type))
        return true;
    }

    return false;
  };

#define MAP_BUILTIN_TYPE(_, __)
#define MAP_BUILTIN_INTEGER_TYPE(CLANG_BUILTIN_KIND, SWIFT_TYPE_NAME) \
  if (matchesStdlibTypeNamed(#SWIFT_TYPE_NAME))                       \
    return true;
#include "swift/ClangImporter/BuiltinMappedTypes.def"

  return false;
}

/// Determine whether the given enum should be @objc.
static bool isEnumObjC(EnumDecl *enumDecl) {
  // FIXME: Use shouldMarkAsObjC once it loses it's TypeChecker argument.

  // If there is no @objc attribute, it's not @objc.
  if (!enumDecl->getAttrs().hasAttribute<ObjCAttr>())
    return false;

  Type rawType = enumDecl->getRawType();

  // @objc enums must have a raw type.
  if (!rawType) {
    enumDecl->diagnose(diag::objc_enum_no_raw_type);
    return false;
  }

  // If the raw type contains an error, we've already diagnosed it.
  if (rawType->hasError())
    return false;

  // The raw type must be one of the C integer types.
  if (!isCIntegerType(rawType)) {
    SourceRange errorRange;
    if (!enumDecl->getInherited().empty())
      errorRange = enumDecl->getInherited().front().getSourceRange();
    enumDecl->diagnose(diag::objc_enum_raw_type_not_integer, rawType)
      .highlight(errorRange);
    return false;
  }

  return true;
}

/// Record that a declaration is @objc.
static void markAsObjC(ValueDecl *D, ObjCReason reason,
                       Optional<ForeignErrorConvention> errorConvention);


llvm::Expected<bool>
IsObjCRequest::evaluate(Evaluator &evaluator, ValueDecl *VD) const {
  auto dc = VD->getDeclContext();
  Optional<ObjCReason> isObjC;
  if (dc->getSelfClassDecl() && !isa<TypeDecl>(VD)) {
    // Members of classes can be @objc.
    isObjC = shouldMarkAsObjC(VD, isa<ConstructorDecl>(VD));
  }
  else if (isa<ClassDecl>(VD)) {
    // Classes can be @objc.

    // Protocols and enums can also be @objc, but this is covered by the
    // isObjC() check a the beginning.;
    isObjC = shouldMarkAsObjC(VD, /*allowImplicit=*/false);
  } else if (auto enumDecl = dyn_cast<EnumDecl>(VD)) {
    // Enums can be @objc so long as they have a raw type that is representable
    // as an arithmetic type in C.
    if (isEnumObjC(enumDecl))
      isObjC = ObjCReason(ObjCReason::ExplicitlyObjC);
  } else if (auto enumElement = dyn_cast<EnumElementDecl>(VD)) {
    // Enum elements can be @objc so long as the containing enum is @objc.
    if (enumElement->getParentEnum()->isObjC()) {
      if (enumElement->getAttrs().hasAttribute<ObjCAttr>())
        isObjC = ObjCReason::ExplicitlyObjC;
      else
        isObjC = ObjCReason::ElementOfObjCEnum;
    }
  } else if (auto proto = dyn_cast<ProtocolDecl>(VD)) {
    if (proto->getAttrs().hasAttribute<ObjCAttr>()) {
      isObjC = ObjCReason(ObjCReason::ExplicitlyObjC);

      // If the protocol is @objc, it may only refine other @objc protocols.
      // FIXME: Revisit this restriction.
      for (auto inherited : proto->getInheritedProtocols()) {
        if (!inherited->isObjC()) {
          proto->diagnose(diag::objc_protocol_inherits_non_objc_protocol,
                          proto->getDeclaredType(),
                          inherited->getDeclaredType());
          inherited->diagnose(diag::kind_identifier_declared_here,
                              DescriptiveDeclKind::Protocol,
                              inherited->getName());
          isObjC = None;
        }
      }
    }
  } else if (isa<ProtocolDecl>(dc) && cast<ProtocolDecl>(dc)->isObjC()) {
    // Members of @objc protocols are @objc.
    isObjC = shouldMarkAsObjC(VD, isa<ConstructorDecl>(VD));
  } else {
    // Cannot be @objc.
  }

  // Perform some icky stateful hackery to mark this declaration as
  // not being @objc.
  auto makeNotObjC = [&] {
    if (auto objcAttr = VD->getAttrs().getAttribute<ObjCAttr>()) {
      objcAttr->setInvalid();
    }
  };

  // If this declaration should not be exposed to Objective-C, we're done.
  if (!isObjC) {
    makeNotObjC();
    return false;
  }

  if (auto accessor = dyn_cast<AccessorDecl>(VD)) {
    auto storage = accessor->getStorage();
    if (auto storageObjCAttr = storage->getAttrs().getAttribute<ObjCAttr>()) {
      // If @objc on the storage declaration was inferred using a
      // deprecated rule, but this accessor is @objc in its own right,
      // complain.
      ASTContext &ctx = dc->getASTContext();
      if (storageObjCAttr && storageObjCAttr->isSwift3Inferred() &&
          shouldDiagnoseObjCReason(*isObjC, ctx)) {
        storage->diagnose(diag::accessor_swift3_objc_inference,
                 storage->getDescriptiveKind(), storage->getFullName(),
                 isa<SubscriptDecl>(storage), accessor->isSetter())
          .fixItInsert(storage->getAttributeInsertionLoc(/*forModifier=*/false),
                       "@objc ");
      }
    }
  }

  // If needed, check whether this declaration is representable in Objective-C.
  Optional<ForeignErrorConvention> errorConvention;
  if (auto var = dyn_cast<VarDecl>(VD)) {
    if (!isRepresentableInObjC(var, *isObjC)) {
      makeNotObjC();
      return false;
    }
  } else if (auto subscript = dyn_cast<SubscriptDecl>(VD)) {
    if (!isRepresentableInObjC(subscript, *isObjC)) {
      makeNotObjC();
      return false;
    }
  } else if (isa<DestructorDecl>(VD)) {
    // Destructors need no additional checking.
  } else if (auto func = dyn_cast<AbstractFunctionDecl>(VD)) {
    if (!isRepresentableInObjC(func, *isObjC, errorConvention)) {
      makeNotObjC();
      return false;
    }
  }

  // Note that this declaration is exposed to Objective-C.
  markAsObjC(VD, *isObjC, errorConvention);

  return true;
}

/// Infer the Objective-C name for a given declaration.
static ObjCSelector inferObjCName(ValueDecl *decl) {
  if (auto destructor = dyn_cast<DestructorDecl>(decl))
    return destructor->getObjCSelector();

  auto attr = decl->getAttrs().getAttribute<ObjCAttr>();

  /// Set the @objc name.
  ASTContext &ctx = decl->getASTContext();
  auto setObjCName = [&](ObjCSelector selector) {
    // If there already is an @objc attribute, update its name.
    if (attr) {
      const_cast<ObjCAttr *>(attr)->setName(selector, /*implicit=*/true);
      return;
    }

    // Otherwise, create an @objc attribute with the implicit name.
    attr = ObjCAttr::create(ctx, selector, /*implicitName=*/true);
    decl->getAttrs().add(attr);
  };

  // If this declaration overrides an @objc declaration, use its name.
  if (auto overridden = decl->getOverriddenDecl()) {
    if (overridden->isObjC()) {
      // Handle methods first.
      if (auto overriddenFunc = dyn_cast<AbstractFunctionDecl>(overridden)) {
        // Determine the selector of the overridden method.
        ObjCSelector overriddenSelector = overriddenFunc->getObjCSelector();

        // Determine whether there is a name conflict.
        bool shouldFixName = !attr || !attr->hasName();
        if (attr && attr->hasName() && *attr->getName() != overriddenSelector) {
          // If the user explicitly wrote the incorrect name, complain.
          if (!attr->isNameImplicit()) {
            {
              auto diag = ctx.Diags.diagnose(
                            attr->AtLoc,
                            diag::objc_override_method_selector_mismatch,
                            *attr->getName(), overriddenSelector);
              fixDeclarationObjCName(diag, decl, overriddenSelector);
            }

            overriddenFunc->diagnose(diag::overridden_here);
          }

          shouldFixName = true;
        }

        // If we have to set the name, do so.
        if (shouldFixName) {
          // Override the name on the attribute.
          setObjCName(overriddenSelector);
        }
        return overriddenSelector;
      }

      // Handle properties.
      if (auto overriddenProp = dyn_cast<VarDecl>(overridden)) {
        Identifier overriddenName = overriddenProp->getObjCPropertyName();
        ObjCSelector overriddenNameAsSel(ctx, 0, overriddenName);

        // Determine whether there is a name conflict.
        bool shouldFixName = !attr || !attr->hasName();
        if (attr && attr->hasName() &&
            *attr->getName() != overriddenNameAsSel) {
          // If the user explicitly wrote the wrong name, complain.
          if (!attr->isNameImplicit()) {
            ctx.Diags.diagnose(attr->AtLoc,
                        diag::objc_override_property_name_mismatch,
                        attr->getName()->getSelectorPieces()[0],
                        overriddenName)
              .fixItReplaceChars(attr->getNameLocs().front(),
                                 attr->getRParenLoc(),
                                 overriddenName.str());
            overridden->diagnose(diag::overridden_here);
          }

          shouldFixName = true;
        }

        // Fix the name, if needed.
        if (shouldFixName) {
          setObjCName(overriddenNameAsSel);
        }
        return overriddenNameAsSel;
      }
    }
  }

  // If the decl already has a name, do nothing; the protocol conformance
  // checker will handle any mismatches.
  if (attr && attr->hasName())
    return *attr->getName();

  // When no override determined the Objective-C name, look for
  // requirements for which this declaration is a witness.
  Optional<ObjCSelector> requirementObjCName;
  ValueDecl *firstReq = nullptr;
  for (auto req : findWitnessedObjCRequirements(decl)) {
    // If this is the first requirement, take its name.
    if (!requirementObjCName) {
      requirementObjCName = req->getObjCRuntimeName();
      firstReq = req;
      continue;
    }

    // If this requirement has a different name from one we've seen,
    // note the ambiguity.
    if (*requirementObjCName != *req->getObjCRuntimeName()) {
      decl->diagnose(diag::objc_ambiguous_inference,
                     decl->getDescriptiveKind(), decl->getFullName(),
                     *requirementObjCName, *req->getObjCRuntimeName());

      // Note the candidates and what Objective-C names they provide.
      auto diagnoseCandidate = [&](ValueDecl *req) {
        auto proto = cast<ProtocolDecl>(req->getDeclContext());
        auto diag = decl->diagnose(diag::objc_ambiguous_inference_candidate,
                                   req->getFullName(),
                                   proto->getFullName(),
                                   *req->getObjCRuntimeName());
        fixDeclarationObjCName(diag, decl, req->getObjCRuntimeName());
      };
      diagnoseCandidate(firstReq);
      diagnoseCandidate(req);

      // Suggest '@nonobjc' to suppress this error, and not try to
      // infer @objc for anything.
      decl->diagnose(diag::req_near_match_nonobjc, true)
        .fixItInsert(decl->getAttributeInsertionLoc(false), "@nonobjc ");
      break;
    }
  }

  // If we have a name, install it via an @objc attribute.
  if (requirementObjCName) {
    setObjCName(*requirementObjCName);
    return *requirementObjCName;
  }

  return *decl->getObjCRuntimeName(true);
}

/// Mark the given declaration as being Objective-C compatible (or
/// not) as appropriate.
///
/// If the declaration has a @nonobjc attribute, diagnose an error
/// using the given Reason, if present.
void markAsObjC(ValueDecl *D, ObjCReason reason,
                Optional<ForeignErrorConvention> errorConvention) {
  ASTContext &ctx = D->getASTContext();

  // By now, the caller will have handled the case where an implicit @objc
  // could be overridden by @nonobjc. If we see a @nonobjc and we are trying
  // to add an @objc for whatever reason, diagnose an error.
  if (auto *attr = D->getAttrs().getAttribute<NonObjCAttr>()) {
    if (!shouldDiagnoseObjCReason(reason, ctx))
      reason = ObjCReason::ImplicitlyObjC;

    D->diagnose(diag::nonobjc_not_allowed,
                getObjCDiagnosticAttrKind(reason));

    attr->setInvalid();
  }

  if (!isa<TypeDecl>(D) && !D->hasInterfaceType()) {
    ctx.getLazyResolver()->resolveDeclSignature(D);
  }

  if (!isa<TypeDecl>(D) && !isa<AccessorDecl>(D) && !isa<EnumElementDecl>(D)) {
    if (ctx.getLazyResolver()) {
      // Only record conformances when we have a lazy resolver.
      useObjectiveCBridgeableConformances(D->getInnermostDeclContext(),
                                          D->getInterfaceType());
    }
  }

  if (auto method = dyn_cast<AbstractFunctionDecl>(D)) {
    // Determine the foreign error convention.
    if (auto baseMethod = method->getOverriddenDecl()) {
      // If the overridden method has a foreign error convention,
      // adopt it.  Set the foreign error convention for a throwing
      // method.  Note that the foreign error convention affects the
      // selector, so we perform this before inferring a selector.
      if (method->hasThrows()) {
        if (auto baseErrorConvention
              = baseMethod->getForeignErrorConvention()) {
          errorConvention = baseErrorConvention;
        }

        assert(errorConvention && "Missing error convention");
        method->setForeignErrorConvention(*errorConvention);
      }
    } else if (method->hasThrows()) {
      // Attach the foreign error convention.
      assert(errorConvention && "Missing error convention");
      method->setForeignErrorConvention(*errorConvention);
    }

    // Infer the Objective-C name for this method.
    auto selector = inferObjCName(method);

    // Swift does not permit class methods with Objective-C selectors 'load',
    // 'alloc', or 'allocWithZone:'. Check for these cases.
    if (!method->isInstanceMember()) {
      auto isForbiddenSelector = [&](ObjCSelector sel)
      -> Optional<Diag<unsigned, DeclName, ObjCSelector>> {
        switch (sel.getNumArgs()) {
        case 0:
          if (sel.getSelectorPieces().front() == ctx.Id_load ||
              sel.getSelectorPieces().front() == ctx.Id_alloc)
            return diag::objc_class_method_not_permitted;
          // Swift 3 and earlier allowed you to override `initialize`, but
          // Swift's semantics do not guarantee that it will be called at
          // the point you expect. It is disallowed in Swift 4 and later.
          if (sel.getSelectorPieces().front() == ctx.Id_initialize)
            return diag::objc_class_method_not_permitted;
          return None;
        case 1:
          if (sel.getSelectorPieces().front() == ctx.Id_allocWithZone)
            return diag::objc_class_method_not_permitted;
          return None;
        default:
          return None;
        }
      };
      if (auto diagID = isForbiddenSelector(selector)) {
        auto diagInfo = getObjCMethodDiagInfo(method);
        method->diagnose(*diagID, diagInfo.first, diagInfo.second, selector);
      }
    }

    // Record the method in the class, if it's a member of one.
    if (auto classDecl = D->getDeclContext()->getSelfClassDecl()) {
      classDecl->recordObjCMethod(method, selector);
    }

    // Record the method in the source file.
    if (auto sourceFile = method->getParentSourceFile()) {
      sourceFile->ObjCMethods[selector].push_back(method);
    }
  } else if (isa<VarDecl>(D)) {
    // Infer the Objective-C name for this property.
    (void)inferObjCName(D);

    // FIXME: We should have a class-based table to check for conflicts.
  }

  // Special handling for Swift 3 @objc inference rules that are no longer
  // present in later versions of Swift.
  if (reason == ObjCReason::MemberOfObjCSubclass) {
    // If we've been asked to unconditionally warn about these deprecated
    // @objc inference rules, do so now. However, we don't warn about
    // accessors---just the main storage declarations.
    if (ctx.LangOpts.WarnSwift3ObjCInference ==
          Swift3ObjCInferenceWarnings::Complete &&
        !(isa<AccessorDecl>(D) && cast<AccessorDecl>(D)->isGetterOrSetter())) {
      D->diagnose(diag::objc_inference_swift3_objc_derived);
      D->diagnose(diag::objc_inference_swift3_addobjc)
        .fixItInsert(D->getAttributeInsertionLoc(/*forModifier=*/false),
                     "@objc ");
      D->diagnose(diag::objc_inference_swift3_addnonobjc)
        .fixItInsert(D->getAttributeInsertionLoc(/*forModifier=*/false),
                     "@nonobjc ");
    }

    // Mark the attribute as having used Swift 3 inference, or create an
    // implicit @objc for that purpose.
    auto attr = D->getAttrs().getAttribute<ObjCAttr>();
    if (!attr) {
      attr = ObjCAttr::createUnnamedImplicit(ctx);
      D->getAttrs().add(attr);
    }
    attr->setSwift3Inferred();
  }
}

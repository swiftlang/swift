//===--- SwiftNameTranslation.cpp - Swift to ObjC Name Translation APIs ---===//
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
//  This file contains utilities for translating Swift names to ObjC.
//
//===----------------------------------------------------------------------===//

#include "swift/AST/SwiftNameTranslation.h"
#include "swift/AST/ASTContext.h"
#include "swift/AST/Decl.h"
#include "swift/AST/DiagnosticsSema.h"
#include "swift/AST/LazyResolver.h"
#include "swift/AST/Module.h"
#include "swift/AST/ParameterList.h"
#include "swift/AST/Type.h"
#include "swift/AST/Types.h"
#include "swift/Basic/Assertions.h"
#include "swift/Basic/StringExtras.h"

#include "clang/AST/DeclObjC.h"
#include "llvm/ADT/SmallString.h"
#include <optional>

using namespace swift;

StringRef swift::objc_translation::
getNameForObjC(const ValueDecl *VD, CustomNamesOnly_t customNamesOnly) {
  assert(isa<ClassDecl>(VD) || isa<ProtocolDecl>(VD) || isa<StructDecl>(VD) ||
         isa<EnumDecl>(VD) || isa<EnumElementDecl>(VD) ||
         isa<TypeAliasDecl>(VD));
  if (auto objc = VD->getAttrs().getAttribute<ObjCAttr>()) {
    if (auto name = objc->getName()) {
      assert(name->getNumSelectorPieces() == 1);
      return name->getSelectorPieces().front().str();
    }
  }

  if (customNamesOnly)
    return StringRef();

  if (auto clangDecl = dyn_cast_or_null<clang::NamedDecl>(VD->getClangDecl())) {
    if (const clang::IdentifierInfo *II = clangDecl->getIdentifier())
      return II->getName();
    if (auto *anonDecl = dyn_cast<clang::TagDecl>(clangDecl))
      if (auto *anonTypedef = anonDecl->getTypedefNameForAnonDecl())
        return anonTypedef->getIdentifier()->getName();
  }

  return VD->getBaseIdentifier().str();
}

std::string swift::objc_translation::
getErrorDomainStringForObjC(const EnumDecl *ED) {
  // Should have already been diagnosed as diag::objc_enum_generic.
  assert(!ED->isGenericContext() && "Trying to bridge generic enum error to Obj-C");

  SmallVector<const NominalTypeDecl *, 4> outerTypes;
  for (const NominalTypeDecl * D = ED;
       D != nullptr;
       D = D->getDeclContext()->getSelfNominalTypeDecl()) {
    // We don't currently PrintAsClang any types whose parents are private or
    // fileprivate.
    assert(D->getFormalAccess() >= AccessLevel::Internal &&
            "We don't currently append private discriminators");
    outerTypes.push_back(D);
  }

  std::string buffer = ED->getParentModule()->getNameStr().str();
  for (auto D : llvm::reverse(outerTypes)) {
    buffer += ".";
    buffer += D->getNameStr();
  }

  return buffer;
}

bool swift::objc_translation::
printSwiftEnumElemNameInObjC(const EnumElementDecl *EL, llvm::raw_ostream &OS,
                             Identifier PreferredName) {
  StringRef ElemName = getNameForObjC(EL, CustomNamesOnly);
  if (!ElemName.empty()) {
    OS << ElemName;
    return true;
  }
  OS << getNameForObjC(EL->getDeclContext()->getSelfEnumDecl());
  if (PreferredName.empty())
    ElemName = EL->getBaseIdentifier().str();
  else
    ElemName = PreferredName.str();

  SmallString<64> Scratch;
  OS << camel_case::toSentencecase(ElemName, Scratch);
  return false;
}

std::pair<Identifier, ObjCSelector> swift::objc_translation::
getObjCNameForSwiftDecl(const ValueDecl *VD, DeclName PreferredName){
  ASTContext &Ctx = VD->getASTContext();
  Identifier BaseName;
  if (PreferredName) {
    auto BaseNameStr = PreferredName.getBaseName().userFacingName();
    BaseName = Ctx.getIdentifier(BaseNameStr);
  }
  if (auto *FD = dyn_cast<AbstractFunctionDecl>(VD)) {
    return {Identifier(), FD->getObjCSelector(PreferredName)};
  } else if (auto *VAD = dyn_cast<VarDecl>(VD)) {
    if (PreferredName)
      return {BaseName, ObjCSelector()};
    return {VAD->getObjCPropertyName(), ObjCSelector()};
  } else if (auto *SD = dyn_cast<SubscriptDecl>(VD)) {
    return getObjCNameForSwiftDecl(SD->getParsedAccessor(AccessorKind::Get),
                                   PreferredName);
  } else if (auto *EL = dyn_cast<EnumElementDecl>(VD)) {
    SmallString<64> Buffer;
    {
      llvm::raw_svector_ostream OS(Buffer);
      printSwiftEnumElemNameInObjC(EL, OS, BaseName);
    }
    return {Ctx.getIdentifier(Buffer.str()), ObjCSelector()};
  } else {
    // @objc(ExplicitName) > PreferredName > Swift name.
    StringRef Name = getNameForObjC(VD, CustomNamesOnly);
    if (!Name.empty())
      return {Ctx.getIdentifier(Name), ObjCSelector()};
    if (PreferredName)
      return {BaseName, ObjCSelector()};
    return {Ctx.getIdentifier(getNameForObjC(VD)), ObjCSelector()};
  }
}

bool swift::objc_translation::
isVisibleToObjC(const ValueDecl *VD, AccessLevel minRequiredAccess,
                bool checkParent) {
  if (!(VD->isObjC() || !VD->getCDeclName().empty()))
    return false;
  if (VD->getFormalAccess() >= minRequiredAccess) {
    return true;
  } else if (checkParent) {
    if (auto ctor = dyn_cast<ConstructorDecl>(VD)) {
      // Check if we're overriding an initializer that is visible to obj-c
      if (auto parent = ctor->getOverriddenDecl())
        return isVisibleToObjC(parent, minRequiredAccess, false);
    }
  }
  return false;
}

StringRef
swift::cxx_translation::getNameForCxx(const ValueDecl *VD,
                                      CustomNamesOnly_t customNamesOnly) {
  ASTContext& ctx = VD->getASTContext();

  for (auto *EA : VD->getAttrs().getAttributes<ExposeAttr>()) {
    if (EA->getExposureKind() == ExposureKind::Cxx && !EA->Name.empty())
      return EA->Name;
  }

  if (customNamesOnly)
    return StringRef();

  if (isa<ConstructorDecl>(VD))
    return "init";

  if (VD->isOperator()) {
    std::string name = ("operator" + VD->getBaseIdentifier().str()).str();
    return ctx.getIdentifier(name).str();
  }

  if (auto *mod = dyn_cast<ModuleDecl>(VD)) {
    if (mod->isStdlibModule())
      return "swift";
  }
  if (VD->getModuleContext()->isStdlibModule()) {
    // Incorporate argument labels into Stdlib API names.
    // FIXME: This should be done more broadly.
    if (auto *AFD = dyn_cast<AbstractFunctionDecl>(VD)) {
      std::string result;
      llvm::raw_string_ostream os(result);
      os << VD->getBaseIdentifier().str();
      if (!AFD->getParameters())
        return os.str();
      for (const auto *param : *AFD->getParameters()) {
        auto paramName = param->getArgumentName();
        if (paramName.empty())
          continue;
        auto paramNameStr = paramName.str();
        os << char(std::toupper(paramNameStr[0]));
        os << paramNameStr.drop_front(1);
      }
      auto r = ctx.getIdentifier(os.str());
      return r.str();
    }

    // FIXME: String.Index should be exposed as String::Index, not
    // _String_Index.
    if (VD->getBaseIdentifier().str() == "Index") {
      return "String_Index";
    }
  }

  return VD->getBaseIdentifier().str();
}

swift::cxx_translation::DeclRepresentation
swift::cxx_translation::getDeclRepresentation(
    const ValueDecl *VD,
    std::optional<std::function<bool(const NominalTypeDecl *)>> isZeroSized) {
  if (getActorIsolation(const_cast<ValueDecl *>(VD)).isActorIsolated())
    return {Unsupported, UnrepresentableIsolatedInActor};
  if (isa<MacroDecl>(VD))
    return {Unsupported, UnrepresentableMacro};
  GenericSignature genericSignature;
  // Don't expose @_alwaysEmitIntoClient decls as they require their
  // bodies to be emitted into client.
  if (VD->getAttrs().hasAttribute<AlwaysEmitIntoClientAttr>())
    return {Unsupported, UnrepresentableRequiresClientEmission};
  if (auto *AFD = dyn_cast<AbstractFunctionDecl>(VD)) {
    if (AFD->hasAsync())
      return {Unsupported, UnrepresentableAsync};
    if (AFD->hasThrows() &&
        !AFD->getASTContext().LangOpts.hasFeature(
            Feature::GenerateBindingsForThrowingFunctionsInCXX))
      return {Unsupported, UnrepresentableThrows};
    if (AFD->isGeneric())
      genericSignature = AFD->getGenericSignature();
  }
  if (const auto *typeDecl = dyn_cast<NominalTypeDecl>(VD)) {
    if (isa<ProtocolDecl>(typeDecl)) {
      if (typeDecl->hasClangNode())
        return {ObjCxxOnly, std::nullopt};
      return {Unsupported, UnrepresentableProtocol};
    }
    // Swift's consume semantics are not yet supported in C++.
    if (!typeDecl->canBeCopyable())
      return {Unsupported, UnrepresentableMoveOnly};
    if (isa<ClassDecl>(VD) && VD->isObjC())
      return {Unsupported, UnrepresentableObjC};
    if (typeDecl->isGeneric()) {
      if (isa<ClassDecl>(VD))
        return {Unsupported, UnrepresentableGeneric};
      genericSignature = typeDecl->getGenericSignature();
    }
    // Nested classes are not yet supported.
    if (isa<ClassDecl>(VD) && !typeDecl->hasClangNode() &&
        isa_and_nonnull<NominalTypeDecl>(
            typeDecl->getDeclContext()->getAsDecl()))
      return {Unsupported, UnrepresentableNested};
    if (!isa<ClassDecl>(typeDecl) && isZeroSized && (*isZeroSized)(typeDecl))
      return {Unsupported, UnrepresentableZeroSizedValueType};
  }
  if (const auto *varDecl = dyn_cast<VarDecl>(VD)) {
    // Check if any property accessor throws, do not expose it in that case.
    for (const auto *accessor : varDecl->getAllAccessors()) {
      if (accessor->hasThrows())
        return {Unsupported, UnrepresentableThrows};
    }
  }
  if (const auto *enumDecl = dyn_cast<EnumDecl>(VD)) {
    if (enumDecl->isIndirect())
      return {Unsupported, UnrepresentableIndirectEnum};
    for (const auto *enumCase : enumDecl->getAllCases()) {
      for (const auto *elementDecl : enumCase->getElements()) {
        if (!elementDecl->hasAssociatedValues())
          continue;
        if (elementDecl->isIndirect())
          return {Unsupported, UnrepresentableIndirectEnum};
        // Do not expose any enums with > 1
        // enum parameter, or any enum parameter
        // whose type we do not yet support.
        if (auto *params = elementDecl->getParameterList()) {
          if (params->size() > 1)
            return {Unsupported, UnrepresentableEnumCaseTuple};
          for (const auto *param : *params) {
            auto paramType = param->getInterfaceType();
            if (!paramType->is<GenericTypeParamType>()) {
              auto *nominal = paramType->getNominalOrBoundGenericNominal();
              if (!nominal || isa<ProtocolDecl>(nominal))
                return {Unsupported, UnrepresentableEnumCaseType};
            }
          }
        }
      }
    }
  }

  // Generic requirements are not yet supported in C++.
  if (!isExposableToCxx(genericSignature)) {
    return {Unsupported, UnrepresentableGenericRequirements};
  }

  return {Representable, std::nullopt};
}

bool swift::cxx_translation::isVisibleToCxx(const ValueDecl *VD,
                                            AccessLevel minRequiredAccess,
                                            bool checkParent) {
  // Do not expose anything from _Concurrency module yet.
  if (VD->getModuleContext()->ValueDecl::getName().getBaseIdentifier() ==
      VD->getASTContext().Id_Concurrency)
    return false;
  if (VD->getFormalAccess() >= minRequiredAccess) {
    return true;
  } else if (checkParent) {
    if (auto ctor = dyn_cast<ConstructorDecl>(VD)) {
      // Check if we're overriding an initializer that is visible to obj-c
      if (auto parent = ctor->getOverriddenDecl())
        return isVisibleToCxx(parent, minRequiredAccess, false);
    }
  }
  return false;
}

bool swift::cxx_translation::isExposableToCxx(GenericSignature genericSig) {
  // If there's no generic signature, it's fine.
  if (!genericSig)
    return true;

  // FIXME: This should use getRequirements() and actually
  // support arbitrary requirements. We don't really want
  // to use getRequirementsWithInverses() here.
  //
  // For now, we use the inverse transform as a quick way to
  // check for the "default" generic signature where each
  // generic parameter is Copyable and Escapable, but not
  // subject to any other requirements; that's exactly the
  // generic signature that C++ interop supports today.
  SmallVector<Requirement, 2> reqs;
  SmallVector<InverseRequirement, 2> inverseReqs;
  genericSig->getRequirementsWithInverses(reqs, inverseReqs);
  if (!reqs.empty()) {
    // Conformance requirements to marker protocols are okay.
    for (const auto &req: reqs) {
      if (req.getKind() != RequirementKind::Conformance)
        return false;

      auto proto = req.getProtocolDecl();
      if (!proto->isMarkerProtocol() && !proto->hasClangNode())
        return false;
    }
  }

  // Allow Copyable and Escapable.
  for (const auto &req: inverseReqs) {
    switch (req.getKind()) {
    case InvertibleProtocolKind::Copyable:
      continue;

    case InvertibleProtocolKind::Escapable:
      continue;
    }

    return false;
  }

  return true;
}

Diagnostic
swift::cxx_translation::diagnoseRepresenationError(RepresentationError error,
                                                   ValueDecl *vd) {
  switch (error) {
  case UnrepresentableObjC:
    return Diagnostic(diag::expose_unsupported_objc_decl_to_cxx, vd);
  case UnrepresentableAsync:
    return Diagnostic(diag::expose_unsupported_async_decl_to_cxx, vd);
  case UnrepresentableIsolatedInActor:
    return Diagnostic(diag::expose_unsupported_actor_isolated_to_cxx, vd);
  case UnrepresentableRequiresClientEmission:
    return Diagnostic(diag::expose_unsupported_client_emission_to_cxx, vd);
  case UnrepresentableGeneric:
    return Diagnostic(diag::expose_generic_decl_to_cxx, vd);
  case UnrepresentableGenericRequirements:
    return Diagnostic(diag::expose_generic_requirement_to_cxx, vd);
  case UnrepresentableThrows:
    return Diagnostic(diag::expose_throwing_to_cxx, vd);
  case UnrepresentableIndirectEnum:
    return Diagnostic(diag::expose_indirect_enum_cxx, vd);
  case UnrepresentableEnumCaseType:
    return Diagnostic(diag::expose_enum_case_type_to_cxx, vd);
  case UnrepresentableEnumCaseTuple:
    return Diagnostic(diag::expose_enum_case_tuple_to_cxx, vd);
  case UnrepresentableProtocol:
    return Diagnostic(diag::expose_protocol_to_cxx_unsupported, vd);
  case UnrepresentableMoveOnly:
    return Diagnostic(diag::expose_move_only_to_cxx, vd);
  case UnrepresentableNested:
    return Diagnostic(diag::expose_nested_type_to_cxx, vd);
  case UnrepresentableMacro:
    return Diagnostic(diag::expose_macro_to_cxx, vd);
  case UnrepresentableZeroSizedValueType:
    return Diagnostic(diag::expose_zero_size_to_cxx, vd);
  }
}

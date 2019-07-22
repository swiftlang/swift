//===----------------------------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2019 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#include "swift/AST/ASTPrinter.h"
#include "swift/AST/Decl.h"
#include "swift/AST/NameLookup.h"
#include "swift/Basic/SourceManager.h"
#include "swift/Frontend/Frontend.h"
#include "swift/Sema/IDETypeCheckingRequests.h"
#include "swift/Subsystems.h"
#include "TypeChecker.h"

using namespace swift;

namespace swift {
// Implement the IDE type zone.
#define SWIFT_TYPEID_ZONE SWIFT_IDE_TYPE_CHECK_REQUESTS_TYPEID_ZONE
#define SWIFT_TYPEID_HEADER "swift/Sema/IDETypeCheckingRequestIDZone.def"
#include "swift/Basic/ImplementTypeIDZone.h"
#undef SWIFT_TYPEID_ZONE
#undef SWIFT_TYPEID_HEADER
}

// Define request evaluation functions for each of the IDE type check requests.
static AbstractRequestFunction *ideTypeCheckRequestFunctions[] = {
#define SWIFT_TYPEID(Name)                                    \
reinterpret_cast<AbstractRequestFunction *>(&Name::evaluateRequest),
#include "swift/Sema/IDETypeCheckingRequestIDZone.def"
#undef SWIFT_TYPEID
};

void swift::registerIDETypeCheckRequestFunctions(Evaluator &evaluator) {
  evaluator.registerRequestFunctions(SWIFT_IDE_TYPE_CHECK_REQUESTS_TYPEID_ZONE,
                                     ideTypeCheckRequestFunctions);
}

static bool isExtensionAppliedInternal(const DeclContext *DC, Type BaseTy,
                                       const ExtensionDecl *ED) {
  // We can't do anything if the base type has unbound generic parameters.
  // We can't leak type variables into another constraint system.
  if (BaseTy->hasTypeVariable() || BaseTy->hasUnboundGenericType() ||
      BaseTy->hasUnresolvedType() || BaseTy->hasError())
    return true;

  if (!ED->isConstrainedExtension())
    return true;

  TypeChecker *TC = &TypeChecker::createForContext((DC->getASTContext()));
  TC->validateExtension(const_cast<ExtensionDecl *>(ED));

  GenericSignature *genericSig = ED->getGenericSignature();
  SubstitutionMap substMap = BaseTy->getContextSubstitutionMap(
      DC->getParentModule(), ED->getExtendedNominal());
  return areGenericRequirementsSatisfied(DC, genericSig, substMap,
                                         /*isExtension=*/true);
}

static bool isMemberDeclAppliedInternal(const DeclContext *DC, Type BaseTy,
                                        const ValueDecl *VD) {
  // We can't leak type variables into another constraint system.
  // We can't do anything if the base type has unbound generic parameters.
  if (BaseTy->hasTypeVariable() || BaseTy->hasUnboundGenericType()||
      BaseTy->hasUnresolvedType() || BaseTy->hasError())
    return true;

  const GenericContext *genericDecl = VD->getAsGenericContext();
  if (!genericDecl)
    return true;
  const GenericSignature *genericSig = genericDecl->getGenericSignature();
  if (!genericSig)
    return true;

  SubstitutionMap substMap = BaseTy->getContextSubstitutionMap(
      DC->getParentModule(), VD->getDeclContext());
  return areGenericRequirementsSatisfied(DC, genericSig, substMap,
                                         /*isExtension=*/false);
}

llvm::Expected<bool>
IsDeclApplicableRequest::evaluate(Evaluator &evaluator,
                                  DeclApplicabilityOwner Owner) const {
  if (auto *VD = dyn_cast<ValueDecl>(Owner.ExtensionOrMember)) {
    return isMemberDeclAppliedInternal(Owner.DC, Owner.Ty, VD);
  } else if (auto *ED = dyn_cast<ExtensionDecl>(Owner.ExtensionOrMember)) {
    return isExtensionAppliedInternal(Owner.DC, Owner.Ty, ED);
  } else {
    llvm_unreachable("unhandled decl kind");
  }
}

//===--- IDETypeChecking.cpp ----------------------------------------------===//
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

#include "swift/AST/ASTContext.h"
#include "swift/AST/Identifier.h"
#include "swift/AST/Decl.h"
#include "swift/AST/Types.h"
#include "swift/Sema/IDETypeChecking.h"

using namespace swift;

static Type getContextFreeInterfaceType(ValueDecl *VD) {
  if (auto AFD = dyn_cast<AbstractFunctionDecl>(VD)) {
    return AFD->getMethodInterfaceType();
  }
  return VD->getInterfaceType();
}

ArrayRef<ValueDecl*> swift::
canDeclProvideDefaultImplementationFor(ValueDecl* VD,
                                llvm::SmallVectorImpl<ValueDecl*> &Scractch) {

  // Skip decls that don't have valid names.
  if (!VD->getFullName())
    return {};

  // Check if VD is from a protocol extension.
  auto P = VD->getDeclContext()->getAsProtocolExtensionContext();
  if (!P)
    return {};

  // Look up all decls in the protocol's inheritance chain for the ones with
  // the same name with VD.
  ResolvedMemberResult LookupResult =
    resolveValueMember(*P->getInnermostDeclContext(),
                       P->getDeclaredInterfaceType(), VD->getFullName());

  auto VDType = getContextFreeInterfaceType(VD);
  for (auto Mem : LookupResult.getMemberDecls(InterestedMemberKind::All)) {
    if (auto Pro = dyn_cast<ProtocolDecl>(Mem->getDeclContext())) {
      if (Mem->isProtocolRequirement() &&
          getContextFreeInterfaceType(Mem)->isEqual(VDType)) {
        // We find a protocol requirement VD can provide default
        // implementation for.
        Scractch.push_back(Mem);
      }
    }
  }
  return Scractch;
}

void swift::
collectDefaultImplementationForProtocolMembers(ProtocolDecl *PD,
                    llvm::SmallDenseMap<ValueDecl*, ValueDecl*> &DefaultMap) {
  Type BaseTy = PD->getDeclaredInterfaceType();
  DeclContext *DC = PD->getInnermostDeclContext();
  auto HandleMembers = [&](DeclRange Members) {
    for (Decl *D : Members) {
      ValueDecl *VD = dyn_cast<ValueDecl>(D);

      // Skip non-value decl.
      if (!VD)
        continue;

      // Skip decls with empty names, e.g. setter/getters for properties.
      if (VD->getName().empty())
        continue;

      ResolvedMemberResult Result = resolveValueMember(*DC, BaseTy,
                                                       VD->getFullName());
      assert(Result);
      for (auto *Default : Result.getMemberDecls(InterestedMemberKind::All)) {
        if (PD == Default->getDeclContext()->getAsProtocolExtensionContext()) {
          DefaultMap.insert({Default, VD});
        }
      }
    }
  };

  // Collect the default implementations for the members in this given protocol.
  HandleMembers(PD->getMembers());

  // Collect the default implementations for the members in the inherited
  // protocols.
  for (auto* IP : PD->getInheritedProtocols())
    HandleMembers(IP->getMembers());
}

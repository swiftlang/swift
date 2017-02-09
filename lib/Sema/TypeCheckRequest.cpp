//===--- TypeCheckRequest.cpp - Type Checking Request ---------------------===//
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
//  This file implements the TypeCheckRequest type, which describes a
//  request to the type checker to compute certain information.
//
//===----------------------------------------------------------------------===//
#include "swift/Sema/TypeCheckRequest.h"
#include "swift/AST/Decl.h"

using namespace swift;

SourceLoc TypeCheckRequest::getLoc() const {
  switch (getPayloadKind(getKind())) {
#define DELEGATE_GET_LOC(PayloadName)             \
  case PayloadKind::PayloadName:                  \
    return get##PayloadName##Payload()->getLoc();

  DELEGATE_GET_LOC(Class)
  DELEGATE_GET_LOC(Enum)

  case PayloadKind::InheritedClauseEntry: {
    auto payload = getInheritedClauseEntryPayload();
    if (auto typeDecl = payload.first.dyn_cast<TypeDecl *>())
      return typeDecl->getInherited()[payload.second].getLoc();

    return payload.first.get<ExtensionDecl *>()->getInherited()[payload.second]
             .getLoc();
  }

  DELEGATE_GET_LOC(Protocol)

  case PayloadKind::DeclContextLookup:
    return getDeclContextLookupPayload().Loc;

  case PayloadKind::TypeResolution:
    return std::get<0>(getTypeResolutionPayload())->getLoc();

  DELEGATE_GET_LOC(TypeDeclResolution)

#undef DELEGATE_GET_LOC
  }

  llvm_unreachable("Unhandled PayloadKind in switch.");
}

Decl *TypeCheckRequest::getAnchor() const {
  switch (getPayloadKind(getKind())) {
#define DECL_PAYLOAD(PayloadName)               \
  case PayloadKind::PayloadName:                \
    return get##PayloadName##Payload();

#define NO_DECL_PAYLOAD(PayloadName)            \
  case PayloadKind::PayloadName:                \
    return nullptr;

  DECL_PAYLOAD(Class)
  DECL_PAYLOAD(Enum)

  case PayloadKind::InheritedClauseEntry: {
    auto payload = getInheritedClauseEntryPayload();
    if (auto typeDecl = payload.first.dyn_cast<TypeDecl *>())
      return typeDecl;

    return payload.first.get<ExtensionDecl *>();
  }

  DECL_PAYLOAD(Protocol)

  case PayloadKind::DeclContextLookup: {
    auto dc = getDeclContextLookupPayload().DC;
    if (auto nominal = dyn_cast<NominalTypeDecl>(dc))
      return nominal;
    if (auto ext = dyn_cast<ExtensionDecl>(dc))
      return ext;
    return nullptr;
  }

  NO_DECL_PAYLOAD(TypeResolution)
  DECL_PAYLOAD(TypeDeclResolution)

#undef NO_DECL_PAYLOAD
#undef DECL_PAYLOAD
  }

  llvm_unreachable("Unhandled PayloadKind in switch.");
}

bool swift::operator==(const TypeCheckRequest &x, const TypeCheckRequest &y) {
  // Check the request kinds.
  if (x.getKind() != y.getKind()) return false;

  // Check the payload.
  switch (TypeCheckRequest::getPayloadKind(x.getKind())) {
#define TYPE_CHECK_REQUEST_PAYLOAD(PayloadName,...)                     \
  case TypeCheckRequest::PayloadKind::PayloadName:                      \
    return x.get##PayloadName##Payload() == y.get##PayloadName##Payload();
#include "swift/Sema/TypeCheckRequestPayloads.def"
  }

  llvm_unreachable("Unhandled PayloadKind in switch.");
}

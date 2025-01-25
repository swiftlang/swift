//===--- Bridging/TypeAttributeBridging.cpp -------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2022-2024 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#include "swift/AST/ASTBridging.h"

#include "swift/AST/ASTContext.h"
#include "swift/AST/Attr.h"
#include "swift/AST/Identifier.h"
#include "swift/Basic/Assertions.h"

using namespace swift;

//===----------------------------------------------------------------------===//
// MARK: TypeAttributes
//===----------------------------------------------------------------------===//

BridgedTypeAttrKind BridgedTypeAttrKind_fromString(BridgedStringRef cStr) {
  auto optKind = TypeAttribute::getAttrKindFromString(cStr.unbridged());
  if (!optKind)
    return BridgedTypeAttrKindNone;
  switch (*optKind) {
#define TYPE_ATTR(_, CLASS)                                                    \
  case TypeAttrKind::CLASS:                                                    \
    return BridgedTypeAttrKind##CLASS;
#include "swift/AST/TypeAttr.def"
  }
}

static std::optional<TypeAttrKind> unbridged(BridgedTypeAttrKind kind) {
  switch (kind) {
#define TYPE_ATTR(_, CLASS)                                                    \
  case BridgedTypeAttrKind##CLASS:                                             \
    return TypeAttrKind::CLASS;
#include "swift/AST/TypeAttr.def"
  case BridgedTypeAttrKindNone:
    return std::nullopt;
  }
  llvm_unreachable("unhandled enum value");
}

BridgedTypeAttributes BridgedTypeAttributes_create() {
  return new TypeAttributes();
}

void BridgedTypeAttributes_delete(BridgedTypeAttributes cAttributes) {
  delete cAttributes.unbridged();
}

void BridgedTypeAttributes_add(BridgedTypeAttributes cAttributes,
                               BridgedTypeAttribute cAttribute) {
  cAttributes.unbridged()->attrs.push_back(cAttribute.unbridged());
}

bool BridgedTypeAttributes_isEmpty(BridgedTypeAttributes cAttributes) {
  TypeAttributes *typeAttributes = cAttributes.unbridged();
  return typeAttributes->attrs.empty();
}

BridgedTypeAttribute BridgedTypeAttribute_createSimple(
    BridgedASTContext cContext, BridgedTypeAttrKind cKind,
    BridgedSourceLoc cAtLoc, BridgedSourceLoc cNameLoc) {
  auto optKind = unbridged(cKind);
  assert(optKind && "creating attribute of invalid kind?");
  return TypeAttribute::createSimple(cContext.unbridged(), *optKind,
                                     cAtLoc.unbridged(), cNameLoc.unbridged());
}

BridgedTypeAttribute BridgedTypeAttribute_createIsolated(
    BridgedASTContext cContext, BridgedSourceLoc cAtLoc,
    BridgedSourceLoc cNameLoc, BridgedSourceLoc cLPLoc,
    BridgedSourceLoc cIsolationLoc,
    BridgedIsolatedTypeAttrIsolationKind cIsolation, BridgedSourceLoc cRPLoc) {
  auto isolationKind = [=] {
    switch (cIsolation) {
    case BridgedIsolatedTypeAttrIsolationKind_DynamicIsolation:
      return IsolatedTypeAttr::IsolationKind::Dynamic;
    }
    llvm_unreachable("bad kind");
  }();
  return new (cContext.unbridged())
      IsolatedTypeAttr(cAtLoc.unbridged(), cNameLoc.unbridged(),
                       {cLPLoc.unbridged(), cRPLoc.unbridged()},
                       {isolationKind, cIsolationLoc.unbridged()});
}

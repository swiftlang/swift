//===--- Bridging/TypeAttributeBridging.cpp -------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2022-2025 Apple Inc. and the Swift project authors
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

// Define `.asTypeAttr` on each BridgedXXXTypeAttr type.
#define SIMPLE_TYPE_ATTR(...)
#define TYPE_ATTR(SPELLING, CLASS)                                             \
  SWIFT_NAME("getter:Bridged" #CLASS "TypeAttr.asTypeAttribute(self:)")        \
  BridgedTypeAttribute Bridged##CLASS##TypeAttr_asTypeAttribute(               \
      Bridged##CLASS##TypeAttr attr) {                                         \
    return attr.unbridged();                                                   \
  }
#include "swift/AST/TypeAttr.def"

BridgedOptionalTypeAttrKind
BridgedOptionalTypeAttrKind_fromString(BridgedStringRef cStr) {
  auto optKind = TypeAttribute::getAttrKindFromString(cStr.unbridged());
  if (!optKind) {
    return BridgedOptionalTypeAttrKind();
  }
  return *optKind;
}

BridgedTypeAttribute
BridgedTypeAttribute_createSimple(BridgedASTContext cContext,
                                  swift::TypeAttrKind kind, SourceLoc atLoc,
                                  SourceLoc nameLoc) {
  return TypeAttribute::createSimple(cContext.unbridged(), kind, atLoc,
                                     nameLoc);
}

BridgedConventionTypeAttr BridgedConventionTypeAttr_createParsed(
    BridgedASTContext cContext, SourceLoc atLoc, SourceLoc kwLoc,
    SourceRange parens, BridgedStringRef cName, SourceLoc nameLoc,
    BridgedDeclNameRef cWitnessMethodProtocol, BridgedStringRef cClangType,
    SourceLoc clangTypeLoc) {
  return new (cContext.unbridged())
      ConventionTypeAttr(atLoc, kwLoc, parens, {cName.unbridged(), nameLoc},
                         cWitnessMethodProtocol.unbridged(),
                         {cClangType.unbridged(), clangTypeLoc});
}

BridgedDifferentiableTypeAttr BridgedDifferentiableTypeAttr_createParsed(
    BridgedASTContext cContext, SourceLoc atLoc, SourceLoc nameLoc,
    SourceRange parensRange, BridgedDifferentiabilityKind cKind,
    SourceLoc kindLoc) {
  return new (cContext.unbridged()) DifferentiableTypeAttr(
      atLoc, nameLoc, parensRange, {unbridged(cKind), kindLoc});
}

BridgedIsolatedTypeAttr BridgedIsolatedTypeAttr_createParsed(
    BridgedASTContext cContext, SourceLoc atLoc, SourceLoc nameLoc,
    SourceRange parensRange, BridgedIsolatedTypeAttrIsolationKind cIsolation,
    SourceLoc isolationLoc) {
  auto isolationKind = [=] {
    switch (cIsolation) {
    case BridgedIsolatedTypeAttrIsolationKind_DynamicIsolation:
      return IsolatedTypeAttr::IsolationKind::Dynamic;
    }
    llvm_unreachable("bad kind");
  }();
  return new (cContext.unbridged()) IsolatedTypeAttr(
      atLoc, nameLoc, parensRange, {isolationKind, isolationLoc});
}

BridgedOpaqueReturnTypeOfTypeAttr
BridgedOpaqueReturnTypeOfTypeAttr_createParsed(
    BridgedASTContext cContext, SourceLoc atLoc, SourceLoc kwLoc,
    SourceRange parens, BridgedStringRef cMangled, SourceLoc mangledLoc,
    size_t index, SourceLoc indexLoc) {
  return new (cContext.unbridged()) OpaqueReturnTypeOfTypeAttr(
      atLoc, kwLoc, parens, {cMangled.unbridged(), mangledLoc},
      {static_cast<unsigned int>(index), indexLoc});
}

//===--- ASTBridgingImpl.h - header for the swift ASTBridging module ------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2023 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_AST_ASTBRIDGINGIMPL_H
#define SWIFT_AST_ASTBRIDGINGIMPL_H

#include "swift/AST/Decl.h"

SWIFT_BEGIN_NULLABILITY_ANNOTATIONS

//===----------------------------------------------------------------------===//
// MARK: BridgedASTContext
//===----------------------------------------------------------------------===//

void * _Nonnull BridgedASTContext_raw(BridgedASTContext bridged) {
  return &bridged.unbridged();
}

BridgedASTContext BridgedASTContext_fromRaw(void * _Nonnull ptr) {
  return *static_cast<swift::ASTContext *>(ptr);
}

//===----------------------------------------------------------------------===//
// MARK: BridgedNominalTypeDecl
//===----------------------------------------------------------------------===//

BridgedStringRef BridgedNominalTypeDecl_getName(BridgedNominalTypeDecl decl) {
  return decl.unbridged()->getName().str();
}

bool BridgedNominalTypeDecl_isGlobalActor(BridgedNominalTypeDecl decl) {
  return decl.unbridged()->isGlobalActor();
}

//===----------------------------------------------------------------------===//
// MARK: BridgedVarDecl
//===----------------------------------------------------------------------===//

BridgedStringRef BridgedVarDecl_getUserFacingName(BridgedVarDecl decl) {
  return decl.unbridged()->getBaseName().userFacingName();
}

SWIFT_END_NULLABILITY_ANNOTATIONS

#endif // SWIFT_AST_ASTBRIDGINGIMPL_H

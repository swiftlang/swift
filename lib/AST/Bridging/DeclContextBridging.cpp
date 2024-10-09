//===--- Bridging/DeclContextBridging.cpp ---------------------------------===//
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

#include "swift/AST/DeclContext.h"

using namespace swift;

//===----------------------------------------------------------------------===//
// MARK: DeclContexts
//===----------------------------------------------------------------------===//

bool BridgedDeclContext_isLocalContext(BridgedDeclContext cDeclContext) {
  return cDeclContext.unbridged()->isLocalContext();
}

BridgedPatternBindingInitializer
BridgedPatternBindingInitializer_create(BridgedDeclContext cDeclContext) {
  return PatternBindingInitializer::create(cDeclContext.unbridged());
}

BridgedDeclContext BridgedPatternBindingInitializer_asDeclContext(
    BridgedPatternBindingInitializer cInit) {
  return cInit.unbridged();
}

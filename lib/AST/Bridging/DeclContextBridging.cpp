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
#include "swift/AST/Expr.h"

using namespace swift;

//===----------------------------------------------------------------------===//
// MARK: DeclContexts
//===----------------------------------------------------------------------===//

BridgedPatternBindingInitializer
BridgedPatternBindingInitializer_create(BridgedDeclContext cDeclContext) {
  return PatternBindingInitializer::create(cDeclContext.unbridged());
}

BridgedDeclContext BridgedPatternBindingInitializer_asDeclContext(
    BridgedPatternBindingInitializer cInit) {
  return cInit.unbridged();
}

BridgedDefaultArgumentInitializer
BridgedDefaultArgumentInitializer_create(BridgedDeclContext cDeclContext,
                                         size_t index) {
  return DefaultArgumentInitializer::create(cDeclContext.unbridged(), index);
}

BridgedDeclContext DefaultArgumentInitializer_asDeclContext(
    BridgedDefaultArgumentInitializer cInit) {
  return cInit.unbridged();
}

BridgedCustomAttributeInitializer
BridgedCustomAttributeInitializer_create(BridgedDeclContext cDeclContext) {
  return CustomAttributeInitializer::create(cDeclContext.unbridged());
}

BridgedDeclContext BridgedCustomAttributeInitializer_asDeclContext(
    BridgedCustomAttributeInitializer cInit) {
  return cInit.unbridged();
}

BridgedDeclContext
BridgedClosureExpr_asDeclContext(BridgedClosureExpr cClosure) {
  return cClosure.unbridged();
}

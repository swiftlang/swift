//===--- Effects.cpp - Effect Checking ASTs -------------------------------===//
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
//  This file implements some logic for rethrows and reasync checking.
//
//===----------------------------------------------------------------------===//

#include "swift/AST/ASTContext.h"
#include "swift/AST/Effects.h"
#include "swift/AST/Evaluator.h"
#include "swift/AST/Decl.h"
#include "swift/AST/ProtocolConformanceRef.h"
#include "swift/AST/Type.h"
#include "swift/AST/TypeCheckRequests.h"
#include "llvm/ADT/ArrayRef.h"
#include "llvm/Support/raw_ostream.h"

using namespace swift;

void swift::simple_display(llvm::raw_ostream &out,
                           const ProtocolRethrowsRequirementList list) {
  for (auto entry : list) {
    simple_display(out, entry.first);
    simple_display(out, entry.second);
  }
}

ProtocolRethrowsRequirementList 
ProtocolDecl::getRethrowingRequirements() const {
  return evaluateOrDefault(getASTContext().evaluator,
    ProtocolRethrowsRequirementsRequest{const_cast<ProtocolDecl *>(this)}, 
    ProtocolRethrowsRequirementList());
}

bool ProtocolDecl::isRethrowingProtocol() const {
  return getAttrs().hasAttribute<swift::AtRethrowsAttr>();
}

FunctionRethrowingKind AbstractFunctionDecl::getRethrowingKind() const {
  return evaluateOrDefault(getASTContext().evaluator,
    FunctionRethrowingKindRequest{const_cast<AbstractFunctionDecl *>(this)}, 
    FunctionRethrowingKind::Invalid);
}

void swift::simple_display(llvm::raw_ostream &out,
                           FunctionRethrowingKind kind) {
  switch (kind) {
  case FunctionRethrowingKind::None:
    out << "non-throwing";
    break;
  case FunctionRethrowingKind::ByClosure:
    out << "by closure";
    break;
  case FunctionRethrowingKind::ByConformance:
    out << "by conformance";
    break;
  case FunctionRethrowingKind::Throws:
    out << "throws";
    break;
  case FunctionRethrowingKind::Invalid:
    out << "invalid";
    break;
  }
}

bool ProtocolConformanceRef::classifyAsThrows() const {
  if (!isConcrete()) { return true; }
  return evaluateOrDefault(getRequirement()->getASTContext().evaluator,
     ProtocolConformanceRefClassifyAsThrowsRequest{ *this }, 
     true);
}
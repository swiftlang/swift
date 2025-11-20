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
#include "swift/AST/Types.h"
#include "swift/AST/TypeCheckRequests.h"
#include "llvm/ADT/ArrayRef.h"
#include "llvm/Support/raw_ostream.h"

using namespace swift;

bool AnyFunctionType::hasEffect(EffectKind kind) const {
  switch (kind) {
  case EffectKind::Throws: return getExtInfo().isThrowing();
  case EffectKind::Async: return getExtInfo().isAsync();
  case EffectKind::Unsafe: return false;
  }
  llvm_unreachable("Bad effect kind");
}

void swift::simple_display(llvm::raw_ostream &out, const EffectKind kind) {
  switch (kind) {
  case EffectKind::Throws: out << "throws"; return;
  case EffectKind::Async: out << "async"; return;
  case EffectKind::Unsafe: out << "@unsafe"; return;
  }
  llvm_unreachable("Bad effect kind");
}

void swift::simple_display(llvm::raw_ostream &out,
                           const PolymorphicEffectRequirementList list) {
  for (auto req : list.getRequirements()) {
    simple_display(out, req);
    out << "\n";
  }

  for (auto conf : list.getConformances()) {
    simple_display(out, conf.first);
    out << " : ";
    simple_display(out, conf.second);
    llvm::errs() << "\n";
  }
}

PolymorphicEffectRequirementList 
ProtocolDecl::getPolymorphicEffectRequirements(EffectKind kind) const {
  return evaluateOrDefault(getASTContext().evaluator,
    PolymorphicEffectRequirementsRequest{kind, const_cast<ProtocolDecl *>(this)},
    PolymorphicEffectRequirementList());
}

bool ProtocolDecl::hasPolymorphicEffect(EffectKind kind) const {
  switch (kind) {
  case EffectKind::Throws:
    return getAttrs().hasAttribute<swift::AtRethrowsAttr>();
  case EffectKind::Async:
    return getAttrs().hasAttribute<swift::AtReasyncAttr>();
  case EffectKind::Unsafe:
    return false;
  }
  llvm_unreachable("Bad effect kind");
}

bool AbstractFunctionDecl::hasEffect(EffectKind kind) const {
  switch (kind) {
  case EffectKind::Throws:
    return hasThrows();
  case EffectKind::Async:
    return hasAsync();
  case EffectKind::Unsafe:
    return getExplicitSafety() == ExplicitSafety::Unsafe;
  }
  llvm_unreachable("Bad effect kind");
}

bool AbstractFunctionDecl::hasPolymorphicEffect(EffectKind kind) const {
  switch (kind) {
  case EffectKind::Throws:
    return getAttrs().hasAttribute<swift::RethrowsAttr>();
  case EffectKind::Async:
    return getAttrs().hasAttribute<swift::ReasyncAttr>();
  case EffectKind::Unsafe:
    return false;
  }
  llvm_unreachable("Bad effect kind");
}

PolymorphicEffectKind
AbstractFunctionDecl::getPolymorphicEffectKind(EffectKind kind) const {
  return evaluateOrDefault(getASTContext().evaluator,
    PolymorphicEffectKindRequest{kind, const_cast<AbstractFunctionDecl *>(this)},
    PolymorphicEffectKind::Invalid);
}

void swift::simple_display(llvm::raw_ostream &out,
                           PolymorphicEffectKind kind) {
  switch (kind) {
  case PolymorphicEffectKind::None:
    out << "none";
    break;
  case PolymorphicEffectKind::ByClosure:
    out << "by closure";
    break;
  case PolymorphicEffectKind::ByConformance:
    out << "by conformance";
    break;
  case PolymorphicEffectKind::AsyncSequenceRethrows:
    out << "by async sequence implicit @rethrows";
    break;
  case PolymorphicEffectKind::Always:
    out << "always";
    break;
  case PolymorphicEffectKind::Invalid:
    out << "invalid";
    break;
  }
}

bool ProtocolConformanceRef::hasEffect(EffectKind kind) const {
  if (!isConcrete()) { return kind != EffectKind::Unsafe; }
  return evaluateOrDefault(getProtocol()->getASTContext().evaluator,
     ConformanceHasEffectRequest{kind, getConcrete()},
     true);
}

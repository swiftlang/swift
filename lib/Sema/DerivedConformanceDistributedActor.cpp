//===--- DerivedConformanceActor.cpp - Derived Actor Conformance ----------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2020 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
//  This file implements implicit derivation of the Actor protocol.
//
//===----------------------------------------------------------------------===//

#include "CodeSynthesis.h"
#include "DerivedConformances.h"
#include "TypeChecker.h"
#include "TypeCheckConcurrency.h"
#include "swift/AST/NameLookupRequests.h"
#include "swift/AST/ParameterList.h"

using namespace swift;

bool DerivedConformance::canDeriveDistributedActor(
    NominalTypeDecl *nominal, DeclContext *dc) {
  auto classDecl = dyn_cast<ClassDecl>(nominal);
  return classDecl && classDecl->isDistributedActor() && dc == nominal;
}
// ==== ------------------------------------------------------------------------

ValueDecl *DerivedConformance::deriveDistributedActor(ValueDecl *requirement) {
//  ASTContext &C = ConformanceDecl->getASTContext();
//
//  const auto name = requirement->getName();
//  fprintf(stderr, "[%s:%d] >> (%s) TRY %s \n", __FILE__, __LINE__, __FUNCTION__, name);

  // TODO: turns out this is the wrong place to synthesize things,
  //       and instead we do earlier in the pipeline, since we need to add properties
  //
  //       We may want to remove this file and hooks entirely, depends if we find
  //       something useful to synthesize here.

//  // Synthesize functions
//  auto func = dyn_cast<FuncDecl>(requirement);
//  if (func) {
//    fprintf(stderr, "[%s:%d] >> (%s) function .... \n", __FILE__, __LINE__, __FUNCTION__);
//    // TODO: derive encode impl
//    return nullptr;
//  }

 return nullptr;
}

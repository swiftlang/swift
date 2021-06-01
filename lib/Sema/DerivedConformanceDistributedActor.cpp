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
 return nullptr;
}

//===--- ActorIsolation.cpp -----------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2024 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#include "swift/AST/ActorIsolation.h"
#include "swift/AST/ASTContext.h"

using namespace swift;

ActorIsolation ActorIsolation::forMainActor(ASTContext &ctx) {
  return ActorIsolation::forGlobalActor(
      ctx.getMainActorType()->mapTypeOutOfContext());
}

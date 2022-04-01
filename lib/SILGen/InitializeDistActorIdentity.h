//===--- InitializeDistActorIdentity.h - dist actor ID init for SILGen ----===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2022 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#include "Cleanup.h"

namespace swift {

namespace Lowering {

/// A clean-up designed to emit an initialization of a distributed actor's
/// identity upon successful initialization of the actor's system.
struct InitializeDistActorIdentity : Cleanup {
private:
  ConstructorDecl *ctor;
  ManagedValue actorSelf;
  VarDecl *systemVar;
public:
  InitializeDistActorIdentity(ConstructorDecl *ctor, ManagedValue actorSelf);

  void emit(SILGenFunction &SGF, CleanupLocation loc,
            ForUnwind_t forUnwind) override;

  void dump(SILGenFunction &) const override;

  VarDecl* getSystemVar() const { return systemVar; }
};

} // end Lowering namespace
} // end swift namespace

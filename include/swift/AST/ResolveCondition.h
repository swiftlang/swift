//===--- ResolveCondition.cpp - Resolution of Build Configurations --------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2017 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
///
/// \file
/// This file implements condition resolution for Swift
///
//===----------------------------------------------------------------------===//

#ifndef SWIFT_AST_RESOLVE_CONDITION_H
#define SWIFT_AST_RESOLVE_CONDITION_H

#include "swift/AST/ASTContext.h"
#include "swift/AST/LazyResolver.h"

namespace swift {
  class ConditionResolver : public LazyMemberLoader {
  public:
    virtual void loadAllMembers(Decl *D, uint64_t unused) override;
    void loadBody(Stmt *D);

  private:
    void resolveCondition(IterableDeclContext *IDC,
                          IfConfigDecl *outermostConfig,
                          IfConfigDecl *recur);
  };
};

#endif /* SWIFT_AST_RESOLVE_CONDITION_H */

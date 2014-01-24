//===--- SourceLoader.h - Import .swift files as modules --------*- c++ -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2015 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See http://swift.org/LICENSE.txt for license information
// See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_SEMA_SOURCELOADER_H
#define SWIFT_SEMA_SOURCELOADER_H

#include "swift/AST/ModuleLoader.h"

namespace swift {

class ASTContext;
class Module;
  
/// \brief Imports serialized Swift modules into an ASTContext.
class SourceLoader : public ModuleLoader {
private:
  ASTContext &Ctx;
  bool SkipBodies;

  explicit SourceLoader(ASTContext &ctx, bool skipBodies)
    : Ctx(ctx), SkipBodies(skipBodies) {}

public:
  static SourceLoader *create(ASTContext &ctx, bool skipBodies) {
    return new SourceLoader(ctx, skipBodies);
  }

  SourceLoader(const SourceLoader &) = delete;
  SourceLoader(SourceLoader &&) = delete;
  SourceLoader &operator=(const SourceLoader &) = delete;
  SourceLoader &operator=(SourceLoader &&) = delete;

  virtual Module *
  loadModule(SourceLoc importLoc,
             ArrayRef<std::pair<Identifier, SourceLoc>> path,
             bool isStdlib) override;

  virtual void loadExtensions(NominalTypeDecl *nominal,
                              unsigned previousGeneration) override;
};

}

#endif

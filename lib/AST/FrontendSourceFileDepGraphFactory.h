//===----- FrontendSourceFileDepGraphFactory.h ------------------*- C++ -*-===//
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

#ifndef FrontendSourceFileDepGraphFactory_h
#define FrontendSourceFileDepGraphFactory_h

#include "swift/AST/AbstractSourceFileDepGraphFactory.h"
#include "llvm/Support/VirtualOutputBackend.h"
namespace swift {
namespace fine_grained_dependencies {

/// Constructs a SourceFileDepGraph from a *real* \c SourceFile
/// Reads the information provided by the frontend and builds the
/// SourceFileDepGraph

class FrontendSourceFileDepGraphFactory
    : public AbstractSourceFileDepGraphFactory {
  const SourceFile *SF;
  const DependencyTracker &depTracker;

public:
  FrontendSourceFileDepGraphFactory(const SourceFile *SF,
                                    llvm::vfs::OutputBackend &backend,
                                    StringRef outputPath,
                                    const DependencyTracker &depTracker,
                                    bool alsoEmitDotFile);

  ~FrontendSourceFileDepGraphFactory() override = default;

private:
  void addAllDefinedDecls() override;
  void addAllUsedDecls() override;
};

class ModuleDepGraphFactory : public AbstractSourceFileDepGraphFactory {
  const ModuleDecl *Mod;

public:
  ModuleDepGraphFactory(llvm::vfs::OutputBackend &backend,
                        const ModuleDecl *Mod, bool emitDot);

  ~ModuleDepGraphFactory() override = default;

private:
  void addAllDefinedDecls() override;
  void addAllUsedDecls() override {}
};

} // namespace fine_grained_dependencies
} // namespace swift

#endif /* FrontendSourceFileDepGraphFactory_h */

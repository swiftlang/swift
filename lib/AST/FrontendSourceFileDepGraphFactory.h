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
namespace swift {
namespace fine_grained_dependencies {

/// Constructs a SourceFileDepGraph from a *real* \c SourceFile
/// Reads the information provided by the frontend and builds the
/// SourceFileDepGraph

class FrontendSourceFileDepGraphFactory
    : public AbstractSourceFileDepGraphFactory {
  SourceFile *const SF;
  const DependencyTracker &depTracker;

public:
  FrontendSourceFileDepGraphFactory(SourceFile *SF, StringRef outputPath,
                                    const DependencyTracker &depTracker,
                                    bool alsoEmitDotFile);

  ~FrontendSourceFileDepGraphFactory() override = default;

private:
  static std::string getFingerprint(SourceFile *SF);

  static bool computeIncludePrivateDeps(SourceFile *SF);
  static std::string getInterfaceHash(SourceFile *SF);

  void addAllDefinedDecls() override;
  void addAllUsedDecls() override;

  /// Given an array of Decls or pairs of them in \p declsOrPairs
  /// create node pairs for context and name
  template <NodeKind kind, typename ContentsT>
  void addAllDefinedDeclsOfAGivenType(std::vector<ContentsT> &contentsVec);

  /// At present, only nominals, protocols, and extensions have (body)
  /// fingerprints
  static Optional<std::string>
  getFingerprintIfAny(std::pair<const NominalTypeDecl *, const ValueDecl *>);
  static Optional<std::string> getFingerprintIfAny(const Decl *d);
};

} // namespace fine_grained_dependencies
} // namespace swift

#endif /* FrontendSourceFileDepGraphFactory_h */

//===--- TBDGenRequests.h - TBDGen Requests ---------------------*- C++ -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2020 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
//  This file defines TBDGen requests.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_TBDGEN_REQUESTS_H
#define SWIFT_TBDGEN_REQUESTS_H

#include "swift/AST/ASTTypeIDs.h"
#include "swift/AST/SimpleRequest.h"

namespace llvm {
namespace MachO {
class InterfaceFile;
} // end namespace MachO

template <class AllocatorTy>
class StringSet;
} // end namespace llvm

namespace swift {

class FileUnit;
class ModuleDecl;
struct TBDGenOptions;

class TBDGenDescriptor final {
  using FileOrModule = llvm::PointerUnion<FileUnit *, ModuleDecl *>;
  FileOrModule Input;
  const TBDGenOptions &Opts;

  TBDGenDescriptor(FileOrModule input, const TBDGenOptions &opts)
      : Input(input), Opts(opts) {
    assert(input);
  }

public:
  /// Returns the file or module we're emitting TBD for.
  FileOrModule getFileOrModule() const { return Input; }

  /// If the input is a single file, returns that file. Otherwise returns
  /// \c nullptr.
  FileUnit *getSingleFile() const;

  /// Returns the parent module for TBD emission.
  ModuleDecl *getParentModule() const;

  /// Returns the TBDGen options.
  const TBDGenOptions &getOptions() const { return Opts; }

  bool operator==(const TBDGenDescriptor &other) const;
  bool operator!=(const TBDGenDescriptor &other) const {
    return !(*this == other);
  }

  static TBDGenDescriptor forFile(FileUnit *file, const TBDGenOptions &opts) {
    return TBDGenDescriptor(file, opts);
  }

  static TBDGenDescriptor forModule(ModuleDecl *M, const TBDGenOptions &opts) {
    return TBDGenDescriptor(M, opts);
  }
};

llvm::hash_code hash_value(const TBDGenDescriptor &desc);
void simple_display(llvm::raw_ostream &out, const TBDGenDescriptor &desc);
SourceLoc extractNearestSourceLoc(const TBDGenDescriptor &desc);

using TBDFileAndSymbols =
    std::pair<llvm::MachO::InterfaceFile, llvm::StringSet<>>;

/// Computes the TBD file and public symbols for a given module or file.
class GenerateTBDRequest
    : public SimpleRequest<GenerateTBDRequest,
                           TBDFileAndSymbols(TBDGenDescriptor),
                           CacheKind::Uncached> {
public:
  using SimpleRequest::SimpleRequest;

private:
  friend SimpleRequest;

  // Evaluation.
  TBDFileAndSymbols evaluate(Evaluator &evaluator, TBDGenDescriptor desc) const;
};

/// Report that a request of the given kind is being evaluated, so it
/// can be recorded by the stats reporter.
template <typename Request>
void reportEvaluatedRequest(UnifiedStatsReporter &stats,
                            const Request &request);

/// The zone number for TBDGen.
#define SWIFT_TYPEID_ZONE TBDGen
#define SWIFT_TYPEID_HEADER "swift/AST/TBDGenTypeIDZone.def"
#include "swift/Basic/DefineTypeIDZone.h"
#undef SWIFT_TYPEID_ZONE
#undef SWIFT_TYPEID_HEADER

// Set up reporting of evaluated requests.
#define SWIFT_REQUEST(Zone, RequestType, Sig, Caching, LocOptions)             \
template<>                                                                     \
inline void reportEvaluatedRequest(UnifiedStatsReporter &stats,                \
                                   const RequestType &request) {               \
  ++stats.getFrontendCounters().RequestType;                                   \
}
#include "swift/AST/TBDGenTypeIDZone.def"
#undef SWIFT_REQUEST

} // end namespace swift

#endif // SWIFT_TBDGEN_REQUESTS_H

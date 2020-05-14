//===--- SILGenRequests.h - SILGen Requests ---------------------*- C++ -*-===//
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
//  This file defines SILGen requests.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_SILGEN_REQUESTS_H
#define SWIFT_SILGEN_REQUESTS_H

#include "swift/AST/ASTTypeIDs.h"
#include "swift/AST/EvaluatorDependencies.h"
#include "swift/AST/SimpleRequest.h"

namespace swift {

class FileUnit;
class LangOptions;
class ModuleDecl;
class SILModule;
class SILOptions;
class SourceFile;

namespace Lowering {
  class TypeConverter;
}

/// Report that a request of the given kind is being evaluated, so it
/// can be recorded by the stats reporter.
template<typename Request>
void reportEvaluatedRequest(UnifiedStatsReporter &stats,
                            const Request &request);

struct SILGenDescriptor {
  llvm::PointerUnion<FileUnit *, ModuleDecl *> context;
  Lowering::TypeConverter &conv;
  const SILOptions &opts;

  friend llvm::hash_code hash_value(const SILGenDescriptor &owner) {
    return llvm::hash_combine(owner.context, (void *)&owner.conv,
                              (void *)&owner.opts);
  }

  friend bool operator==(const SILGenDescriptor &lhs,
                         const SILGenDescriptor &rhs) {
    return lhs.context == rhs.context &&
           &lhs.conv == &rhs.conv &&
           &lhs.opts == &rhs.opts;
  }

  friend bool operator!=(const SILGenDescriptor &lhs,
                         const SILGenDescriptor &rhs) {
    return !(lhs == rhs);
  }

public:
  static SILGenDescriptor forFile(FileUnit &sf, Lowering::TypeConverter &conv,
                                  const SILOptions &opts) {
    return SILGenDescriptor{&sf, conv, opts};
  }

  static SILGenDescriptor forWholeModule(ModuleDecl *mod,
                                         Lowering::TypeConverter &conv,
                                         const SILOptions &opts) {
    return SILGenDescriptor{mod, conv, opts};
  }

  /// For a single file input, returns a single element array containing that
  /// file. Otherwise returns an array of each file in the module.
  ArrayRef<FileUnit *> getFiles() const;

  /// If the module or file contains SIL that needs parsing, returns the file
  /// to be parsed, or \c nullptr if parsing isn't required.
  SourceFile *getSourceFileToParse() const;
};

void simple_display(llvm::raw_ostream &out, const SILGenDescriptor &d);

SourceLoc extractNearestSourceLoc(const SILGenDescriptor &desc);

class SILGenerationRequest
    : public SimpleRequest<
          SILGenerationRequest, std::unique_ptr<SILModule>(SILGenDescriptor),
          RequestFlags::Uncached | RequestFlags::DependencySource> {
public:
  using SimpleRequest::SimpleRequest;

private:
  friend SimpleRequest;

  // Evaluation.
  std::unique_ptr<SILModule>
  evaluate(Evaluator &evaluator, SILGenDescriptor desc) const;

public:
  // Incremental dependencies.
  evaluator::DependencySource
  readDependencySource(const evaluator::DependencyCollector &) const;
};

/// Parses a .sil file into a SILModule.
class ParseSILModuleRequest
    : public SimpleRequest<ParseSILModuleRequest,
                           std::unique_ptr<SILModule>(SILGenDescriptor),
                           RequestFlags::Uncached> {
public:
  using SimpleRequest::SimpleRequest;

private:
  friend SimpleRequest;

  // Evaluation.
  std::unique_ptr<SILModule>
  evaluate(Evaluator &evaluator, SILGenDescriptor desc) const;
};

/// The zone number for SILGen.
#define SWIFT_TYPEID_ZONE SILGen
#define SWIFT_TYPEID_HEADER "swift/AST/SILGenTypeIDZone.def"
#include "swift/Basic/DefineTypeIDZone.h"
#undef SWIFT_TYPEID_ZONE
#undef SWIFT_TYPEID_HEADER

 // Set up reporting of evaluated requests.
#define SWIFT_REQUEST(Zone, RequestType, Sig, Caching, LocOptions)             \
template<>                                                                     \
inline void reportEvaluatedRequest(UnifiedStatsReporter &stats,                \
                            const RequestType &request) {                      \
  ++stats.getFrontendCounters().RequestType;                                   \
}
#include "swift/AST/SILGenTypeIDZone.def"
#undef SWIFT_REQUEST

} // end namespace swift

#endif // SWIFT_SILGEN_REQUESTS_H

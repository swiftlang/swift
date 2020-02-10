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
  llvm::PointerUnion<ModuleDecl *, FileUnit *> context;
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
};

void simple_display(llvm::raw_ostream &out, const SILGenDescriptor &d);

SourceLoc extractNearestSourceLoc(const SILGenDescriptor &desc);

class SILGenSourceFileRequest :
    public SimpleRequest<SILGenSourceFileRequest,
                         std::unique_ptr<SILModule>(SILGenDescriptor),
                         CacheKind::Uncached> {
public:
  using SimpleRequest::SimpleRequest;

private:
  friend SimpleRequest;

  // Evaluation.
  llvm::Expected<std::unique_ptr<SILModule>>
  evaluate(Evaluator &evaluator, SILGenDescriptor desc) const;

public:
  bool isCached() const { return true; }
};

class SILGenWholeModuleRequest :
    public SimpleRequest<SILGenWholeModuleRequest,
                         std::unique_ptr<SILModule>(SILGenDescriptor),
                         CacheKind::Uncached> {
public:
  using SimpleRequest::SimpleRequest;

private:
  friend SimpleRequest;

  // Evaluation.
  llvm::Expected<std::unique_ptr<SILModule>>
  evaluate(Evaluator &evaluator, SILGenDescriptor desc) const;

public:
  bool isCached() const { return true; }
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

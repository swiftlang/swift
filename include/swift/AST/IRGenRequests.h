//===--- IRGenRequests.h - IRGen Requests -----------------------*- C++ -*-===//
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
//  This file defines IRGen requests.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_IRGen_REQUESTS_H
#define SWIFT_IRGen_REQUESTS_H

#include "swift/AST/ASTTypeIDs.h"
#include "swift/AST/SimpleRequest.h"
#include "swift/Basic/PrimarySpecificPaths.h"
#include "llvm/ADT/StringSet.h"

namespace swift {
class SourceFile;
class IRGenOptions;
class SILModule;

namespace irgen {
  class IRGenModule;
};

}; // namespace swift

namespace llvm {
class GlobalVariable;
class LLVMContext;
class Module;
}; // namespace llvm

namespace swift {

struct IRGenDescriptor {
  const IRGenOptions &Opts;
  llvm::PointerUnion<ModuleDecl *, SourceFile *> Ctx;
  SILModule *SILMod;
  StringRef ModuleName;
  const PrimarySpecificPaths &PSPs;
  StringRef PrivateDiscriminator;
  llvm::LLVMContext &LLVMContext;
  ArrayRef<std::string> parallelOutputFilenames;
  llvm::GlobalVariable **outModuleHash;
  llvm::StringSet<> *LinkerDirectives;

  friend llvm::hash_code hash_value(const IRGenDescriptor &owner) {
    return llvm::hash_combine(owner.Ctx);
  }

  friend bool operator==(const IRGenDescriptor &lhs,
                         const IRGenDescriptor &rhs) {
    return lhs.Ctx == rhs.Ctx;
  }

  friend bool operator!=(const IRGenDescriptor &lhs,
                         const IRGenDescriptor &rhs) {
    return !(lhs == rhs);
  }

public:
  static IRGenDescriptor
  forFile(const IRGenOptions &Opts, SourceFile &SF,
          std::unique_ptr<SILModule> &&SILMod, StringRef ModuleName,
          const PrimarySpecificPaths &PSPs, StringRef PrivateDiscriminator,
          llvm::LLVMContext &LLVMContext, llvm::GlobalVariable **outModuleHash,
          llvm::StringSet<> *LinkerDirectives) {
    return IRGenDescriptor{Opts,
                           &SF,
                           SILMod.release(),
                           ModuleName,
                           PSPs,
                           PrivateDiscriminator,
                           LLVMContext,
                           {},
                           outModuleHash,
                           LinkerDirectives};
  }

  static IRGenDescriptor
  forWholeModule(const IRGenOptions &Opts, swift::ModuleDecl *M,
                 std::unique_ptr<SILModule> &&SILMod, StringRef ModuleName,
                 const PrimarySpecificPaths &PSPs,
                 llvm::LLVMContext &LLVMContext,
                 ArrayRef<std::string> parallelOutputFilenames,
                 llvm::GlobalVariable **outModuleHash,
                 llvm::StringSet<> *LinkerDirectives) {
    return IRGenDescriptor{Opts,
                           M,
                           SILMod.release(),
                           ModuleName,
                           PSPs,
                           "",
                           LLVMContext,
                           parallelOutputFilenames,
                           outModuleHash,
                           LinkerDirectives};
  }
};

/// Report that a request of the given kind is being evaluated, so it
/// can be recorded by the stats reporter.
template<typename Request>
void reportEvaluatedRequest(UnifiedStatsReporter &stats,
                            const Request &request);

class IRGenSourceFileRequest
    : public SimpleRequest<IRGenSourceFileRequest,
                           std::unique_ptr<llvm::Module>(IRGenDescriptor),
                           CacheKind::Uncached> {
public:
  using SimpleRequest::SimpleRequest;

private:
  friend SimpleRequest;

  // Evaluation.
  std::unique_ptr<llvm::Module>
  evaluate(Evaluator &evaluator, IRGenDescriptor desc) const;

public:
  bool isCached() const { return true; }
};

class IRGenWholeModuleRequest
    : public SimpleRequest<IRGenWholeModuleRequest,
                           std::unique_ptr<llvm::Module>(IRGenDescriptor),
                           CacheKind::Uncached> {
public:
  using SimpleRequest::SimpleRequest;

private:
  friend SimpleRequest;

  // Evaluation.
  std::unique_ptr<llvm::Module>
  evaluate(Evaluator &evaluator, IRGenDescriptor desc) const;

public:
  bool isCached() const { return true; }
};

void simple_display(llvm::raw_ostream &out, const IRGenDescriptor &d);

SourceLoc extractNearestSourceLoc(const IRGenDescriptor &desc);

/// The zone number for IRGen.
#define SWIFT_TYPEID_ZONE IRGen
#define SWIFT_TYPEID_HEADER "swift/AST/IRGenTypeIDZone.def"
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
#include "swift/AST/IRGenTypeIDZone.def"
#undef SWIFT_REQUEST

} // end namespace swift

#endif // SWIFT_IRGen_REQUESTS_H

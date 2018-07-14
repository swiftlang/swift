//===--- CallerAnalysis.cpp - Determine callsites to a function  ----------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2017 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#include "swift/SILOptimizer/Analysis/CallerAnalysis.h"

#include "swift/SIL/SILModule.h"
#include "swift/SILOptimizer/Utils/Local.h"
#include "llvm/Support/FileSystem.h"
#include "llvm/Support/YAMLTraits.h"

using namespace swift;

//===----------------------------------------------------------------------===//
//                              Actual Analysis
//===----------------------------------------------------------------------===//

void CallerAnalysis::processFunctionCallSites(SILFunction *F) {
  // Scan the whole module and search Apply sites.
  for (auto &BB : *F) {
    for (auto &II : BB) {
      if (auto Apply = FullApplySite::isa(&II)) {
        SILFunction *CalleeFn = Apply.getCalleeFunction();
        if (!CalleeFn)
          continue;

        // Update the callee information for this function.
        FunctionInfo &CallerInfo = FuncInfos[F];
        CallerInfo.Callees.insert(CalleeFn);
        
        // Update the callsite information for the callee.
        FunctionInfo &CalleeInfo = FuncInfos[CalleeFn];
        CalleeInfo.Callers.insert(F);
        continue;
      }
      if (auto *PAI = dyn_cast<PartialApplyInst>(&II)) {
        SILFunction *CalleeFn = PAI->getCalleeFunction();
        if (!CalleeFn)
          continue;

        // Update the callee information for this function.
        FunctionInfo &CallerInfo = FuncInfos[F];
        CallerInfo.Callees.insert(CalleeFn);
        
        // Update the partial-apply information for the callee.
        FunctionInfo &CalleeInfo = FuncInfos[CalleeFn];
        int &minAppliedArgs = CalleeInfo.PartialAppliers[F];
        int numArgs = (int)PAI->getNumArguments();
        if (minAppliedArgs == 0 || numArgs < minAppliedArgs) {
          minAppliedArgs = numArgs;
        }
        continue;
      }
    }
  }
}

void CallerAnalysis::invalidateExistingCalleeRelation(SILFunction *F) {
  FunctionInfo &CallerInfo = FuncInfos[F];
  for (auto Callee : CallerInfo.Callees) {
    FunctionInfo &CalleeInfo = FuncInfos[Callee];
    CalleeInfo.Callers.erase(F);
    CalleeInfo.PartialAppliers.erase(F);
  }
}

//===----------------------------------------------------------------------===//
//                         CallerAnalysis YAML Dumper
//===----------------------------------------------------------------------===//

namespace {

using llvm::yaml::IO;
using llvm::yaml::MappingTraits;
using llvm::yaml::Output;
using llvm::yaml::ScalarEnumerationTraits;
using llvm::yaml::SequenceTraits;

/// A special struct that marshals call graph state into a form that is easy for
/// llvm's yaml i/o to dump. Its structure is meant to correspond to how the
/// data should be shown by the printer, so naturally it is slightly redundant.
struct YAMLCallGraphNode {
  StringRef calleeName;
  bool hasCaller;
  unsigned minPartialAppliedArgs;
  std::vector<StringRef> partialAppliers;
  std::vector<StringRef> fullAppliers;

  YAMLCallGraphNode() = delete;
  ~YAMLCallGraphNode() = default;

  // This is a data structure that can not be copied or moved.
  YAMLCallGraphNode(const YAMLCallGraphNode &) = delete;
  YAMLCallGraphNode(YAMLCallGraphNode &&) = delete;
  YAMLCallGraphNode &operator=(const YAMLCallGraphNode &) = delete;
  YAMLCallGraphNode &operator=(YAMLCallGraphNode &&) = delete;

  YAMLCallGraphNode(StringRef calleeName, bool hasCaller,
                    unsigned minPartialAppliedArgs,
                    std::vector<StringRef> &&partialAppliers,
                    std::vector<StringRef> &&fullAppliers)
      : calleeName(calleeName), hasCaller(hasCaller),
        minPartialAppliedArgs(minPartialAppliedArgs),
        partialAppliers(std::move(partialAppliers)),
        fullAppliers(std::move(fullAppliers)) {}
};

} // end anonymous namespace

namespace llvm {
namespace yaml {

template <> struct MappingTraits<YAMLCallGraphNode> {
  static void mapping(IO &io, YAMLCallGraphNode &func) {
    io.mapRequired("calleeName", func.calleeName);
    io.mapRequired("hasCaller", func.hasCaller);
    io.mapRequired("minPartialAppliedArgs", func.minPartialAppliedArgs);
    io.mapRequired("partialAppliers", func.partialAppliers);
    io.mapRequired("fullAppliers", func.fullAppliers);
  }
};

} // namespace yaml
} // namespace llvm

void CallerAnalysis::dump() const { print(llvm::errs()); }

void CallerAnalysis::print(const char *filePath) const {
  using namespace llvm::sys;
  std::error_code error;
  llvm::raw_fd_ostream fileOutputStream(filePath, error, fs::F_Text);
  if (error) {
    llvm::errs() << "Failed to open path \"" << filePath << "\" for writing.!";
    llvm_unreachable("default error handler");
  }
  print(fileOutputStream);
}

void CallerAnalysis::print(llvm::raw_ostream &os) const {
  llvm::yaml::Output yout(os);

  // NOTE: We purposely do not iterate over our internal state here to ensure
  // that we dump for all functions and that we dump the state we have stored
  // with the functions in module order.
  for (auto &f : Mod) {
    const auto &fi = getCallerInfo(&f);

    std::vector<StringRef> fullAppliers;
    for (auto *caller : fi.Callers) {
      fullAppliers.push_back(caller->getName());
    }
    std::vector<StringRef> partialAppliers;
    for (auto iter : fi.PartialAppliers) {
      partialAppliers.push_back(iter.first->getName());
    }

    YAMLCallGraphNode node(f.getName(), fi.hasCaller(),
                           fi.getMinPartialAppliedArgs(),
                           std::move(partialAppliers), std::move(fullAppliers));
    yout << node;
  }
}

//===----------------------------------------------------------------------===//
//                              Main Entry Point
//===----------------------------------------------------------------------===//

SILAnalysis *swift::createCallerAnalysis(SILModule *M) {
  return new CallerAnalysis(M);
}

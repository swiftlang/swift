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
#include "swift/SIL/InstructionUtils.h"
#include "swift/SIL/SILModule.h"
#include "swift/SILOptimizer/Utils/Local.h"
#include "llvm/Support/FileSystem.h"
#include "llvm/Support/YAMLTraits.h"

using namespace swift;

namespace {
using FunctionInfo = CallerAnalysis::FunctionInfo;
} // end anonymous namespace

//===----------------------------------------------------------------------===//
//                        CallerAnalysis::FunctionInfo
//===----------------------------------------------------------------------===//

CallerAnalysis::FunctionInfo::FunctionInfo(SILFunction *f)
    : callerStates(),
      mayHaveIndirectCallers(canBeCalledIndirectly(f->getRepresentation())) {}

//===----------------------------------------------------------------------===//
//                               CallerAnalysis
//===----------------------------------------------------------------------===//

// NOTE: This is only meant to be used by external users of
// CallerAnalysis since we recompute. For internal uses, please
// instead use getOrInsertFunctionInfo.
const FunctionInfo &CallerAnalysis::getCallerInfo(SILFunction *f) const {
  // Recompute every function in the invalidated function list and empty the
  // list.
  auto &self = const_cast<CallerAnalysis &>(*this);
  self.processRecomputeFunctionList();
  return self.getOrInsertCallerInfo(f);
}

// Private only version of this function for mutable callers that tries to
// initialize a new f.
FunctionInfo &CallerAnalysis::getOrInsertCallerInfo(SILFunction *f) {
  return funcInfos.try_emplace(f, f).first->second;
}

CallerAnalysis::CallerAnalysis(SILModule *m)
    : SILAnalysis(AnalysisKind::Caller), mod(*m) {
  // Make sure we add everything to the recompute function list when we start.
  for (auto &f : mod) {
    getOrInsertCallerInfo(&f);
    recomputeFunctionList.insert(&f);
  }
}

static bool mayHaveIndirectCallers(SILFunction *calleeFn) {
  // We do not support specialized vtables so specialized methods should never
  // be able to be called indirectly.
  if (calleeFn->getLinkage() == SILLinkage::Shared)
    return false;
  return canBeCalledIndirectly(calleeFn->getRepresentation());
}

void CallerAnalysis::processFunctionCallSites(SILFunction *callerFn) {
  // First grab our caller info so that we can store back references
  // from our callerFn to the calleeFn so that we can invalidate all
  // callee info about our caller efficiently.
  FunctionInfo &callerInfo = getOrInsertCallerInfo(callerFn);

#ifndef NDEBUG
  llvm::SmallPtrSet<SILInstruction *, 8> visitedCallSites;
  llvm::SmallSetVector<SILInstruction *, 8> callSitesThatMustBeVisited;
#endif

  // Scan the caller function and search for full or partial apply
  // sites in the caller function.
  for (auto &block : *callerFn) {
    for (auto &i : block) {
#ifndef NDEBUG
      // If this is a call site that we visited as part of seeing a different
      // function_ref, skip it. We know that it has been processed correctly.
      if (visitedCallSites.count(&i))
        continue;
#endif

      // Try to find the apply sites.
      if (auto *fri = dyn_cast<FunctionRefInst>(&i)) {
        if (auto result = findLocalApplySites(fri)) {
          auto *calleeFn = fri->getReferencedFunction();
          FunctionInfo &calleeInfo = getOrInsertCallerInfo(calleeFn);

          calleeInfo.mayHaveIndirectCallers =
            mayHaveIndirectCallers(calleeFn);

          // Next create our caller state.
          auto iter = calleeInfo.callerStates.insert({callerFn, {}});
          // If we succeeded in inserting a new value, put in an optimistic
          // value for escaping.
          if (iter.second) {
            iter.first->second.isDirectCallerSetComplete = true;
          }
          iter.first->second.isDirectCallerSetComplete &= !result.isEscaping();

          if (result.fullApplySites.size()) {
            callerInfo.calleeStates.insert(calleeFn);
            iter.first->second.hasFullApply = true;
#ifndef NDEBUG
            for (auto applySite : result.fullApplySites) {
              visitedCallSites.insert(applySite.getInstruction());
              callSitesThatMustBeVisited.remove(applySite.getInstruction());
            }
#endif
          }

          if (result.partialApplySites.size()) {
            callerInfo.calleeStates.insert(calleeFn);
            auto &optMin = iter.first->second.numPartialAppliedArguments;
            unsigned min = optMin.getValueOr(UINT_MAX);
            for (ApplySite partialSite : result.partialApplySites) {
              min = std::min(min, partialSite.getNumArguments());
            }
            optMin = min;
#ifndef NDEBUG
            for (auto applySite : result.partialApplySites) {
              visitedCallSites.insert(applySite.getInstruction());
              callSitesThatMustBeVisited.remove(applySite.getInstruction());
            }
#endif
          }
          continue;
        }
      }

#ifndef NDEBUG
      // Make sure that we are in sync with FullApplySite.
      if (auto apply = FullApplySite::isa(&i)) {
        if (apply.getCalleeFunction() && !visitedCallSites.count(&i)) {
          callSitesThatMustBeVisited.insert(&i);
        }
        continue;
      }

      // Make sure that we are in sync with looking for partial apply callees.
      if (auto *pai = dyn_cast<PartialApplyInst>(&i)) {
        if (pai->getCalleeFunction() && !visitedCallSites.count(&i)) {
          callSitesThatMustBeVisited.insert(pai);
        }
        continue;
      }
#endif
    }
  }

#ifndef NDEBUG
  if (callSitesThatMustBeVisited.empty())
    return;
  llvm::errs() << "Found unhandled call sites!\n";
  while (callSitesThatMustBeVisited.size()) {
    auto *i = callSitesThatMustBeVisited.pop_back_val();
    llvm::errs() << "Inst: " << *i;
  }
  assert(false && "Unhandled call site?!");
#endif
}

void CallerAnalysis::invalidateExistingCalleeRelation(SILFunction *caller) {
  // Look up the callees that our caller refers to and invalidate any
  // values that point back at the caller.
  FunctionInfo &callerInfo = getOrInsertCallerInfo(caller);

  while (callerInfo.calleeStates.size()) {
    auto *callee = callerInfo.calleeStates.pop_back_val();
    FunctionInfo &calleeInfo = getOrInsertCallerInfo(callee);
    assert(calleeInfo.callerStates.count(caller) &&
           "Referenced callee is not fully/partially applied in the caller?!");

    // Then remove the caller from this specific callee's info struct
    // and to be conservative mark the callee as potentially having an
    // escaping use that we do not understand.
    calleeInfo.callerStates.erase(caller);
  }
}

//===----------------------------------------------------------------------===//
//                          CallerAnalysis YAML Dump
//===----------------------------------------------------------------------===//

namespace {

using llvm::yaml::IO;
using llvm::yaml::MappingTraits;
using llvm::yaml::Output;
using llvm::yaml::ScalarEnumerationTraits;
using llvm::yaml::SequenceTraits;

/// A special struct that marshals call graph state into a form that
/// is easy for llvm's yaml i/o to dump. Its structure is meant to
/// correspond to how the data should be shown by the printer, so
/// naturally it is slightly redundant.
struct YAMLCallGraphNode {
  StringRef calleeName;
  bool hasCaller;
  unsigned minPartialAppliedArgs;
  bool hasOnlyCompleteDirectCallerSets;
  bool hasAllCallers;
  std::vector<StringRef> partialAppliers;
  std::vector<StringRef> fullAppliers;

  YAMLCallGraphNode() = delete;
  ~YAMLCallGraphNode() = default;

  /// Deleted copy constructor. This is a move only data structure.
  YAMLCallGraphNode(const YAMLCallGraphNode &) = delete;
  YAMLCallGraphNode(YAMLCallGraphNode &&) = default;
  YAMLCallGraphNode &operator=(const YAMLCallGraphNode &) = delete;
  YAMLCallGraphNode &operator=(YAMLCallGraphNode &&) = default;

  YAMLCallGraphNode(StringRef calleeName, bool hasCaller,
                    unsigned minPartialAppliedArgs,
                    bool hasOnlyCompleteDirectCallerSets, bool hasAllCallers,
                    std::vector<StringRef> &&partialAppliers,
                    std::vector<StringRef> &&fullAppliers)
      : calleeName(calleeName), hasCaller(hasCaller),
        minPartialAppliedArgs(minPartialAppliedArgs),
        hasOnlyCompleteDirectCallerSets(hasOnlyCompleteDirectCallerSets),
        hasAllCallers(hasAllCallers),
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
    io.mapRequired("hasOnlyCompleteDirectCallerSets",
                   func.hasOnlyCompleteDirectCallerSets);
    io.mapRequired("hasAllCallers", func.hasAllCallers);
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
  for (auto &f : mod) {
    const auto &fi = getCallerInfo(&f);

    std::vector<StringRef> partialAppliers;
    std::vector<StringRef> fullAppliers;
    for (auto &apply : fi.getAllReferencingCallers()) {
      if (apply.second.hasFullApply) {
        fullAppliers.push_back(apply.first->getName());
      }
      if (apply.second.numPartialAppliedArguments.hasValue()) {
        partialAppliers.push_back(apply.first->getName());
      }
    }

    YAMLCallGraphNode node(
        f.getName(), fi.hasCaller(), fi.getMinPartialAppliedArgs(),
        fi.hasOnlyCompleteDirectCallerSets(), fi.hasAllCallers(),
        std::move(partialAppliers), std::move(fullAppliers));
    yout << node;
  }
}

//===----------------------------------------------------------------------===//
//                              Main Entry Point
//===----------------------------------------------------------------------===//

SILAnalysis *swift::createCallerAnalysis(SILModule *mod) {
  return new CallerAnalysis(mod);
}

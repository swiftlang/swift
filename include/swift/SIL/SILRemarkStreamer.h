//===--- SILRemarkStreamer.h - Interface for streaming remarks --*- C++ -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2020 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
/// \file
/// This file defines the interface used to stream SIL optimization diagnostics
/// through LLVM's RemarkStreamer interface.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_SIL_SILREMARKSTREAMER_H
#define SWIFT_SIL_SILREMARKSTREAMER_H

#include "swift/SIL/OptimizationRemark.h"
#include "llvm/Remarks/RemarkStreamer.h"

namespace swift {

class SILRemarkStreamer {
  llvm::remarks::RemarkStreamer &llvmStreamer;
  // Source manager for resolving source locations.
  const SourceManager &srcMgr;
  /// Convert diagnostics into LLVM remark objects.
  /// The lifetime of the members of the result is bound to the lifetime of
  /// the SIL remarks.
  template <typename RemarkT>
  llvm::remarks::Remark
  toLLVMRemark(const OptRemark::Remark<RemarkT> &remark) const;

public:
  SILRemarkStreamer(llvm::remarks::RemarkStreamer &llvmStreamer,
                    const SourceManager &srcMgr)
      : llvmStreamer(llvmStreamer), srcMgr(srcMgr) {}
  /// Emit a remark through the streamer.
  template <typename RemarkT>
  void emit(const OptRemark::Remark<RemarkT> &remark);
};

std::pair<std::unique_ptr<llvm::raw_fd_ostream>,
          std::unique_ptr<SILRemarkStreamer>>
createSILRemarkStreamer(SILModule &srcMgr, StringRef filename, StringRef passes,
                        llvm::remarks::Format format,
                        DiagnosticEngine &diagEngine, SourceManager &sourceMgr);

// Implementation for template member functions.

// OptRemark type -> llvm::remarks::Type
template <typename RemarkT> static llvm::remarks::Type toRemarkType() {
  if (std::is_same<RemarkT, OptRemark::RemarkPassed>::value)
    return llvm::remarks::Type::Passed;
  if (std::is_same<RemarkT, OptRemark::RemarkMissed>::value)
    return llvm::remarks::Type::Missed;
  llvm_unreachable("Unknown remark type");
}

static inline Optional<llvm::remarks::RemarkLocation>
toRemarkLocation(const SourceLoc &loc, const SourceManager &srcMgr) {
  if (!loc.isValid())
    return None;

  StringRef file = srcMgr.getDisplayNameForLoc(loc);
  unsigned line, col;
  std::tie(line, col) = srcMgr.getLineAndColumn(loc);
  return llvm::remarks::RemarkLocation{file, line, col};
}

template <typename RemarkT>
llvm::remarks::Remark SILRemarkStreamer::toLLVMRemark(
    const OptRemark::Remark<RemarkT> &optRemark) const {
  llvm::remarks::Remark llvmRemark; // The result.
  llvmRemark.RemarkType = toRemarkType<RemarkT>();
  llvmRemark.PassName = optRemark.getPassName();
  llvmRemark.RemarkName = optRemark.getIdentifier();
  llvmRemark.FunctionName = optRemark.getDemangledFunctionName();
  llvmRemark.Loc = toRemarkLocation(optRemark.getLocation(), srcMgr);

  for (const OptRemark::Argument &arg : optRemark.getArgs()) {
    llvmRemark.Args.emplace_back();
    llvmRemark.Args.back().Key = arg.key;
    llvmRemark.Args.back().Val = arg.val;
    llvmRemark.Args.back().Loc = toRemarkLocation(arg.loc, srcMgr);
  }

  return llvmRemark;
}

template <typename RemarkT>
void SILRemarkStreamer::emit(const OptRemark::Remark<RemarkT> &optRemark) {
  if (!llvmStreamer.matchesFilter(optRemark.getPassName()))
    return;

  return llvmStreamer.getSerializer().emit(toLLVMRemark(optRemark));
}

} // namespace swift
#endif

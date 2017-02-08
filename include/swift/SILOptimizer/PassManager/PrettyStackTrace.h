//===--- PrettyStackTrace.h - PrettyStackTrace for Transforms ---*- C++ -*-===//
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

#ifndef SWIFT_SILOPTIMIZER_PASSMANAGER_PRETTYSTACKTRACE_H
#define SWIFT_SILOPTIMIZER_PASSMANAGER_PRETTYSTACKTRACE_H

#include "swift/SIL/PrettyStackTrace.h"
#include "llvm/Support/PrettyStackTrace.h"

namespace swift {

class SILFunctionTransform;
class SILModuleTransform;

class PrettyStackTraceSILFunctionTransform
    : public PrettyStackTraceSILFunction {
  SILFunctionTransform *SFT;
  unsigned PassNumber;

public:
  PrettyStackTraceSILFunctionTransform(SILFunctionTransform *SFT,
                                       unsigned PassNumber);

  virtual void print(llvm::raw_ostream &OS) const;
};

class PrettyStackTraceSILModuleTransform : public llvm::PrettyStackTraceEntry {
  SILModuleTransform *SMT;
  unsigned PassNumber;

public:
  PrettyStackTraceSILModuleTransform(SILModuleTransform *SMT,
                                     unsigned PassNumber)
      : SMT(SMT), PassNumber(PassNumber) {}
  virtual void print(llvm::raw_ostream &OS) const;
};

} // end namespace swift

#endif

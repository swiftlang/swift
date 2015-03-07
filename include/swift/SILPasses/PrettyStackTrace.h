//===--- PrettyStackTrace.h - PrettyStackTrace for Transforms -*- C++ -*---===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2015 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See http://swift.org/LICENSE.txt for license information
// See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_SILPASSES_PRETTYSTACKTRACE_H
#define SWIFT_SILPASSES_PRETTYSTACKTRACE_H

#include "llvm/Support/PrettyStackTrace.h"

namespace swift {

class SILFunctionTransform;
class SILModuleTransform;

class PrettyStackTraceSILFunctionTransform
    : public llvm::PrettyStackTraceEntry {
  SILFunctionTransform *SFT;

public:
  PrettyStackTraceSILFunctionTransform(SILFunctionTransform *SFT) : SFT(SFT) {}
  virtual void print(llvm::raw_ostream &OS) const;
};

class PrettyStackTraceSILModuleTransform : public llvm::PrettyStackTraceEntry {
  SILModuleTransform *SMT;

public:
  PrettyStackTraceSILModuleTransform(SILModuleTransform *SMT) : SMT(SMT) {}
  virtual void print(llvm::raw_ostream &OS) const;
};

} // end namespace swift

#endif

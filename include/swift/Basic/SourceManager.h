//===--- SourceManager.h - Manager for Source Buffers -----------*- C++ -*-===//
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


#ifndef SWIFT_SOURCEMANAGER_H
#define SWIFT_SOURCEMANAGER_H

#include "llvm/Support/SourceMgr.h"

namespace swift {

/// \brief This class manages and owns source buffers.
class SourceManager {
  llvm::SourceMgr LLVMSourceMgr;

public:
  SourceManager() {}

  llvm::SourceMgr *operator->() { return &LLVMSourceMgr; }
  const llvm::SourceMgr *operator->() const { return &LLVMSourceMgr; }

  const llvm::SourceMgr &getLLVMSourceMgr() const {
    return LLVMSourceMgr;
  }
};

} // namespace swift

#endif // LLVM_SWIFT_SOURCEMANAGER_H


//===--- ThreadSafeRefCntPtr.cpp ------------------------------------------===//
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

#include "SourceKit/Support/ThreadSafeRefCntPtr.h"
#include <functional>

using namespace SourceKit;
using llvm::sys::Mutex;

static const size_t MtxCount = 16;
static Mutex Mtxs[MtxCount];

Mutex *ThreadSafeRefCntPtrImpl::getMutex(void *Ptr) {
  return Mtxs + (std::hash<const void*>()(Ptr) & (MtxCount-1));
}

//===--- BridgingUtils.h - utilities for swift bridging -------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2021 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_BASIC_BRIDGINGUTILS_H
#define SWIFT_BASIC_BRIDGINGUTILS_H

#include "swift/Basic/BasicBridging.h"
#include "swift/Basic/SourceLoc.h"
#include "llvm/ADT/ArrayRef.h"
#include "llvm/ADT/StringRef.h"
#include "llvm/Support/AllocatorBase.h"

#include <string>

namespace swift {

template <typename T>
inline llvm::ArrayRef<T> getArrayRef(BridgedArrayRef bridged) {
  return {static_cast<const T *>(bridged.data), bridged.numElements};
}

} // namespace swift

#endif

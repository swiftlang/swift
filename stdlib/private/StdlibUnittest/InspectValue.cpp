//===----------------------------------------------------------------------===//
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

#include "swift/Runtime/Config.h"
#include "swift/Runtime/Metadata.h"

using namespace swift;

SWIFT_CC(swift) LLVM_LIBRARY_VISIBILITY extern "C"
uint32_t getMetadataKindOf(
    OpaqueValue *value,
    const Metadata *type
) {
  return uint32_t(type->getKind());
}


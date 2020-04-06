//===--- Metadata.cpp -----------------------------------------------------===//
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

#include "../SwiftShims/Metadata.h"
#include "swift/ABI/Metadata.h"

// Shims to call a metadata accessor in Swift.
SWIFT_RUNTIME_STDLIB_API
swift::MetadataResponse
swift::_swift_metadataAccessorCall(void *accessor,
                                   __swift_size_t request,
                                   const void * const *args,
                                   __swift_size_t size) {
  using Fn = MetadataResponse (...);
  auto func = reinterpret_cast<Fn *>(accessor);
  auto array = llvm::ArrayRef<const void *>(args, size);
  return MetadataAccessFunction(func)(MetadataRequest(request), array);
}

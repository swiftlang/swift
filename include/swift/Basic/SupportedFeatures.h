//===--- SupportedFeatures.h - Supported Features Output --------*- C++ -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2025 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
// This file provides a high-level API for supported features info
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_SUPPORTEDFEATURES_H
#define SWIFT_SUPPORTEDFEATURES_H

#include "swift/Basic/LLVM.h"

namespace swift {
namespace features {
void printSupportedFeatures(llvm::raw_ostream &out);
} // namespace features
} // namespace swift

#endif

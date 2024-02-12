//===--- SupplementaryOutputPaths.h ----------------------------*- C++ -*-===*//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2018 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_FRONTEND_SUPPLEMENTARYOUTPUTPATHS_H
#define SWIFT_FRONTEND_SUPPLEMENTARYOUTPUTPATHS_H

#include "swift/Basic/FileTypes.h"
#include "swift/Basic/LLVM.h"
#include "llvm/IR/Function.h"

#include <string>

namespace swift {
struct SupplementaryOutputPaths {
#define OUTPUT(NAME, TYPE) std::string NAME;
#include "swift/Basic/SupplementaryOutputPaths.def"
#undef OUTPUT

  SupplementaryOutputPaths() = default;

  /// Apply a given function for each existing (non-empty string) supplementary output
  void forEachSetOutput(llvm::function_ref<void(const std::string&)> fn) const {
#define OUTPUT(NAME, TYPE)                                                     \
  if (!NAME.empty())                                                           \
    fn(NAME);
#include "swift/Basic/SupplementaryOutputPaths.def"
#undef OUTPUT
  }

  void forEachSetOutputAndType(
      llvm::function_ref<void(const std::string &, file_types::ID)> fn) const {
#define OUTPUT(NAME, TYPE)                                                     \
  if (!NAME.empty())                                                           \
    fn(NAME, file_types::ID::TYPE);
#include "swift/Basic/SupplementaryOutputPaths.def"
#undef OUTPUT
  }

  bool empty() const {
    return
#define OUTPUT(NAME, TYPE) NAME.empty() &&
#include "swift/Basic/SupplementaryOutputPaths.def"
#undef OUTPUT
        true;
  }
};
} // namespace swift

#endif // SWIFT_FRONTEND_SUPPLEMENTARYOUTPUTPATHS_H

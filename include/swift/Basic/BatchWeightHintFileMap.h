//===--- BatchWeightHintFileMap.h - Map of input-file to batch-weight-hints
//----*- C++
//-*-===//
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

#ifndef SWIFT_BASIC_BATCHWEIGHTHINTFILEMAP_H
#define SWIFT_BASIC_BATCHWEIGHTHINTFILEMAP_H

#include "swift/Basic/FileTypes.h"
#include "swift/Basic/LLVM.h"
#include "llvm/ADT/DenseMap.h"
#include "llvm/ADT/StringMap.h"
#include "llvm/ADT/StringSet.h"
#include "llvm/Support/Error.h"
#include "llvm/Support/MemoryBuffer.h"
#include "llvm/Support/SourceMgr.h"
#include "llvm/Support/YAMLParser.h"

#include <memory>
#include <string>

namespace swift {

/// This structure embeds a map that stores input-path to batch-weight-hints.
class BatchWeightHintFileMap {
private:
  llvm::StringMap<double> InputToBatchWeightHintMap;

public:
  BatchWeightHintFileMap() {}

  ~BatchWeightHintFileMap() = default;

  /// Load a BatchWeightHintFileMap from the given \p Path.
  static llvm::Expected<BatchWeightHintFileMap>
  loadFromPath(StringRef Path, StringRef workingDirectory);

  /// Load a BatchWeightHintFileMap from the given \p Buffer.
  static llvm::Expected<BatchWeightHintFileMap>
  loadFromBuffer(StringRef Data, StringRef workingDirectory);

  /// Loads a BatchWeightHintFileMap from the given Buffer, takes ownership
  /// of the buffer as well.
  static llvm::Expected<BatchWeightHintFileMap>
  loadFromBuffer(std::unique_ptr<llvm::MemoryBuffer> Buffer,
                 StringRef workingDirectory);

  /// Get the batch-weight-hint for the given \p Input, if present in the
  /// BatchWeightHintFileMap. (If not present, returns 0.)
  double getBatchWeightHintsForInput(StringRef Input);

  /// Get the batch-weight-hint for a single compilion product.
  double getBatchWeightHintsForSingleOutput();

private:
  /// Parses the given \p Buffer and returns either a BatchWeightHintFileMap or
  /// error, take ownership of the buffer as well.
  static llvm::Expected<BatchWeightHintFileMap>
  parse(std::unique_ptr<llvm::MemoryBuffer> Buffer, StringRef workingDirectory);
};

} // end namespace swift

#endif

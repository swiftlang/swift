//===--- OutputFileMap.h - Driver output file map ---------------*- C++ -*-===//
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

#ifndef SWIFT_DRIVER_OUTPUTFILEMAP_H
#define SWIFT_DRIVER_OUTPUTFILEMAP_H

#include "swift/Basic/LLVM.h"
#include "swift/Driver/Types.h"
#include "llvm/ADT/DenseMap.h"
#include "llvm/ADT/StringMap.h"
#include "llvm/Support/MemoryBuffer.h"
#include "llvm/Support/SourceMgr.h"
#include "llvm/Support/YAMLParser.h"

#include <memory>
#include <string>

namespace swift {
namespace driver {

typedef llvm::DenseMap<types::ID, std::string> TypeToPathMap;

class OutputFileMap {
private:
  llvm::StringMap<TypeToPathMap> InputToOutputsMap;

  OutputFileMap() {}

public:
  ~OutputFileMap() = default;

  /// Loads an OutputFileMap from the given \p Path, if possible.
  static std::unique_ptr<OutputFileMap> loadFromPath(StringRef Path);

  static std::unique_ptr<OutputFileMap> loadFromBuffer(StringRef Data);

  /// Loads an OutputFileMap from the given \p Buffer, taking ownership
  /// of the buffer in the process.
  static std::unique_ptr<OutputFileMap>
  loadFromBuffer(std::unique_ptr<llvm::MemoryBuffer> Buffer);

  /// Get the map of outputs for the given \p Input, if present in the
  /// OutputFileMap. (If not present, returns nullptr.)
  const TypeToPathMap *getOutputMapForInput(StringRef Input) const;

  /// Get the map of outputs for a single compile product.
  const TypeToPathMap *getOutputMapForSingleOutput() const;

  /// Dump the OutputFileMap to the given \p os.
  void dump(llvm::raw_ostream &os, bool Sort = false) const;

private:
  /// \brief Parses the given \p Buffer into the OutputFileMap, taking ownership
  /// of \p Buffer in the process.
  ///
  /// \returns true on error, false on success
  bool parse(std::unique_ptr<llvm::MemoryBuffer> Buffer);
};

} // end namespace driver
} // end namespace swift

#endif

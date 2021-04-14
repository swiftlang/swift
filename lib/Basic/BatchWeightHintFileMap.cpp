//===--- BatchWeightHintFileMap.cpp - Map of inputs to multiple outputs ------------===//
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

#include "swift/Basic/BatchWeightHintFileMap.h"
#include "swift/Basic/FileTypes.h"
#include "llvm/ADT/SmallString.h"
#include "llvm/ADT/StringRef.h"
#include "llvm/Support/Path.h"
#include "llvm/Support/raw_ostream.h"
#include <system_error>
#include <sstream>
#include <string>

using namespace swift;

llvm::Expected<BatchWeightHintFileMap>
BatchWeightHintFileMap::loadFromPath(StringRef Path, StringRef workingDir) {
  llvm::ErrorOr<std::unique_ptr<llvm::MemoryBuffer>> FileBufOrErr =
      llvm::MemoryBuffer::getFile(Path);
  if (!FileBufOrErr) {
    return llvm::errorCodeToError(FileBufOrErr.getError());
  }
  return loadFromBuffer(std::move(FileBufOrErr.get()), workingDir);
}

llvm::Expected<BatchWeightHintFileMap>
BatchWeightHintFileMap::loadFromBuffer(StringRef Data, StringRef workingDir) {
  std::unique_ptr<llvm::MemoryBuffer> Buffer{
      llvm::MemoryBuffer::getMemBuffer(Data)};
  return loadFromBuffer(std::move(Buffer), workingDir);
}

llvm::Expected<BatchWeightHintFileMap>
BatchWeightHintFileMap::loadFromBuffer(std::unique_ptr<llvm::MemoryBuffer> Buffer,
                              StringRef workingDir) {
  return parse(std::move(Buffer), workingDir);
}

double BatchWeightHintFileMap::getBatchWeightHintsForInput(StringRef Input) {
  auto iter = InputToBatchWeightHintMap.find(Input);
  if (iter == InputToBatchWeightHintMap.end())
    return 0;
  else
    return iter->second;
}

double BatchWeightHintFileMap::getBatchWeightHintsForSingleOutput() {
  return getBatchWeightHintsForInput(StringRef());
}

llvm::Expected<BatchWeightHintFileMap>
BatchWeightHintFileMap::parse(std::unique_ptr<llvm::MemoryBuffer> Buffer,
                     StringRef workingDir) {
  // Resolve the fully qualified path name.
  auto tryToResolvePath =
      [workingDir](
          StringRef PathStr,
          llvm::SmallVectorImpl<char> &PathStorage) -> StringRef {
    if (workingDir.empty() || PathStr.empty() || PathStr == "-" ||
        llvm::sys::path::is_absolute(PathStr)) {
      return PathStr;
    }
    // Make a copy of the path.
    SmallString<128> PathStrCopy(PathStr);
    PathStorage.clear();
    PathStorage.reserve(PathStrCopy.size() + workingDir.size() + 1);
    PathStorage.insert(PathStorage.begin(), workingDir.begin(),
                       workingDir.end());

    // Append PathStr to PathStorage.
    llvm::sys::path::append(PathStorage, PathStrCopy);
    return StringRef(PathStorage.data(), PathStorage.size());
  };

  BatchWeightHintFileMap BWFM;
  llvm::SourceMgr SM;

  // Emit Error.
  auto emitError =
      [](const char *errorStr) -> llvm::Expected<BatchWeightHintFileMap> {
    return llvm::make_error<llvm::StringError>(errorStr,
                                               llvm::inconvertibleErrorCode());
  };
  /*
  Format of the YAML file:
  [
  { "full_path/swift_file_1.swift" :  weight },
  { "full_path/swift_file_2.swift" :  weight },
  ]

  */
  llvm::yaml::Stream Stream(Buffer->getMemBufferRef(), SM);
  for (auto DI = Stream.begin(); DI != Stream.end(); ++ DI) {
    auto Root = DI->getRoot();
    if (!Root) {
      return emitError("No Root");
    }
    auto Array = dyn_cast<llvm::yaml::SequenceNode>(DI->getRoot());
    if (!Array) {
      return emitError("Root not a SequenceNode");
    }
    for (auto It = Array->begin(); It != Array->end(); ++ It) {
      auto *Map = dyn_cast<llvm::yaml::MappingNode>(&*It);
      if (!Map) {
        return emitError("Not a Mapping Node in SequenceNode");
      }
      for (auto &Pair : *Map) {

        llvm::yaml::Node *Key = Pair.getKey();
        llvm::yaml::Node *Value = Pair.getValue();

        auto *fileNameNode = dyn_cast<llvm::yaml::ScalarNode>(Pair.getKey());
        if (!fileNameNode) {
          return emitError("Bad Key as FileName");
        }
        // Drop all double quotes
        auto fileNameSR = fileNameNode->getRawValue().str();
        fileNameSR.erase(remove(fileNameSR.begin(), fileNameSR.end(), '\"'), fileNameSR.end());
        auto fileName = StringRef(fileNameSR);

        auto *weightNode = dyn_cast<llvm::yaml::ScalarNode>(Pair.getValue());
        if (!weightNode) {
          return emitError("Bad Value as Weight");
        }
        auto weightStr = weightNode->getRawValue();
        double weight=0;
        weightStr.getAsDouble(weight);

        llvm::SmallString<128> InputStorage;
			  auto resolvedPath = tryToResolvePath(fileName, InputStorage);
        // Save in the map --> input path : weight.
        BWFM.InputToBatchWeightHintMap[resolvedPath] = weight;
      }
    }
  }
  return BWFM;
}

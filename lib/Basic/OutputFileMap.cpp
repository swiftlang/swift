//===--- OutputFileMap.cpp - Map of inputs to multiple outputs ------------===//
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

#include "swift/Basic/OutputFileMap.h"
#include "llvm/ADT/SmallString.h"
#include "llvm/ADT/StringRef.h"
#include "llvm/Support/Path.h"
#include "llvm/Support/raw_ostream.h"
#include <system_error>

using namespace swift;

llvm::Expected<OutputFileMap>
OutputFileMap::loadFromPath(StringRef Path, StringRef workingDirectory) {
  llvm::ErrorOr<std::unique_ptr<llvm::MemoryBuffer>> FileBufOrErr =
      llvm::MemoryBuffer::getFile(Path);
  if (!FileBufOrErr) {
    return llvm::errorCodeToError(FileBufOrErr.getError());
  }
  return loadFromBuffer(std::move(FileBufOrErr.get()), workingDirectory);
}

llvm::Expected<OutputFileMap>
OutputFileMap::loadFromBuffer(StringRef Data, StringRef workingDirectory) {
  std::unique_ptr<llvm::MemoryBuffer> Buffer{
      llvm::MemoryBuffer::getMemBuffer(Data)};
  return loadFromBuffer(std::move(Buffer), workingDirectory);
}

llvm::Expected<OutputFileMap>
OutputFileMap::loadFromBuffer(std::unique_ptr<llvm::MemoryBuffer> Buffer,
                              StringRef workingDirectory) {
  return parse(std::move(Buffer), workingDirectory);
}

const TypeToPathMap *OutputFileMap::getOutputMapForInput(StringRef Input) const{
  auto iter = InputToOutputsMap.find(Input);
  if (iter == InputToOutputsMap.end())
    return nullptr;
  else
    return &iter->second;
}

TypeToPathMap &
OutputFileMap::getOrCreateOutputMapForInput(StringRef Input) {
  return InputToOutputsMap[Input];
}

const TypeToPathMap *OutputFileMap::getOutputMapForSingleOutput() const {
  return getOutputMapForInput(StringRef());
}

TypeToPathMap &
OutputFileMap::getOrCreateOutputMapForSingleOutput() {
  return InputToOutputsMap[StringRef()];
}

void OutputFileMap::dump(llvm::raw_ostream &os, bool Sort) const {
  using TypePathPair = std::pair<file_types::ID, std::string>;

  auto printOutputPair = [&os](StringRef InputPath,
                               const TypePathPair &OutputPair) -> void {
    os << InputPath << " -> " << file_types::getTypeName(OutputPair.first)
       << ": \"" << OutputPair.second << "\"\n";
  };

  if (Sort) {
    using PathMapPair = std::pair<StringRef, TypeToPathMap>;
    std::vector<PathMapPair> Maps;
    for (auto &InputPair : InputToOutputsMap) {
      Maps.emplace_back(InputPair.first(), InputPair.second);
    }
    std::sort(Maps.begin(), Maps.end(), [] (const PathMapPair &LHS,
                                            const PathMapPair &RHS) -> bool {
      return LHS.first < RHS.first;
    });
    for (auto &InputPair : Maps) {
      const TypeToPathMap &Map = InputPair.second;
      std::vector<TypePathPair> Pairs;
      Pairs.insert(Pairs.end(), Map.begin(), Map.end());
      std::sort(Pairs.begin(), Pairs.end());
      for (auto &OutputPair : Pairs) {
        printOutputPair(InputPair.first, OutputPair);
      }
    }
  } else {
    for (auto &InputPair : InputToOutputsMap) {
      const TypeToPathMap &Map = InputPair.second;
      for (const TypePathPair &OutputPair : Map) {
        printOutputPair(InputPair.first(), OutputPair);
      }
    }
  }
}

static void writeQuotedEscaped(llvm::raw_ostream &os,
                               const StringRef fileName) {
  os << "\"" << llvm::yaml::escape(fileName) << "\"";
}

void OutputFileMap::write(llvm::raw_ostream &os,
                          ArrayRef<StringRef> inputs) const {
  for (const auto input : inputs) {
    writeQuotedEscaped(os, input);
    os << ":";

    const TypeToPathMap *outputMap = getOutputMapForInput(input);
    if (!outputMap) {
      // The map doesn't have an entry for this input. (Perhaps there were no
      // outputs and thus the entry was never created.) Put an empty sub-map
      // into the output and move on.
      os << " {}\n";
      continue;
    }

    os << "\n";
    for (auto &typeAndOutputPath : *outputMap) {
      file_types::ID type = typeAndOutputPath.getFirst();
      StringRef output = typeAndOutputPath.getSecond();
      os << "  " << file_types::getTypeName(type) << ": ";
      writeQuotedEscaped(os, output);
      os << "\n";
    }
  }
}

llvm::Expected<OutputFileMap>
OutputFileMap::parse(std::unique_ptr<llvm::MemoryBuffer> Buffer,
                     StringRef workingDirectory) {
  auto constructError =
      [](const char *errorString) -> llvm::Expected<OutputFileMap> {
    return llvm::make_error<llvm::StringError>(errorString,
                                               llvm::inconvertibleErrorCode());
  };
  /// FIXME: Make the returned error strings more specific by including some of
  /// the source.
  llvm::SourceMgr SM;
  llvm::yaml::Stream YAMLStream(Buffer->getMemBufferRef(), SM);
  auto I = YAMLStream.begin();
  if (I == YAMLStream.end())
    return constructError("empty YAML stream");

  auto Root = I->getRoot();
  if (!Root)
    return constructError("no root");

  OutputFileMap OFM;

  auto *Map = dyn_cast<llvm::yaml::MappingNode>(Root);
  if (!Map)
    return constructError("root was not a MappingNode");

  auto resolvePath =
      [workingDirectory](
          llvm::yaml::ScalarNode *Path,
          llvm::SmallVectorImpl<char> &PathStorage) -> StringRef {
    StringRef PathStr = Path->getValue(PathStorage);
    if (workingDirectory.empty() || PathStr.empty() || PathStr == "-" ||
        llvm::sys::path::is_absolute(PathStr)) {
      return PathStr;
    }
    // Copy the path to avoid making assumptions about how getValue deals with
    // Storage.
    SmallString<128> PathStrCopy(PathStr);
    PathStorage.clear();
    PathStorage.reserve(PathStrCopy.size() + workingDirectory.size() + 1);
    PathStorage.insert(PathStorage.begin(), workingDirectory.begin(),
                       workingDirectory.end());
    llvm::sys::path::append(PathStorage, PathStrCopy);
    return StringRef(PathStorage.data(), PathStorage.size());
  };

  for (auto &Pair : *Map) {
    llvm::yaml::Node *Key = Pair.getKey();
    llvm::yaml::Node *Value = Pair.getValue();

    if (!Key)
      return constructError("bad key");

    if (!Value)
      return constructError("bad value");

    auto *InputPath = dyn_cast<llvm::yaml::ScalarNode>(Key);
    if (!InputPath)
      return constructError("input path not a scalar node");

    llvm::yaml::MappingNode *OutputMapNode =
      dyn_cast<llvm::yaml::MappingNode>(Value);
    if (!OutputMapNode)
      return constructError("output map not a MappingNode");

    TypeToPathMap OutputMap;

    for (auto &OutputPair : *OutputMapNode) {
      llvm::yaml::Node *Key = OutputPair.getKey();
      llvm::yaml::Node *Value = OutputPair.getValue();

      auto *KindNode = dyn_cast<llvm::yaml::ScalarNode>(Key);
      if (!KindNode)
        return constructError("kind not a ScalarNode");

      auto *Path = dyn_cast<llvm::yaml::ScalarNode>(Value);
      if (!Path)
        return constructError("path not a scalar node");

      llvm::SmallString<16> KindStorage;
      file_types::ID Kind =
          file_types::lookupTypeForName(KindNode->getValue(KindStorage));

      // Ignore unknown types, so that an older swiftc can be used with a newer
      // build system.
      if (Kind == file_types::TY_INVALID)
        continue;

      llvm::SmallString<128> PathStorage;
      OutputMap.insert(std::pair<file_types::ID, std::string>(
          Kind, resolvePath(Path, PathStorage)));
    }

    llvm::SmallString<128> InputStorage;
    OFM.InputToOutputsMap[resolvePath(InputPath, InputStorage)] =
        std::move(OutputMap);
  }

  if (YAMLStream.failed())
    return constructError("Output file map parse failed");

  return OFM;
}

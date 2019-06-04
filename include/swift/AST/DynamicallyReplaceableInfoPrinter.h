//===- DynamicallyReplaceableInfoPrinter.h - Print dynamic info -*- C++ -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2019 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_AST_DYNAMICREPLACEABLEINFOPRINTER_H
#define SWIFT_AST_DYNAMICREPLACEABLEINFOPRINTER_H

#include "swift/AST/ASTWalker.h"
#include "llvm/ADT/SmallVector.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/Support/YAMLTraits.h"

namespace swift {

struct YAMLDynamicallyReplaceableNode {
  std::string name;
  std::string type;
  uint64_t startLocLine;
  uint64_t startLocCol;

  uint64_t endLocLine;
  uint64_t endLocCol;


  bool operator<(const YAMLDynamicallyReplaceableNode &other) const {
    if (name == other.name)
      return type < other.type;
    return name < other.name;
  }
};

struct YAMLDynamicallyReplaceableFile {
  std::string name;
  std::vector<swift::YAMLDynamicallyReplaceableNode> decls;

  bool operator<(const YAMLDynamicallyReplaceableFile &other) const {
    return name < other.name;
  }
};

struct YAMLDynamicallyReplaceableModule {
  std::string name;
  uint64_t formatVersion;
  std::vector<swift::YAMLDynamicallyReplaceableFile> files;
};

}

namespace llvm {
namespace yaml {

template <> struct MappingTraits<swift::YAMLDynamicallyReplaceableNode> {
  static void mapping(IO &io,
                      swift::YAMLDynamicallyReplaceableNode &node) {
    io.mapRequired("name", node.name);
    io.mapRequired("type", node.type);
    io.mapRequired("startLocLine", node.startLocLine);
    io.mapRequired("startLocCol", node.startLocCol);
    io.mapRequired("endLocLine", node.endLocLine);
    io.mapRequired("endLocCol", node.endLocCol);
  }
};

template <>
struct MappingTraits<swift::YAMLDynamicallyReplaceableFile> {
  static void mapping(IO &io,
                      swift::YAMLDynamicallyReplaceableFile &node) {
    io.mapRequired("name", node.name);
    io.mapOptional("decls", node.decls);
  }
};

template <>
struct MappingTraits<swift::YAMLDynamicallyReplaceableModule> {
  static void mapping(IO &io,
                      swift::YAMLDynamicallyReplaceableModule &node) {
    io.mapRequired("name", node.name);
    io.mapRequired("formatVersion", node.formatVersion);
    io.mapOptional("files", node.files);
  }
};

} // namespace yaml
} // namespace llvm

LLVM_YAML_IS_SEQUENCE_VECTOR(swift::YAMLDynamicallyReplaceableNode);
LLVM_YAML_IS_DOCUMENT_LIST_VECTOR(
    swift::YAMLDynamicallyReplaceableModule);
LLVM_YAML_IS_SEQUENCE_VECTOR(
    swift::YAMLDynamicallyReplaceableFile);


namespace swift {

class DynamicallyReplaceableInfoPrinter : public ASTWalker {
  YAMLDynamicallyReplaceableModule module;
  llvm::DenseMap<StringRef, size_t> fileMap;

public:
  DynamicallyReplaceableInfoPrinter(llvm::StringRef moduleName);

  bool walkToDeclPre(Decl *d);
  void writeToStream(llvm::raw_ostream &os);
private:
  YAMLDynamicallyReplaceableFile &getNodeForFile(StringRef filename);
};

} //namespace swift

#endif // SWIFT_AST_DYNAMICREPLACEABLEINFOPRINTER_H

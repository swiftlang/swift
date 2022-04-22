//===---------------- BinaryScanningTool.h - Swift Compiler ---------------===//
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

#ifndef SWIFT_BINARY_SCANNING_TOOL_H
#define SWIFT_BINARY_SCANNING_TOOL_H

#include "swift/StaticMirror/ObjectFileContext.h"
#include "llvm/Support/Error.h"
#include "llvm/Support/StringSaver.h"

namespace llvm {
namespace object {
class ObjectFile;
class Binary;
} // namespace object
} // namespace llvm

namespace swift {

namespace reflection {
struct ProtocolConformanceInfo;
} // namespace reflection

namespace static_mirror {

/// The high-level implementation of the static mirror binary scanner
class BinaryScanningTool {
  template <unsigned PointerSize>
  using ExternalReflectionContext = swift::reflection::ReflectionContext<External<RuntimeTarget<PointerSize>>>;

public:
  /// Construct a binary scanning tool.
  BinaryScanningTool(const std::vector<std::string> &binaryPaths,
                     const std::string Arch);

  /// Collect all types conforming to specified protocols
  reflection::ConformanceCollectionResult
  collectConformances(const std::vector<std::string> &protocolNames);

  /// Collect all associated types of the specified type
  reflection::AssociatedTypeCollectionResult
  collectAssociatedTypes(const std::string &mangledTypeName);

  /// Collect associated types of all discovered types
  reflection::AssociatedTypeCollectionResult
  collectAllAssociatedTypes();

  /// Collect all field type infos of the specified type
  reflection::FieldTypeCollectionResult
  collectFieldTypes(const std::string &mangledTypeName);

  /// Collect field type infos of all discovered types
  reflection::FieldTypeCollectionResult
  collectAllFieldTypes();

private:
  // Note: binaryOrError and objectOrError own the memory for our ObjectFile;
  // once they go out of scope, we can no longer do anything.
  std::vector<llvm::object::OwningBinary<llvm::object::Binary>> BinaryOwners;
  std::vector<std::unique_ptr<llvm::object::ObjectFile>> ObjectOwners;
  std::vector<const llvm::object::ObjectFile *> ObjectFiles;

  std::unique_ptr<ReflectionContextHolder> Context;
  size_t PointerSize;
};

} // end namespace static_mirror
} // end namespace swift

#endif // SWIFT_BINARY_SCANNING_TOOL_H

//===--- LegacyLayoutFormat.h - YAML format for legacy layout ---*- C++ -*-===//
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
//
// This file defines the YAML format for the backward deployment type layout
// dump.
//
// When a class with @objc ancestry is statically visible to Clang code, older
// Objective-C runtimes do not give us the opportunity to run any code to
// initialize the class metadata when it is realized.
//
// This creates a problem if the class has resiliently-sized fields. Since the
// standard library and overlays are now built with resilience enabled, this
// creates a backward-compatibility issue where such class definitions would now
// require singleton metadata initialization instead of idempotent metadata
// initialization, and singleton metadata initializaiton precludes the class
// from being statically visible to Clang.
//
// To support this case, we emit fixed metadata for any such class, and in place
// of each resilient field type, we use previously-emitted fixed type info.
//
// This fixed type info must match the Swift standard library and overlays used
// for backward deployment, since on older Objective-C runtimes these layouts
// will be used at runtime.
//
// However, since these types are resilient, their layouts might change in the
// future. Newer Objective-C runtimes will expose a hook allowing the Swift
// runtime to re-compute the class layout when the class is realized.
//
// Note that except for metadata emission, field accesses and instance
// allocation for such classes must proceed as if they use singleton metadata
// initialization, loading the field offsets from global variables and loading
// the size and alignment dynamically from metadata when allocating.
//
// Also, any Swift-side accesses of the metadata must call the metadata accessor
// function, allowing the Swift runtime to re-initialize the layout if
// necessary.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_IRGEN_LEGACY_LAYOUT_FORMAT_H
#define SWIFT_IRGEN_LEGACY_LAYOUT_FORMAT_H

#include "llvm/ADT/ArrayRef.h"
#include "llvm/ADT/SmallVector.h"
#include "llvm/Support/FileSystem.h"
#include "llvm/Support/YAMLTraits.h"
#include "llvm/Support/raw_ostream.h"

namespace swift {
namespace irgen {

struct YAMLTypeInfoNode {
  std::string Name;
  uint64_t Size;
  uint64_t Alignment;
  uint64_t NumExtraInhabitants;

  bool operator<(const YAMLTypeInfoNode &other) const {
    return Name < other.Name;
  }
};

struct YAMLModuleNode {
  StringRef Name;
  std::vector<YAMLTypeInfoNode> Decls;
};

} // namespace irgen
} // namespace swift

namespace llvm {
namespace yaml {

template <> struct MappingTraits<swift::irgen::YAMLTypeInfoNode> {
  static void mapping(IO &io, swift::irgen::YAMLTypeInfoNode &node) {
    io.mapRequired("Name", node.Name);
    io.mapRequired("Size", node.Size);
    io.mapRequired("Alignment", node.Alignment);
    io.mapRequired("ExtraInhabitants", node.NumExtraInhabitants);
  }
};

template <> struct MappingTraits<swift::irgen::YAMLModuleNode> {
  static void mapping(IO &io, swift::irgen::YAMLModuleNode &node) {
    io.mapRequired("Name", node.Name);
    io.mapOptional("Decls", node.Decls);
  }
};

} // namespace yaml
} // namespace llvm

LLVM_YAML_IS_SEQUENCE_VECTOR(swift::irgen::YAMLTypeInfoNode);
LLVM_YAML_IS_DOCUMENT_LIST_VECTOR(swift::irgen::YAMLModuleNode);

#endif // SWIFT_IRGEN_LEGACY_LAYOUT_FORMAT_H

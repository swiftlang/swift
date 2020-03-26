//===--- FileTypes.h - Input & output formats used by the tools -*- C++ -*-===//
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

#ifndef SWIFT_BASIC_FILETYPES_H
#define SWIFT_BASIC_FILETYPES_H

#include "swift/Basic/LLVM.h"
#include "llvm/ADT/DenseMapInfo.h"
#include "llvm/ADT/STLExtras.h"
#include "llvm/ADT/StringRef.h"

namespace swift {
namespace file_types {
enum ID : uint8_t {
#define TYPE(NAME, ID, EXTENSION, FLAGS) TY_##ID,
#include "swift/Basic/FileTypes.def"
#undef TYPE
  TY_INVALID
};

/// Return the name of the type for \p Id.
StringRef getTypeName(ID Id);

/// Return the extension to use when creating a file of this type,
/// or an empty string if unspecified.
StringRef getExtension(ID Id);

/// Lookup the type to use for the file extension \p Ext.
/// If the extension is empty or is otherwise not recognized, return
/// the invalid type \c TY_INVALID.
ID lookupTypeForExtension(StringRef Ext);

/// Lookup the type to use for the name \p Name.
ID lookupTypeForName(StringRef Name);

/// Returns true if the type represents textual data.
bool isTextual(ID Id);

/// Returns true if the type is produced in the compiler after the LLVM
/// passes.
///
/// For those types the compiler produces multiple output files in multi-
/// threaded compilation.
bool isAfterLLVM(ID Id);

/// Returns true if the type is a file that contributes to the Swift module
/// being compiled.
///
/// These need to be passed to the Swift frontend
bool isPartOfSwiftCompilation(ID Id);

static inline void forAllTypes(llvm::function_ref<void(file_types::ID)> fn) {
  for (uint8_t i = 0; i < static_cast<uint8_t>(TY_INVALID); ++i)
    fn(static_cast<ID>(i));
}

/// Some files are produced by the frontend and read by the driver in order to
/// support incremental compilation. Invoke the passed-in function for every
/// such file type.
static inline void
forEachIncrementalOutputType(llvm::function_ref<void(file_types::ID)> fn) {
  static const std::vector<file_types::ID> incrementalOutputTypes = {
      file_types::TY_SwiftDeps, file_types::TY_SwiftRanges,
      file_types::TY_CompiledSource};
  for (auto type : incrementalOutputTypes)
    fn(type);
}

} // end namespace file_types
} // end namespace swift

namespace llvm {
template <> struct DenseMapInfo<swift::file_types::ID> {
  using ID = swift::file_types::ID;
  static inline ID getEmptyKey() { return ID::TY_INVALID; }
  static inline ID getTombstoneKey() {
    return static_cast<ID>(ID::TY_INVALID + 1);
  }
  static unsigned getHashValue(ID Val) { return (unsigned)Val * 37U; }
  static bool isEqual(ID LHS, ID RHS) { return LHS == RHS; }
};
} // end namespace llvm

#endif

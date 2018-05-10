//===--- FileTypes.h - Input & Temporary Driver Types -----------*- C++ -*-===//
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

#ifndef SWIFT_FRONTEND_FILETYPES_H
#define SWIFT_FRONTEND_FILETYPES_H

#include "swift/Basic/LLVM.h"
#include "llvm/ADT/DenseMapInfo.h"
#include "llvm/ADT/StringRef.h"
#include <functional>

namespace swift {
namespace file_types {
enum ID : uint8_t {
#define TYPE(NAME, ID, TEMP_SUFFIX, FLAGS) TY_##ID,
#include "swift/Frontend/Types.def"
#undef TYPE
  TY_INVALID
};

/// Return the name of the type for \p Id.
StringRef getTypeName(ID Id);

/// Return the suffix to use when creating a temp file of this type,
/// or null if unspecified.
StringRef getTypeTempSuffix(ID Id);

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

template <typename Fn> void forAllTypes(const Fn &fn);
} // namespace file_types
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
} // namespace llvm

template <typename Fn> void swift::file_types::forAllTypes(const Fn &fn) {
  static_assert(
      std::is_constructible<std::function<void(file_types::ID)>, Fn>::value,
      "must have the signature 'void(file_types::ID)'");
  for (uint8_t i = 0; i < static_cast<uint8_t>(TY_INVALID); ++i)
    fn(static_cast<ID>(i));
}

#endif

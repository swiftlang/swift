//===--- LocalTypeDeclManglings.h - Local type lookup manglings -*- C++ -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2026 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_AST_LOCALTYPEDECLMANGLINGS_H
#define SWIFT_AST_LOCALTYPEDECLMANGLINGS_H

#include "swift/Basic/LLVM.h"
#include "llvm/ADT/StringRef.h"
#include <string>

namespace swift {
namespace Mangle {

/// The manglings that can be used to look up a local type declaration by the
/// mangled name of its type. Debug info and runtime metadata respect
/// @_originallyDefinedIn and the ABI name of _Concurrency, but USRs don't, so
/// the manglings can differ.
class LocalTypeDeclManglings {
  std::string Mangling;

  /// Empty if the USR mangling is the same as \c Mangling.
  std::string USRMangling;

public:
  LocalTypeDeclManglings() = default;
  LocalTypeDeclManglings(std::string mangling, std::string usrMangling)
      : Mangling(std::move(mangling)) {
    if (usrMangling != Mangling)
      USRMangling = std::move(usrMangling);
  }

  /// The mangling used by debug info and runtime metadata.
  StringRef getMangling() const { return Mangling; }

  /// The mangling used by USRs.
  StringRef getUSRMangling() const {
    return USRMangling.empty() ? Mangling : USRMangling;
  }

  /// Whether the USR mangling is different from the mangling used by debug
  /// info and runtime metadata.
  bool hasDistinctUSRMangling() const { return !USRMangling.empty(); }

  /// Whether \p mangledName is one of the manglings.
  bool matches(StringRef mangledName) const {
    return mangledName == Mangling ||
           (!USRMangling.empty() && mangledName == USRMangling);
  }
};

} // end namespace Mangle
} // end namespace swift

#endif

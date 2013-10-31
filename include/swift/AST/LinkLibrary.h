//===--- LinkLibrary.h - A module-level linker dependency -------*- C++ -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2015 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See http://swift.org/LICENSE.txt for license information
// See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_AST_LINKLIBRARY_H
#define SWIFT_AST_LINKLIBRARY_H

#include "swift/Basic/LLVM.h"
#include "llvm/ADT/SmallString.h"
#include <string>

namespace swift {

// Must be kept in sync with diag::error_immediate_mode_missing_library.
enum class LibraryKind {
  Library = 0,
  Framework
};

/// Represents a linker dependency for an imported module.
// FIXME: This is basically a slightly more generic version of Clang's
// Module::LinkLibrary.
class LinkLibrary {
private:
  std::string Name;
  LibraryKind Kind;

public:
  LinkLibrary(StringRef N, LibraryKind K) : Name(N), Kind(K) {}

  LibraryKind getKind() const { return Kind; }
  StringRef getName() const { return Name; }
};

} // end namespace swift

#endif

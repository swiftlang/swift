//===- Utils.h - Misc utilities -------------------------------------------===//
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

#ifndef SWIFT_IDE_UTILS_H
#define SWIFT_IDE_UTILS_H

#include "swift/Basic/LLVM.h"
#include <memory>

namespace llvm {
  class MemoryBuffer;
}

namespace swift {
  class AbstractStorageDecl;
  class ValueDecl;
  enum class AccessorKind;

namespace ide {

/// Returns true if the input source is fully formed, or false if, for example,
/// a closing brace is missing.
bool isSourceInputComplete(std::unique_ptr<llvm::MemoryBuffer> MemBuf);
bool isSourceInputComplete(StringRef Text);

/// Prints out the USR for the given Decl.
/// \returns true if it failed, false on success.
bool printDeclUSR(const ValueDecl *D, raw_ostream &OS);

/// Prints out the accessor USR for the given storage Decl.
/// \returns true if it failed, false on success.
bool printAccessorUSR(const AbstractStorageDecl *D, AccessorKind AccKind,
                      llvm::raw_ostream &OS);

} // namespace ide
} // namespace swift

#endif // SWIFT_IDE_UTILS_H

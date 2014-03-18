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
class Decl;

namespace ide {

/// Returns true if the input source is fully formed, or false if, for example,
/// a closing brace is missing.
bool isSourceInputComplete(std::unique_ptr<llvm::MemoryBuffer> MemBuf);
bool isSourceInputComplete(StringRef Text);

/// If the declaration has a documentation comment, prints the comment to \p OS
/// in Clang-like XML format.
///
/// \returns true if the declaration has a documentation comment.
bool getDocumentationCommentAsXML(const Decl *D, raw_ostream &OS);

} // namespace ide
} // namespace swift

#endif // SWIFT_IDE_UTILS_H

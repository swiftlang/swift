//===--- CommentConversion.h - Conversion of comments to other formats ----===//
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

#ifndef SWIFT_IDE_COMMENT_CONVERSION_H
#define SWIFT_IDE_COMMENT_CONVERSION_H

#include "swift/Basic/LLVM.h"
#include <memory>

namespace swift {
class Decl;
class DocComment;

namespace ide {

/// If the declaration has a documentation comment, prints the comment to \p OS
/// in Clang-like XML format.
///
/// \returns true if the declaration has a documentation comment.
bool getDocumentationCommentAsXML(const Decl *D, raw_ostream &OS);

/// Converts the given comment to Doxygen.
void getDocumentationCommentAsDoxygen(const DocComment *DC, raw_ostream &OS);

} // namespace ide
} // namespace swift

#endif // SWIFT_IDE_COMMENT_CONVERSION_H


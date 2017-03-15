//===--- CommentConversion.h - Conversion of comments to other formats ----===//
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

#ifndef SWIFT_IDE_COMMENT_CONVERSION_H
#define SWIFT_IDE_COMMENT_CONVERSION_H

#include "swift/Basic/LLVM.h"
#include <memory>
#include <string>

namespace swift {
class Decl;
class DocComment;

namespace ide {

/// If the declaration has a documentation comment, prints the comment to \p OS
/// in Clang-like XML format.
///
/// \returns true if the declaration has a documentation comment.
bool getDocumentationCommentAsXML(const Decl *D, raw_ostream &OS);

/// If the declaration has a documentation comment and a localization key,
/// print it into the given output stream and return true. Else, return false.
bool getLocalizationKey(const Decl *D, raw_ostream &OS);

/// Converts the given comment to Doxygen.
void getDocumentationCommentAsDoxygen(const DocComment *DC, raw_ostream &OS);

/// Extract and normalize text from the given comment.
std::string extractPlainTextFromComment(const StringRef Text);

} // namespace ide
} // namespace swift

#endif // SWIFT_IDE_COMMENT_CONVERSION_H


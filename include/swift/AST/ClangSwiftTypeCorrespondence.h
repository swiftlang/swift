//=- ClangSwiftTypeCorrespondence.h - Relations between Clang & Swift types -=//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2019 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
// This file describes some common relations between Clang types and Swift
// types that are need by the ClangTypeConverter and parts of ClangImporter.
//
// Since ClangTypeConverter is an implementation detail, ClangImporter should
// not depend on ClangTypeConverter.h.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_AST_CLANG_SWIFT_TYPE_CORRESPONDENCE_H
#define SWIFT_AST_CLANG_SWIFT_TYPE_CORRESPONDENCE_H

namespace clang {
class Type;
}

namespace swift {
/// Checks whether a Clang type can be imported as a Swift Optional type.
///
/// For example, a `const uint8_t *` could be imported as
/// `Optional<UnsafePointer<UInt8>>`.
bool canImportAsOptional(const clang::Type *type);
}

#endif /* SWIFT_AST_CLANG_SWIFT_TYPE_CORRESPONDENCE_H */

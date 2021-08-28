//===--- ModuleSourceFileInfo.h ---------------------------------*- C++ -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2021 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_IDE_MODULESOURCEFILEINFO_H
#define SWIFT_IDE_MODULESOURCEFILEINFO_H

#include "swift/AST/RawComment.h"
#include "swift/Basic/BasicSourceInfo.h"
#include "swift/Basic/LLVM.h"

namespace swift {
class ASTContext;
class Decl;
namespace ide {

/// Get the source file path where \p D is declared. Returns an empty string
/// if the information is not available.
StringRef getSourceFilePathForDecl(const Decl *D);

/// Check if the source file of \p info is up-to-date.
///  * \c true if the mtime and the size are the same.
///  * \c true if the interface has hasn't changed.
///  * \c false otherwise.
bool isSourceFileUpToDate(const BasicSourceFileInfo &info, ASTContext &Ctx);


} // namespace ide
} // namespace swift

#endif // SWIFT_IDE_MODULESOURCEFILEINFO_H

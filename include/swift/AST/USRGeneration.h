//===--- USRGeneration.h - Routines for USR generation --------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2016 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See http://swift.org/LICENSE.txt for license information
// See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_AST_USRGENERATION_H
#define SWIFT_AST_USRGENERATION_H

#include "swift/Basic/LLVM.h"

namespace swift {
class AbstractStorageDecl;
class ValueDecl;
enum class AccessorKind;

namespace ide {

/// Prints out the USR for the given Decl.
/// \returns true if it failed, false on success.
bool printDeclUSR(const ValueDecl *D, raw_ostream &OS);

/// Prints out the accessor USR for the given storage Decl.
/// \returns true if it failed, false on success.
bool printAccessorUSR(const AbstractStorageDecl *D, AccessorKind AccKind,
                      llvm::raw_ostream &OS);

} // namespace ide
} // namespace swift

#endif // LLVM_SWIFT_AST_USRGENERATION_H


//===----------------------------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2023 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_REFACTORING_UTILS_H
#define SWIFT_REFACTORING_UTILS_H

#include "swift/AST/ASTContext.h"
#include "swift/AST/Decl.h"
#include "swift/Basic/LLVM.h"

namespace swift {
namespace refactoring {
StringRef correctNameInternal(ASTContext &Ctx, StringRef Name,
                              ArrayRef<ValueDecl *> AllVisibles);

StringRef correctNewDeclName(SourceLoc Loc, DeclContext *DC, StringRef Name);

/// If \p NTD is a protocol, return all the protocols it inherits from. If it's
/// a type, return all the protocols it conforms to.
SmallVector<ProtocolDecl *, 2> getAllProtocols(NominalTypeDecl *NTD);
}
} // namespace swift

#endif

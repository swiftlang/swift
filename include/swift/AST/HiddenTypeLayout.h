//===--- HiddenTypeLayout.h - Abstract member layout analysis --*- C++ -*-===//
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

#ifndef SWIFT_AST_HIDDENTYPELAYOUT_H
#define SWIFT_AST_HIDDENTYPELAYOUT_H

#include "llvm/ADT/ArrayRef.h"
#include "llvm/ADT/STLExtras.h"

namespace swift {

class Decl;
class FileUnit;
class StructDecl;
class VarDecl;

struct AbstractMemberTypeLayoutRequirement {
  StructDecl *ABIExposedStruct;
  VarDecl *StoredProperty;
};

/// Find non-client-visible stored properties whose layouts contribute to this
/// module's client-visible ABI.
void forEachRequiredAbstractMemberTypeLayout(
    llvm::ArrayRef<const FileUnit *> files,
    llvm::function_ref<void(
        const AbstractMemberTypeLayoutRequirement &)> callback);

} // end namespace swift

#endif // SWIFT_AST_HIDDENTYPELAYOUT_H

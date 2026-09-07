//===--- HiddenTypeLayout.h - Hidden type layout analysis ------*- C++ -*-===//
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

#include "swift/AST/Type.h"
#include "llvm/ADT/ArrayRef.h"
#include "llvm/ADT/STLExtras.h"

namespace swift {

class Decl;
class FileUnit;
class ModuleDecl;
class NominalTypeDecl;
class ValueDecl;

enum class HiddenTypeLayoutOrigin {
  ImplementationOnly,
  InternalBridgingHeader,
  RecoveredHiddenType,
};

struct HiddenTypeLayoutRequirement {
  const Decl *LayoutDecl;
  Type HiddenType;
  HiddenTypeLayoutOrigin Origin;
  NominalTypeDecl *ABIExposedType;
  ValueDecl *LayoutAffectingStorage;
};

/// Find hidden types that contribute to this module's client-visible ABI.
void forEachRequiredHiddenTypeLayout(
    const ModuleDecl *module, ArrayRef<const FileUnit *> files,
    llvm::function_ref<void(const HiddenTypeLayoutRequirement &)> callback);

} // end namespace swift

#endif // SWIFT_AST_HIDDENTYPELAYOUT_H

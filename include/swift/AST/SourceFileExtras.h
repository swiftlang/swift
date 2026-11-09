//===--- SourceFileExtras.h - Extra data for a source file ------*- C++ -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2019 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_AST_SOURCEFILEEXTRAS_H
#define SWIFT_AST_SOURCEFILEEXTRAS_H

#include "llvm/ADT/DenseSet.h"

namespace swift {

class ConstructorDecl;
class Decl;
class NominalTypeDecl;

/// Extra information associated with a source file that is lazily created and
/// stored in a separately-allocated side structure.
struct SourceFileExtras {
  /// Set of compatibility memberwise initializers that we have emitted a
  /// warning for.
  llvm::DenseSet<const ConstructorDecl *> DiagnosedCompatMemberwiseInits;

  /// (outermost enclosing declaration, unusable imported type) pairs that we
  /// have emitted an error for, so that each declaration gets at most one.
  llvm::DenseSet<std::pair<const Decl *, const NominalTypeDecl *>>
      DiagnosedUnusableImportedTypes;
};
}

#endif // SWIFT_AST_SOURCEFILEEXTRAS_H

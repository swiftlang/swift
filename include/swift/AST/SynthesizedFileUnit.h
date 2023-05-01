//===--- SynthesizedFileUnit.h - A synthesized file unit --------*- C++ -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2020 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_AST_SYNTHESIZEDFILEUNIT_H
#define SWIFT_AST_SYNTHESIZEDFILEUNIT_H

#include "swift/AST/FileUnit.h"
#include "swift/Basic/Debug.h"

namespace swift {

/// A container for synthesized declarations, attached to a `SourceFile`.
///
/// Currently, only module-level synthesized declarations are supported.
class SynthesizedFileUnit final : public FileUnit {
  /// The parent source file.
  FileUnit &FU;

  /// Synthesized top level declarations.
  TinyPtrVector<Decl *> TopLevelDecls;

  /// A unique identifier representing this file; used to mark private decls
  /// within the file to keep them from conflicting with other files in the
  /// same module.
  mutable Identifier PrivateDiscriminator;

public:
  SynthesizedFileUnit(FileUnit &FU);
  ~SynthesizedFileUnit() = default;

  /// Returns the parent source file.
  FileUnit &getFileUnit() const { return FU; }

  /// Add a synthesized top-level declaration.
  void addTopLevelDecl(Decl *D) { TopLevelDecls.push_back(D); }

  virtual void lookupValue(DeclName name, NLKind lookupKind,
                           OptionSet<ModuleLookupFlags> Flags,
                           SmallVectorImpl<ValueDecl *> &result) const override;

  void lookupObjCMethods(
      ObjCSelector selector,
      SmallVectorImpl<AbstractFunctionDecl *> &results) const override;

  Identifier getDiscriminatorForPrivateDecl(const Decl *D) const override;

  void getTopLevelDecls(SmallVectorImpl<Decl*> &results) const override;

  ArrayRef<Decl *> getTopLevelDecls() const {
    return TopLevelDecls;
  };

  static bool classof(const FileUnit *file) {
    return file->getKind() == FileUnitKind::Synthesized;
  }
  static bool classof(const DeclContext *DC) {
    return isa<FileUnit>(DC) && classof(cast<FileUnit>(DC));
  }
};

} // namespace swift

#endif // SWIFT_AST_SYNTHESIZEDFILEUNIT_H

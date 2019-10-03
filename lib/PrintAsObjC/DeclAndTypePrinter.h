//===--- DeclAndTypePrinter.h - Emit ObjC decls from Swift AST --*- C++ -*-===//
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

#ifndef SWIFT_PRINTASOBJC_DECLANDTYPEPRINTER_H
#define SWIFT_PRINTASOBJC_DECLANDTYPEPRINTER_H

#include "swift/AST/Type.h"
// for OptionalTypeKind
#include "swift/ClangImporter/ClangImporter.h"

namespace clang {
  class NamedDecl;
} // end namespace clang

namespace swift {

/// Responsible for printing a Swift Decl or Type in Objective-C, to be
/// included in a Swift module's ObjC compatibility header.
class DeclAndTypePrinter {
public:
  using DelayedMemberSet = llvm::SmallSetVector<const ValueDecl *, 32>;

private:
  class Implementation;
  friend class Implementation;

  ModuleDecl &M;
  raw_ostream &os;
  const DelayedMemberSet &delayedMembers;
  AccessLevel minRequiredAccess;

  struct CTypeInfo {
    StringRef name;
    bool canBeNullable;
  };

  /// A map from {Module, TypeName} pairs to {C name, C nullability} pairs.
  ///
  /// This is populated on first use with a list of known Swift types that are
  /// translated directly by the ObjC printer instead of structurally, allowing
  /// it to do things like map 'Int' to 'NSInteger' and 'Float' to 'float'.
  /// In some sense it's the reverse of the ClangImporter's MappedTypes.def.
  llvm::DenseMap<std::pair<Identifier, Identifier>, CTypeInfo> specialNames;

  /// The name 'CFTypeRef'.
  ///
  /// Cached for convenience.
  Identifier ID_CFTypeRef;

  Implementation getImpl();

public:
  DeclAndTypePrinter(ModuleDecl &mod, raw_ostream &out,
                     DelayedMemberSet &delayed, AccessLevel access)
    : M(mod), os(out), delayedMembers(delayed), minRequiredAccess(access) {}

  /// Returns true if \p VD should be included in a compatibility header for
  /// the options the printer was constructed with.
  bool shouldInclude(const ValueDecl *VD);

  void print(const Decl *D);
  void print(Type ty);

  /// Prints a category declaring the given members.
  ///
  /// All members must have the same parent type. The list must not be empty.
  void
  printAdHocCategory(llvm::iterator_range<const ValueDecl * const *> members);

  /// Returns the name of an <os/object.h> type minus the leading "OS_",
  /// or an empty string if \p decl is not an <os/object.h> type.
  static StringRef maybeGetOSObjectBaseName(const clang::NamedDecl *decl);
};

} // end namespace swift

#endif

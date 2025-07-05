//===--- USRGeneration.h - Routines for USR generation ----------*- C++ -*-===//
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
//
// Unique Symbol References (USRs) provide a textual encoding for
// declarations. These are used for indexing, analogous to how mangled names
// are used in object files.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_AST_USRGENERATION_H
#define SWIFT_AST_USRGENERATION_H

#include "swift/Basic/LLVM.h"

#include <string>

namespace swift {
class Decl;
class AbstractStorageDecl;
class ValueDecl;
class ExtensionDecl;
class ModuleEntity;
enum class AccessorKind;
class Type;

namespace ide {

/// Prints out the USR for the Type.
/// \returns true if it failed, false on success.
bool printTypeUSR(Type Ty, raw_ostream &OS);

/// Prints out the USR for the Type of the given decl.
/// \returns true if it failed, false on success.
bool printDeclTypeUSR(const ValueDecl *D, raw_ostream &OS);

/// Prints out the USR for the given ValueDecl.
/// @param distinguishSynthesizedDecls Whether to use the USR of the
/// synthesized declaration instead of the USR of the underlying Clang USR.
/// @param useSwiftUSR Whether to generate a Swift USR for all Clang declarations as well.
/// \returns true if it failed, false on success.
bool printValueDeclUSR(const ValueDecl *D, raw_ostream &OS,
                       bool distinguishSynthesizedDecls = false,
                       bool useSwiftUSR = false);

/// Prints out the Swift USR for the given ValueDecl regardless of its source (Swift or Clang).
/// Equivalent to `printValueDeclUSR(D, OS, false, /*useSwiftUSR=*/true)`
inline bool printValueDeclSwiftUSR(const ValueDecl *D, raw_ostream &OS) {
  return printValueDeclUSR(D, OS, /*distinguishSynthesizedDecls=*/false,
                           /*useSwiftUSR=*/true);
}

/// Prints out the USR for the given ModuleEntity.
/// In case module aliasing is used, it prints the real module name. For example,
/// if a file has `import Foo` and `-module-alias Foo=Bar` is passed, treat Foo as
/// an alias and Bar as the real module name as its dependency. Note that the
/// aliasing only applies to Swift modules.
/// \returns true if it failed, false on success.
bool printModuleUSR(ModuleEntity Mod, raw_ostream &OS);

/// Prints out the accessor USR for the given storage Decl.
/// \returns true if it failed, false on success.
bool printAccessorUSR(const AbstractStorageDecl *D, AccessorKind AccKind,
                      llvm::raw_ostream &OS);

/// Prints out the extension USR for the given extension Decl.
/// \returns true if it failed, false on success.
bool printExtensionUSR(const ExtensionDecl *ED, raw_ostream &OS);

/// Prints out the USR for the given Decl.
/// @param distinguishSynthesizedDecls Whether to use the USR of the
/// synthesized declaration instead of the USR of the underlying Clang USR.
/// \returns true if it failed, false on success.
bool printDeclUSR(const Decl *D, raw_ostream &OS,
                  bool distinguishSynthesizedDecls = false);

/// Demangle a mangle-name-based USR to a human readable name.
std::string demangleUSR(StringRef mangled);

} // namespace ide
} // namespace swift

#endif // LLVM_SWIFT_AST_USRGENERATION_H


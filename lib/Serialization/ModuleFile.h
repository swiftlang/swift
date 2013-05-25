//===--- ModuleFile.h - Info about a loaded serialized module ---*- C++ -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2015 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See http://swift.org/LICENSE.txt for license information
// See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_SERIALIZATION_MODULEFILE_H
#define SWIFT_SERIALIZATION_MODULEFILE_H

#include "ModuleFormat.h"
#include "swift/AST/Type.h"
#include "swift/Basic/PointerIntUnion.h"
#include "swift/Basic/LLVM.h"
#include "llvm/ADT/ArrayRef.h"
#include "llvm/ADT/OwningPtr.h"
#include "llvm/Bitcode/BitstreamReader.h"

namespace llvm {
  class BitstreamCursor;
  class BitstreamReader;
  class MemoryBuffer;
}

namespace swift {
class Decl;
class Module;

/// Describes whether a loaded module can be used.
enum class ModuleStatus {
  /// The module is valid.
  Valid,

  /// The module contains pointers to source files, which should be loaded
  /// instead.
  ///
  /// This is a bring-up hack and will eventually go away.
  FallBackToTranslationUnit,

  /// The module file format is too new to be used by this version of the
  /// compiler.
  FormatTooNew,

  /// The module file is malformed in some way.
  Malformed
};

/// A serialized module, along with the tools to access it.
class ModuleFile {
  /// A reference back to the AST representation of the module.
  Module *ModuleContext;

  /// The module file data.
  llvm::OwningPtr<llvm::MemoryBuffer> InputFile;

  /// The reader attached to InputFile.
  llvm::BitstreamReader InputReader;

  /// The cursor used to lazily load things from the file.
  llvm::BitstreamCursor DeclTypeCursor;

  /// Paths to the source files used to build this module.
  SmallVector<StringRef, 4> SourcePaths;

  /// Decls referenced by this module.
  std::vector<PointerIntUnion<const Decl *, serialization::BitOffset,
                              uint64_t>> Decls;

  /// Types referenced by this module.
  std::vector<PointerIntUnion<Type, serialization::BitOffset, uint64_t>> Types;

  /// Whether this module file can be used.
  ModuleStatus Status;

  /// Constructs an new module and validates it.
  ModuleFile(llvm::OwningPtr<llvm::MemoryBuffer> &&input);

  /// Convenience function for module loading.
  void error(ModuleStatus issue = ModuleStatus::Malformed) {
    assert(issue != ModuleStatus::Valid &&
           issue != ModuleStatus::FallBackToTranslationUnit);
    Status = issue;
  }

  /// Returns the decl with the given ID, deserializing it if needed.
  const Decl *getDecl(serialization::DeclID DID);

  /// Returns the type with the current ID, deserializing it if needed.
  Type getType(serialization::TypeID TID);

public:
  /// Loads a module from the given memory buffer.
  ///
  /// \param input A memory buffer containing the serialized module data.
  ///              The created module takes ownership of the buffer, even if
  ///              there's an error in loading.
  /// \param[out] module The loaded module.
  /// \returns Whether the module was successfully loaded, or what went wrong
  ///          if it was not.
  static ModuleStatus load(llvm::OwningPtr<llvm::MemoryBuffer> &&input,
                           llvm::OwningPtr<ModuleFile> &module) {
    module.reset(new ModuleFile(std::move(input)));
    return module->getStatus();
  }

  /// Associates this module file with an AST module.
  void associateWithModule(Module *module) {
    assert(!ModuleContext && "already associated with an AST module");
    ModuleContext = module;

    // Force all the decls for testing purposes.
    for (serialization::DeclID i = 1; i <= Decls.size(); ++i)
      assert(getDecl(i) != nullptr);
  }

  /// Checks whether this module can be used.
  ModuleStatus getStatus() const { return Status; }

  /// Returns paths to the source files that were used to build this module.
  ArrayRef<StringRef> getInputSourcePaths() const {
    assert(getStatus() == ModuleStatus::Valid ||
           getStatus() == ModuleStatus::FallBackToTranslationUnit);
    return SourcePaths;
  }
};

} // end namespace swift

#endif

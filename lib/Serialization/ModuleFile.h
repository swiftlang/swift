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
#include "swift/AST/Identifier.h"
#include "swift/AST/TypeLoc.h"
#include "swift/Basic/Fixnum.h"
#include "swift/Basic/LLVM.h"
#include "llvm/ADT/ArrayRef.h"
#include "llvm/ADT/DenseMap.h"
#include "llvm/ADT/OwningPtr.h"
#include "llvm/Bitcode/BitstreamReader.h"

namespace llvm {
  class BitstreamCursor;
  class BitstreamReader;
  class MemoryBuffer;
}

namespace swift {
class Decl;
class DeclContext;
class Module;
class Pattern;
class ValueDecl;

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

  /// The module file depends on another module that can't be loaded.
  MissingDependency,

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

  /// The data blob containing all of the module's identifiers.
  StringRef IdentifierData;

  /// Paths to the source files used to build this module.
  SmallVector<StringRef, 4> SourcePaths;

public:
  /// Represents another module that has been imported as a dependency.
  ///
  /// If \c Mod is null, the dependency hasn't been resolved yet.
  class Dependency {
  public:
    Module *Mod;
    StringRef Name;

    /*implicit*/ Dependency(StringRef name) : Mod(nullptr), Name(name) {}
  };

private:
  /// All modules this module depends on.
  SmallVector<Dependency, 8> Dependencies;

  /// Decls referenced by this module.
  std::vector<PointerUnion<Decl*, serialization::BitOffset>> Decls;

  /// Types referenced by this module.
  std::vector<PointerUnion<Type, serialization::BitOffset>> Types;

  /// Represents an identifier that may or may not have been deserialized yet.
  ///
  /// If \c Offset is non-zero, the identifier has not been loaded yet.
  class SerializedIdentifier {
  public:
    Identifier Ident;
    serialization::BitOffset Offset;

    template <typename IntTy>
    /*implicit*/ SerializedIdentifier(IntTy rawOffset)
      : Offset(rawOffset) {}
  };

  /// Types referenced by this module.
  std::vector<SerializedIdentifier> Identifiers;

  /// All top-level decls in this module.
  // FIXME: A single identifier may refer to multiple decls.
  llvm::DenseMap<Identifier, serialization::DeclID> TopLevelIDs;

  /// FIXME: HACK: an array of the top-level decl IDs.
  std::vector<serialization::DeclID> RawTopLevelIDs;

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

  /// Recursively reads a pattern from \c DeclTypeCursor.
  ///
  /// If the record at the cursor is not a pattern, returns null.
  Pattern *maybeReadPattern();

  /// Creates an array of types from the given IDs.
  ///
  /// The returned array is owned by the ASTContext.
  MutableArrayRef<TypeLoc> getTypes(ArrayRef<uint64_t> rawTypeIDs);

  /// Reads members of a DeclContext from \c DeclTypeCursor.
  ///
  /// The returned array is owned by the ASTContext.
  /// Returns Nothing if there is an error.
  ///
  /// Note: this destroys the cursor's position in the stream. Furthermore,
  /// because it reads from the cursor, it is not possible to reset the cursor
  /// after reading. Nothing should ever follow a DECL_CONTEXT block.
  Optional<MutableArrayRef<Decl *>> readMembers();

  /// Returns the decl context with the given ID, deserializing it if needed.
  DeclContext *getDeclContext(serialization::DeclID DID);

  /// Controls how a Decl is deserialized.
  enum DeclDeserializationOptions {
    /// Don't set the decl's context. It will be set by the caller instead.
    ///
    /// This is useful for declarations that reference each other at
    /// construction time.
    SkipContext = 0x1
  };

  /// Returns the decl with the given ID, deserializing it if needed.
  Decl *getDecl(serialization::DeclID DID,
                DeclDeserializationOptions opts = {});

  /// Returns the type with the given ID, deserializing it if needed.
  Type getType(serialization::TypeID TID);

  /// Returns the identifier with the given ID, deserializing it if needed.
  Identifier getIdentifier(serialization::IdentifierID IID);

  /// Populates TopLevelIDs for name lookup.
  void buildTopLevelDeclMap();

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
  ///
  /// Returns false if the association failed.
  bool associateWithModule(Module *module);

  /// Checks whether this module can be used.
  ModuleStatus getStatus() const { return Status; }

  /// Returns paths to the source files that were used to build this module.
  ArrayRef<StringRef> getInputSourcePaths() const {
    assert(getStatus() == ModuleStatus::Valid ||
           getStatus() == ModuleStatus::FallBackToTranslationUnit);
    return SourcePaths;
  }

  /// Returns the list of modules this module depends on.
  ArrayRef<Dependency> getDependencies() const {
    return Dependencies;
  }

  /// Searches the module's top-level decls for the given identifier.
  void lookupValue(Identifier name, SmallVectorImpl<ValueDecl*> &results);
};

} // end namespace swift

#endif

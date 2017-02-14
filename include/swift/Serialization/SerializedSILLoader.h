//===--- SerializedSILLoader.h - Handle SIL section in modules --*- C++ -*-===//
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

#ifndef SWIFT_SERIALIZATION_SILLOADER_H
#define SWIFT_SERIALIZATION_SILLOADER_H

#include "swift/AST/Decl.h"
#include "swift/AST/Identifier.h"
#include "swift/SIL/SILDeclRef.h"
#include "swift/SIL/SILLinkage.h"
#include <memory>
#include <vector>

namespace swift {
class ASTContext;
class FileUnit;
class ModuleDecl;
class SILDeserializer;
class SILFunction;
class SILGlobalVariable;
class SILModule;
class SILVTable;
class SILWitnessTable;
class SILDefaultWitnessTable;

/// Maintains a list of SILDeserializer, one for each serialized modules
/// in ASTContext. It provides lookupSILFunction that will perform lookup
/// on each SILDeserializer.
class SerializedSILLoader {
public:
  class Callback {
  public:
    /// Observe that we deserialized a function declaration.
    virtual void didDeserialize(ModuleDecl *M, SILFunction *fn) {}

    /// Observe that we successfully deserialized a function body.
    virtual void didDeserializeFunctionBody(ModuleDecl *M, SILFunction *fn) {}

    /// Observe that we successfully deserialized a witness table's entries.
    virtual void didDeserializeWitnessTableEntries(ModuleDecl *M,
                                                   SILWitnessTable *wt) {}

    /// Observe that we successfully deserialized a default witness table's
    /// entries.
    virtual void didDeserializeDefaultWitnessTableEntries(ModuleDecl *M,
                                                  SILDefaultWitnessTable *wt) {}

    /// Observe that we deserialized a global variable declaration.
    virtual void didDeserialize(ModuleDecl *M, SILGlobalVariable *var) {}

    /// Observe that we deserialized a v-table declaration.
    virtual void didDeserialize(ModuleDecl *M, SILVTable *vtable) {}

    /// Observe that we deserialized a witness-table declaration.
    virtual void didDeserialize(ModuleDecl *M, SILWitnessTable *wtable) {}

    /// Observe that we deserialized a default witness-table declaration.
    virtual void didDeserialize(ModuleDecl *M, SILDefaultWitnessTable *wtable) {}

    virtual ~Callback() = default;
  private:
    virtual void _anchor();
  };

private:
  std::vector<std::unique_ptr<SILDeserializer> > LoadedSILSections;

  explicit SerializedSILLoader(ASTContext &ctx, SILModule *SILMod,
                               Callback *callback);

public:
  /// Create a new loader.
  ///
  /// \param callback - not owned by the loader
  static std::unique_ptr<SerializedSILLoader> create(ASTContext &ctx,
                                                     SILModule *SILMod,
                                                     Callback *callback) {
    return std::unique_ptr<SerializedSILLoader>(
      new SerializedSILLoader(ctx, SILMod, callback));
  }
  ~SerializedSILLoader();

  SILFunction *lookupSILFunction(SILFunction *Callee);
  SILFunction *
  lookupSILFunction(StringRef Name, bool declarationOnly = false,
                    Optional<SILLinkage> linkage = None);
  bool hasSILFunction(StringRef Name, Optional<SILLinkage> linkage = None);
  SILVTable *lookupVTable(Identifier Name);
  SILVTable *lookupVTable(const ClassDecl *C) {
    return lookupVTable(C->getName());
  }
  SILWitnessTable *lookupWitnessTable(SILWitnessTable *C);
  SILDefaultWitnessTable *lookupDefaultWitnessTable(SILDefaultWitnessTable *C);

  /// Invalidate the cached entries for deserialized SILFunctions.
  void invalidateCaches();

  bool invalidateFunction(SILFunction *F);

  /// Deserialize all SILFunctions, VTables, and WitnessTables in all
  /// SILModules.
  void getAll();

  /// Deserialize all SILFunctions, VTables, and WitnessTables for
  /// a given Module.
  void getAllForModule(Identifier Mod, FileUnit *PrimaryFile);

  /// Deserialize all SILFunctions in all SILModules.
  void getAllSILFunctions();

  /// Deserialize all VTables in all SILModules.
  void getAllVTables();

  /// Deserialize all WitnessTables in all SILModules.
  void getAllWitnessTables();

  /// Deserialize all DefaultWitnessTables in all SILModules.
  void getAllDefaultWitnessTables();

  SerializedSILLoader(const SerializedSILLoader &) = delete;
  SerializedSILLoader(SerializedSILLoader &&) = delete;
  SerializedSILLoader &operator=(const SerializedSILLoader &) = delete;
  SerializedSILLoader &operator=(SerializedSILLoader &&) = delete;
};

} // end namespace swift

#endif

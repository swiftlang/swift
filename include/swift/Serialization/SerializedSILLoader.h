//===--- SerializedSILLoader.h - Handle SIL section in modules --*- c++ -*-===//
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

#ifndef SWIFT_SERIALIZATION_SILLOADER_H
#define SWIFT_SERIALIZATION_SILLOADER_H

namespace swift {
class ASTContext;
class Module;
class SILDeserializer;
class SILFunction;
class SILGlobalVariable;
class SILModule;
class SILVTable;

/// Maintains a list of SILDeserializer, one for each serialized modules
/// in ASTContext. It provides lookupSILFunction that will perform lookup
/// on each SILDeserializer.
class SerializedSILLoader {
public:
  class Callback {
  public:
    /// Observe that we deserialized a function declaration.
    virtual void didDeserialize(Module *M, SILFunction *fn) {}

    /// Observe that we successfully deserialized a function body.
    virtual void didDeserializeBody(Module *M, SILFunction *fn) {}

    /// Observe that we deserialized a global variable declaration.
    virtual void didDeserialize(Module *M, SILGlobalVariable *var) {}

    /// Observe that we deserialized a v-table declaration.
    virtual void didDeserialize(Module *M, SILVTable *vtable) {}

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
  static SerializedSILLoader *create(ASTContext &ctx, SILModule *SILMod,
                                     Callback *callback) {
    return new SerializedSILLoader(ctx, SILMod, callback);
  }
  ~SerializedSILLoader();

  SILFunction *lookupSILFunction(SILFunction *Callee);
  SILVTable *lookupVTable(Identifier Name);
  /// Deserialize all VTables in all SILModules.
  void getAllVTables();

  SerializedSILLoader(const SerializedSILLoader &) = delete;
  SerializedSILLoader(SerializedSILLoader &&) = delete;
  SerializedSILLoader &operator=(const SerializedSILLoader &) = delete;
  SerializedSILLoader &operator=(SerializedSILLoader &&) = delete;
};

} // end namespace swift

#endif

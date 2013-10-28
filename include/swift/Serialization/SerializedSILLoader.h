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
class SILDeserializer;
class SILFunction;
class SILModule;
class SILVTable;

/// Maintains a list of SILDeserializer, one for each serialized modules
/// in ASTContext. It provides lookupSILFunction that will perform lookup
/// on each SILDeserializer.
class SerializedSILLoader {
private:
  std::vector<std::unique_ptr<SILDeserializer> > LoadedSILSections;

  explicit SerializedSILLoader(ASTContext &ctx, SILModule *SILMod);

public:
  static SerializedSILLoader *create(ASTContext &ctx, SILModule *SILMod) {
    return new SerializedSILLoader(ctx, SILMod);
  }
  SILFunction *lookupSILFunction(SILFunction *Callee);
  SILVTable *lookupVTable(Identifier Name);
  /// Deserialize all VTables in all SILModules.
  void getAllVTables();

  ~SerializedSILLoader();

  SerializedSILLoader(const SerializedSILLoader &) = delete;
  SerializedSILLoader(SerializedSILLoader &&) = delete;
  SerializedSILLoader &operator=(const SerializedSILLoader &) = delete;
  SerializedSILLoader &operator=(SerializedSILLoader &&) = delete;
};

} // end namespace swift

#endif

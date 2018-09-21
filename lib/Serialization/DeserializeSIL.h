//===--- DeserializeSIL.h - Read SIL ----------------------------*- C++ -*-===//
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

#include "SILFormat.h"
#include "swift/SIL/SILModule.h"
#include "swift/Serialization/ModuleFile.h"
#include "swift/Serialization/SerializedSILLoader.h"

#include "llvm/ADT/DenseMap.h"
#include "llvm/Support/SaveAndRestore.h"

namespace llvm {
  template <typename Info> class OnDiskIterableChainedHashTable;
}

namespace swift {
  class SILDeserializer {
    using TypeID = serialization::TypeID;
    
    ModuleFile *MF;
    SILModule &SILMod;
    DeserializationNotificationHandlerSet *Callback;

    /// The cursor used to lazily load SILFunctions.
    llvm::BitstreamCursor SILCursor;
    llvm::BitstreamCursor SILIndexCursor;

    class FuncTableInfo;
    using SerializedFuncTable =
      llvm::OnDiskIterableChainedHashTable<FuncTableInfo>;

    std::unique_ptr<SerializedFuncTable> FuncTable;
    MutableArrayRef<ModuleFile::PartiallySerialized<SILFunction*>> Funcs;

    std::unique_ptr<SerializedFuncTable> VTableList;
    MutableArrayRef<ModuleFile::Serialized<SILVTable*>> VTables;

    std::unique_ptr<SerializedFuncTable> GlobalVarList;
    MutableArrayRef<ModuleFile::Serialized<SILGlobalVariable*>> GlobalVars;

    std::unique_ptr<SerializedFuncTable> WitnessTableList;
    MutableArrayRef<ModuleFile::PartiallySerialized<SILWitnessTable *>>
    WitnessTables;

    std::unique_ptr<SerializedFuncTable> DefaultWitnessTableList;
    MutableArrayRef<ModuleFile::PartiallySerialized<SILDefaultWitnessTable *>>
    DefaultWitnessTables;

    MutableArrayRef<ModuleFile::PartiallySerialized<SILProperty *>>
    Properties;

    /// A declaration will only
    llvm::DenseMap<NormalProtocolConformance *, SILWitnessTable *>
    ConformanceToWitnessTableMap;

    /// Data structures used to perform name lookup for local values.
    llvm::DenseMap<uint32_t, ValueBase*> LocalValues;
    llvm::DenseMap<uint32_t, ValueBase*> ForwardLocalValues;
    serialization::ValueID LastValueID = 0;

    /// Data structures used to perform lookup of basic blocks.
    llvm::DenseMap<unsigned, SILBasicBlock*> BlocksByID;
    llvm::DenseMap<SILBasicBlock*, unsigned> UndefinedBlocks;
    unsigned BasicBlockID = 0;

    /// Return the SILBasicBlock of a given ID.
    SILBasicBlock *getBBForReference(SILFunction *Fn, unsigned ID);
    SILBasicBlock *getBBForDefinition(SILFunction *Fn, SILBasicBlock *Prev,
                                      unsigned ID);

    /// Read a SIL function.
    SILFunction *readSILFunction(serialization::DeclID, SILFunction *InFunc,
                                 StringRef Name, bool declarationOnly,
                                 bool errorIfEmptyBody = true);
    /// Read a SIL function.
    llvm::Expected<SILFunction *>
    readSILFunctionChecked(serialization::DeclID, SILFunction *InFunc,
                           StringRef Name, bool declarationOnly,
                           bool errorIfEmptyBody = true);

    /// Read a SIL basic block within a given SIL function.
    SILBasicBlock *readSILBasicBlock(SILFunction *Fn,
                                     SILBasicBlock *Prev,
                                     SmallVectorImpl<uint64_t> &scratch);
    /// Read a SIL instruction within a given SIL basic block.
    bool readSILInstruction(SILFunction *Fn, SILBasicBlock *BB,
                            SILBuilder &Builder,
                            unsigned RecordKind,
                            SmallVectorImpl<uint64_t> &scratch);

    /// Read the SIL function table.
    std::unique_ptr<SerializedFuncTable>
    readFuncTable(ArrayRef<uint64_t> fields, StringRef blobData);

    /// When an instruction or block argument is defined, this method is used to
    /// register it and update our symbol table.
    void setLocalValue(ValueBase *Value, serialization::ValueID Id);
    /// Get a reference to a local value with the specified ID and type.
    SILValue getLocalValue(serialization::ValueID Id,
                           SILType Type);

    SILFunction *getFuncForReference(StringRef Name, SILType Ty);
    SILFunction *getFuncForReference(StringRef Name);
    SILVTable *readVTable(serialization::DeclID);
    SILGlobalVariable *getGlobalForReference(StringRef Name);
    SILGlobalVariable *readGlobalVar(StringRef Name);
    SILWitnessTable *readWitnessTable(serialization::DeclID,
                                      SILWitnessTable *existingWt);
    void readWitnessTableEntries(
           llvm::BitstreamEntry &entry,
           std::vector<SILWitnessTable::Entry> &witnessEntries,
           std::vector<SILWitnessTable::ConditionalConformance>
             &conditionalConformances);
    SILProperty *readProperty(serialization::DeclID);
    SILDefaultWitnessTable *
    readDefaultWitnessTable(serialization::DeclID,
                            SILDefaultWitnessTable *existingWt);

    Optional<KeyPathPatternComponent>
    readKeyPathComponent(ArrayRef<uint64_t> ListOfValues, unsigned &nextValue);
    
public:
    Identifier getModuleIdentifier() const {
      return MF->getAssociatedModule()->getName();
    }
    FileUnit *getFile() const {
      return MF->getFile();
    }
    SILFunction *lookupSILFunction(SILFunction *InFunc);
    SILFunction *lookupSILFunction(StringRef Name,
                                   bool declarationOnly = false);
    bool hasSILFunction(StringRef Name, Optional<SILLinkage> Linkage = None);
    SILVTable *lookupVTable(Identifier Name);
    SILWitnessTable *lookupWitnessTable(SILWitnessTable *wt);
    SILDefaultWitnessTable *
    lookupDefaultWitnessTable(SILDefaultWitnessTable *wt);

    /// Invalidate all cached SILFunctions.
    void invalidateFunctionCache();

    /// Invalidate a specific cached SILFunction.
    bool invalidateFunction(SILFunction *F);

    /// Deserialize all SILFunctions, VTables, WitnessTables, and
    /// DefaultWitnessTables inside the module, and add them to SILMod.
    ///
    /// TODO: Globals.
    void getAll(bool UseCallback = true) {
      llvm::SaveAndRestore<DeserializationNotificationHandlerSet *> SaveCB(
          Callback);

      if (!UseCallback)
        Callback = nullptr;

      getAllSILFunctions();
      getAllSILGlobalVariables();
      getAllVTables();
      getAllWitnessTables();
      getAllDefaultWitnessTables();
      getAllProperties();
    }

    /// Deserialize all SILFunctions inside the module and add them to SILMod.
    void getAllSILFunctions();

    /// Deserialize all SILGlobalVariables inside the module and add them to
    /// SILMod.
    void getAllSILGlobalVariables();

    /// Deserialize all VTables inside the module and add them to SILMod.
    void getAllVTables();

    /// Deserialize all WitnessTables inside the module and add them to SILMod.
    void getAllWitnessTables();

    /// Deserialize all DefaultWitnessTables inside the module and add them
    /// to SILMod.
    void getAllDefaultWitnessTables();

    /// Deserialize all Property descriptors inside the module and add them
    /// to SILMod.
    void getAllProperties();

    SILDeserializer(ModuleFile *MF, SILModule &M,
                    DeserializationNotificationHandlerSet *callback);

    // Out of line to avoid instantiation OnDiskChainedHashTable here.
    ~SILDeserializer();
  };
} // end namespace swift

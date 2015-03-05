//===--- DeserializeSIL.h - Read SIL ---------------------------*- C++ -*--===//
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

#include "swift/Serialization/ModuleFile.h"
#include "SILFormat.h"
#include "swift/SIL/SILModule.h"
#include "swift/Serialization/SerializedSILLoader.h"

#include "llvm/ADT/DenseMap.h"
#include "llvm/Support/SaveAndRestore.h"

namespace llvm {
  template <typename Info> class OnDiskIterableChainedHashTable;
}

namespace swift {
  class SILDeserializer {
    ModuleFile *MF;
    SILModule &SILMod;
    SerializedSILLoader::Callback *Callback;

    /// The cursor used to lazily load SILFunctions.
    llvm::BitstreamCursor SILCursor;
    llvm::BitstreamCursor SILIndexCursor;

    class FuncTableInfo;
    using SerializedFuncTable =
      llvm::OnDiskIterableChainedHashTable<FuncTableInfo>;

    std::unique_ptr<SerializedFuncTable> FuncTable;
    std::vector<ModuleFile::PartiallySerialized<SILFunction*>> Funcs;

    std::unique_ptr<SerializedFuncTable> VTableList;
    std::vector<ModuleFile::Serialized<SILVTable*>> VTables;

    std::unique_ptr<SerializedFuncTable> GlobalVarList;
    std::vector<ModuleFile::Serialized<SILGlobalVariable*>> GlobalVars;

    std::unique_ptr<SerializedFuncTable> WitnessTableList;
    std::vector<ModuleFile::PartiallySerialized<SILWitnessTable *>>
    WitnessTables;

    /// A declaration will only
    llvm::DenseMap<NormalProtocolConformance *, SILWitnessTable *>
    ConformanceToWitnessTableMap;

    /// Data structures used to perform name lookup for local values.
    llvm::DenseMap<uint32_t, ValueBase*> LocalValues;
    llvm::DenseMap<uint32_t, std::vector<SILValue>> ForwardMRVLocalValues;
    serialization::ValueID LastValueID = 0;

    /// Data structures used to perform lookup of basic blocks.
    llvm::DenseMap<unsigned, SILBasicBlock*> BlocksByID;
    llvm::DenseMap<SILBasicBlock*, unsigned> UndefinedBlocks;
    unsigned BasicBlockID = 0;

    /// Return the SILBasicBlock of a given ID.
    SILBasicBlock *getBBForReference(SILFunction *Fn, unsigned ID);
    SILBasicBlock *getBBForDefinition(SILFunction *Fn, unsigned ID);

    /// Read a SIL function.
    SILFunction *readSILFunction(serialization::DeclID, SILFunction *InFunc,
                                 StringRef Name, bool declarationOnly,
                                 bool errorIfEmptyBody = true);
    /// Read a SIL basic block within a given SIL function.
    SILBasicBlock *readSILBasicBlock(SILFunction *Fn,
                                     SmallVectorImpl<uint64_t> &scratch);
    /// Read a SIL instruction within a given SIL basic block.
    bool readSILInstruction(SILFunction *Fn, SILBasicBlock *BB,
                            unsigned RecordKind,
                            SmallVectorImpl<uint64_t> &scratch);

    /// Read the SIL function table.
    std::unique_ptr<SerializedFuncTable>
    readFuncTable(ArrayRef<uint64_t> fields, StringRef blobData);

    /// When an instruction or block argument is defined, this method is used to
    /// register it and update our symbol table.
    void setLocalValue(ValueBase *Value, serialization::ValueID Id);
    /// Get a reference to a local value with the specified ID and type.
    SILValue getLocalValue(serialization::ValueID Id, unsigned ResultNum,
                           SILType Type);

    SILFunction *getFuncForReference(StringRef Name, SILType Ty);
    SILFunction *getFuncForReference(StringRef Name);
    SILFunction *lookupSILFunction(StringRef Name);
    SILVTable *readVTable(serialization::DeclID);
    SILGlobalVariable *getGlobalForReference(StringRef Name);
    SILGlobalVariable *readGlobalVar(StringRef Name);
    SILWitnessTable *readWitnessTable(serialization::DeclID,
                                      SILWitnessTable *existingWt,
                                      bool declarationOnly);

public:
    Identifier getModuleIdentifier() const {
      return MF->getAssociatedModule()->getName();
    }
    SILFunction *lookupSILFunction(SILFunction *InFunc);
    SILVTable *lookupVTable(Identifier Name);
    SILWitnessTable *lookupWitnessTable(SILWitnessTable *wt);

    /// Invalidate all cached SILFunctions.
    void invalidateFunctionCache();

    /// Deserialize all SILFunctions, VTables, and WitnessTables inside the
    /// module and add them to SILMod.
    ///
    /// TODO: Globals.
    void getAll(bool UseCallback = true) {
      llvm::SaveAndRestore<SerializedSILLoader::Callback *> SaveCB(Callback);

      if (!UseCallback)
        Callback = nullptr;

      getAllSILFunctions();
      getAllVTables();
      getAllWitnessTables();
    }

    /// Deserialize all SILFunctions inside the module and add them to SILMod.
    void getAllSILFunctions();

    /// Deserialize all VTables inside the module and add them to SILMod.
    void getAllVTables();

    /// Deserialize all WitnessTables inside the module and add them to SILMod.
    void getAllWitnessTables();

    SILDeserializer(ModuleFile *MF, SILModule &M,
                    SerializedSILLoader::Callback *callback);

    // Out of line to avoid instantiation OnDiskChainedHashTable here.
    ~SILDeserializer();
  };
} // end namespace swift

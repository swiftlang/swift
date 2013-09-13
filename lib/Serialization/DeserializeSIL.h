//===--- DeserializeSIL.h - Read SIL -------------------------------------===//
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

#include "ModuleFile.h"
#include "swift/SIL/SILModule.h"

// This template should eventually move to llvm/Support.
namespace clang {
  template <typename Info>
  class OnDiskChainedHashTable;
}

namespace swift {
  class SILDeserializer {
    ModuleFile *MF;
    SILModule &SILMod;

    /// The cursor used to lazily load SILFunctions.
    llvm::BitstreamCursor SILCursor;
    llvm::BitstreamCursor SILIndexCursor;

    class FuncTableInfo;
    using SerializedFuncTable = clang::OnDiskChainedHashTable<FuncTableInfo>;
    std::unique_ptr<SerializedFuncTable> FuncTable;

    std::vector<ModuleFile::Serialized<SILFunction*>> Funcs;

    SILFunction *readSILFunction(serialization::DeclID);
    void readSILBasicBlock(SILFunction *F);
    void readSILInstruction(SILBasicBlock *BB);

    std::unique_ptr<SerializedFuncTable>
    readFuncTable(ArrayRef<uint64_t> fields, StringRef blobData);

public:
    SILFunction *lookupSILFunction(Identifier name);
    SILDeserializer(ModuleFile *MF, SILModule &M);

    // Out of line to avoid instantiation OnDiskChainedHashTable here.
    ~SILDeserializer();
  };
} // end namespace swift

//===--- SerializeSIL.cpp - Read and write SIL ----------------------------===//
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

#include "SILFormat.h"
#include "Serialization.h"
#include "swift/AST/Module.h"
#include "swift/SIL/SILArgument.h"
#include "swift/SIL/SILModule.h"

// This is a template-only header; eventually it should move to llvm/Support.
#include "clang/Basic/OnDiskHashTable.h"

#include "llvm/ADT/StringExtras.h"

using namespace swift;
using namespace swift::serialization;
using namespace swift::serialization::sil_block;

namespace {
    /// Used to serialize the on-disk func hash table.
  class FuncTableInfo {
  public:
    using key_type = Identifier;
    using key_type_ref = key_type;
    using data_type = DeclID;
    using data_type_ref = const data_type &;

    uint32_t ComputeHash(key_type_ref key) {
      assert(!key.empty());
      return llvm::HashString(key.str());
    }

    std::pair<unsigned, unsigned> EmitKeyDataLength(raw_ostream &out,
                                                    key_type_ref key,
                                                    data_type_ref data) {
      using namespace clang::io;
      uint32_t keyLength = key.str().size();
      uint32_t dataLength = sizeof(DeclID);
      Emit16(out, keyLength);
      Emit16(out, dataLength);
      return { keyLength, dataLength };
    }

    void EmitKey(raw_ostream &out, key_type_ref key, unsigned len) {
      out << key.str();
    }

    void EmitData(raw_ostream &out, key_type_ref key, data_type_ref data,
                  unsigned len) {
      static_assert(sizeof(DeclID) <= 32, "DeclID too large");
      using namespace clang::io;
      Emit32(out, data);
    }
  };

  class SILSerializer {
    Serializer &S;
    ASTContext &Ctx;

    llvm::BitstreamWriter &Out;

    /// A reusable buffer for emitting records.
    SmallVector<uint64_t, 64> ScratchRecord;

    /// In case we want to encode the relative of InstID vs ValueID.
    ValueID InstID = 0;

    llvm::DenseMap<const ValueBase*, ValueID> ValueIDs;
    ValueID LastValueID = 0;
    ValueID addValueRef(SILValue SV) {
      return addValueRef(SV.getDef());
    }
    ValueID addValueRef(const ValueBase *Val);

    using TableData = FuncTableInfo::data_type;
    using Table = llvm::DenseMap<FuncTableInfo::key_type, TableData>;
    Table FuncTable;
    std::vector<BitOffset> Funcs;
    DeclID FuncID;

    std::array<unsigned, 256> SILAbbrCodes;
    template <typename Layout>
    void registerSILAbbr() {
      using AbbrArrayTy = decltype(SILAbbrCodes);
      static_assert(Layout::Code <= std::tuple_size<AbbrArrayTy>::value,
                    "layout has invalid record code");
      SILAbbrCodes[Layout::Code] = Layout::emitAbbrev(Out);
    }

    void writeSILFunction(const SILFunction &F);
    void writeSILBasicBlock(const SILBasicBlock &BB);
    void writeSILInstruction(const SILInstruction &SI);
    void writeFuncTable();

  public:
    SILSerializer(Serializer &S, ASTContext &Ctx,
                  llvm::BitstreamWriter &Out);

    void writeAllSILFunctions(const SILModule *M);
  };
} // end anonymous namespace

SILSerializer::SILSerializer(Serializer &S, ASTContext &Ctx,
                             llvm::BitstreamWriter &Out) :
                            S(S), Ctx(Ctx), Out(Out), FuncID(1) {
}

/// We enumerate all valus to update ValueIDs in a separate pass
/// to correctly handle forward reference of a value.
ValueID SILSerializer::addValueRef(const ValueBase *Val) {
  if (!Val)
    return 0;

  ValueID &id = ValueIDs[Val];
  if (id != 0)
    return id;

  id = ++LastValueID;
  return id;
}

void SILSerializer::writeSILFunction(const SILFunction &F) {
  FuncTable[Ctx.getIdentifier(F.getName())] = FuncID++;
  Funcs.push_back(Out.GetCurrentBitNo());
  InstID = 0;
  unsigned abbrCode = SILAbbrCodes[SILFunctionLayout::Code];
  TypeID FnID = S.addTypeRef(F.getLoweredType().getSwiftType());
  SILFunctionLayout::emitRecord(Out, ScratchRecord, abbrCode,
                       (unsigned)F.getLinkage(), FnID);
  for (const SILBasicBlock &BB : F)
    writeSILBasicBlock(BB);
}

void SILSerializer::writeSILBasicBlock(const SILBasicBlock &BB) {
  SmallVector<DeclID, 4> Args;
  for (auto I = BB.bbarg_begin(), E = BB.bbarg_end(); I != E; ++I) {
    SILArgument *SA = *I;
    DeclID tId = S.addTypeRef(SA->getType().getSwiftType());
    DeclID vId = addValueRef(static_cast<const ValueBase*>(SA));
    Args.push_back(tId);
    Args.push_back(vId);
  }

  unsigned abbrCode = SILAbbrCodes[SILBasicBlockLayout::Code];
  SILBasicBlockLayout::emitRecord(Out, ScratchRecord, abbrCode, Args);

  for (const SILInstruction &SI : BB)
    writeSILInstruction(SI);
}

void SILSerializer::writeSILInstruction(const SILInstruction &SI) {
  switch (SI.getKind()) {
  default: {
    unsigned abbrCode = SILAbbrCodes[SILInstTodoLayout::Code];
    SILInstTodoLayout::emitRecord(Out, ScratchRecord, abbrCode,
                                  (unsigned)SI.getKind());
    break;
  }
  case ValueKind::AllocStackInst: {
    const AllocStackInst *ASI = cast<AllocStackInst>(&SI);
    unsigned abbrCode = SILAbbrCodes[SILOneTypeLayout::Code];
    SILOneTypeLayout::emitRecord(Out, ScratchRecord, abbrCode,
                        (unsigned)SI.getKind(),
                        S.addTypeRef(ASI->getElementType().getSwiftType()));
    break;
  }
  // One operand 
  case ValueKind::DeallocStackInst:
  case ValueKind::ReturnInst: {
    unsigned abbrCode = SILAbbrCodes[SILOneOperandLayout::Code];
    SILOneOperandLayout::emitRecord(Out, ScratchRecord, abbrCode,
                       (unsigned)SI.getKind(),
                       S.addTypeRef(SI.getOperand(0).getType().getSwiftType()),
                       addValueRef(SI.getOperand(0)));
    break;
  }
  case ValueKind::StoreInst: {
    const StoreInst *StI = cast<StoreInst>(&SI);
    unsigned abbrCode = SILAbbrCodes[SILOneValueOneOperandLayout::Code];
    SILOneValueOneOperandLayout::emitRecord(Out, ScratchRecord, abbrCode,
                       (unsigned)SI.getKind(), addValueRef(StI->getSrc()),
                       S.addTypeRef(StI->getDest().getType().getSwiftType()),
                       addValueRef(StI->getDest()));
    break;
  }
  }
  // Non-void values get registered in the value table.
  if (SI.hasValue()) {
    addValueRef(&SI);
    ++InstID;
  }
}

void SILSerializer::writeFuncTable() {
  using clang::OnDiskChainedHashTableGenerator;

  if (FuncTable.empty())
    return;

  SmallVector<uint64_t, 8> scratch;
  llvm::SmallString<4096> hashTableBlob;
  uint32_t tableOffset;
  {
    OnDiskChainedHashTableGenerator<FuncTableInfo> generator;
    for (auto &entry : FuncTable)
      generator.insert(entry.first, entry.second);

    llvm::raw_svector_ostream blobStream(hashTableBlob);
    // Make sure that no bucket is at offset 0
    clang::io::Emit32(blobStream, 0);
    tableOffset = generator.Emit(blobStream);
  }

  unsigned abbrCode = SILAbbrCodes[FuncListLayout::Code];
  FuncListLayout::emitRecord(Out, ScratchRecord, abbrCode, tableOffset,
                             hashTableBlob);

  abbrCode = SILAbbrCodes[FuncOffsetLayout::Code];
  FuncOffsetLayout::emitRecord(Out, ScratchRecord, abbrCode, Funcs);
}

void SILSerializer::writeAllSILFunctions(const SILModule *M) {
  {
    BCBlockRAII subBlock(Out, SIL_BLOCK_ID, 4);
    registerSILAbbr<SILFunctionLayout>();
    registerSILAbbr<SILBasicBlockLayout>();
    registerSILAbbr<SILOneValueOneOperandLayout>();
    registerSILAbbr<SILOneTypeLayout>();
    registerSILAbbr<SILOneOperandLayout>();
    registerSILAbbr<SILInstTodoLayout>();

    // Go through all SILFunctions in M, and if it is transparent,
    // write out the SILFunction.
    for (const SILFunction &F : *M) {
      if (F.isTransparent())
        writeSILFunction(F);
    }
  }
  {
    BCBlockRAII restoreBlock(Out, SIL_INDEX_BLOCK_ID, 4);
    registerSILAbbr<FuncListLayout>();
    registerSILAbbr<FuncOffsetLayout>();
    writeFuncTable();
  }
}

void Serializer::writeSILFunctions(const SILModule *M) {
  if (!M)
    return;

  SILSerializer SILSer(*this, TU->Ctx, Out);
  SILSer.writeAllSILFunctions(M);

}

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

using namespace swift;
using namespace swift::serialization;
using namespace swift::serialization::sil_block;

namespace {
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

    TypeID addTypeRef(Type ty);

    /// All identifiers that need to be serialized.
    std::vector<Identifier> IdentifiersToWrite;
    llvm::DenseMap<Identifier, IdentifierID> IdentifierIDs;
    IdentifierID LastIdentifierID = 0;

    IdentifierID addIdentifierRef(Identifier ident);
    IdentifierID addIdentifierRef(StringRef SR) {
      return addIdentifierRef(Ctx.getIdentifier(SR.data()));
    }

    std::array<unsigned, 256> SILAbbrCodes;
    template <typename Layout>
    void registerSILAbbr() {
      using AbbrArrayTy = decltype(SILAbbrCodes);
      static_assert(Layout::Code <= std::tuple_size<AbbrArrayTy>::value,
                    "layout has invalid record code");
      SILAbbrCodes[Layout::Code] = Layout::emitAbbrev(Out);
    }
  public:
    SILSerializer(Serializer &S, ASTContext &Ctx, const SILModule *M,
                  llvm::BitstreamWriter &Out);

    void writeSILFunction(const SILFunction &F);
    void writeSILBasicBlock(const SILBasicBlock &BB);
    void writeSILInstruction(const SILInstruction &SI);
  };
} // end anonymous namespace

SILSerializer::SILSerializer(Serializer &S, ASTContext &Ctx,
                             const SILModule *M,
                             llvm::BitstreamWriter &Out) :
                            S(S), Ctx(Ctx), Out(Out) {
  registerSILAbbr<SILFunctionLayout>();
  registerSILAbbr<SILBasicBlockLayout>();
  registerSILAbbr<SILOneValueOneOperandLayout>();
  registerSILAbbr<SILOneTypeLayout>();
  registerSILAbbr<SILOneOperandLayout>();
}

TypeID SILSerializer::addTypeRef(Type ty) {
  switch (ty.getPointer()->getKind()) {
  case TypeKind::BuiltinInteger:
  case TypeKind::BuiltinFloat:
  case TypeKind::BuiltinRawPointer:
  case TypeKind::BuiltinObjectPointer:
  case TypeKind::BuiltinObjCPointer:
  case TypeKind::BuiltinVector:
    /// FIXME: Builtin types can't be directly added to Serializer. We need
    /// to map Builtin types to their typealiases.
    return 0;
  default:
    return 0;
    return S.addTypeRef(ty);
  }
}

IdentifierID SILSerializer::addIdentifierRef(Identifier ident) {
  if (ident.empty())
    return 0;

  IdentifierID &id = IdentifierIDs[ident];
  if (id != 0)
    return id;

  id = ++LastIdentifierID;
  IdentifiersToWrite.push_back(ident);
  return id;
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
  InstID = 0;
  unsigned abbrCode = SILAbbrCodes[SILFunctionLayout::Code];
  SILFunctionLayout::emitRecord(Out, ScratchRecord, abbrCode,
                       (unsigned)F.getLinkage(),
                       addIdentifierRef(F.getName()),
                       addTypeRef(F.getLoweredType().getSwiftType()));
  for (const SILBasicBlock &BB : F)
    writeSILBasicBlock(BB);
}

void SILSerializer::writeSILBasicBlock(const SILBasicBlock &BB) {
  SmallVector<DeclID, 4> Args;
  for (auto I = BB.bbarg_begin(), E = BB.bbarg_end(); I != E; ++I) {
    SILArgument *SA = *I;
    DeclID tId = addTypeRef(SA->getType().getSwiftType());
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
  default:
    break;
  case ValueKind::AllocStackInst: {
    const AllocStackInst *ASI = cast<AllocStackInst>(&SI);
    unsigned abbrCode = SILAbbrCodes[SILOneTypeLayout::Code];
    SILOneTypeLayout::emitRecord(Out, ScratchRecord, abbrCode,
                        (unsigned)SI.getKind(),
                        addTypeRef(ASI->getElementType().getSwiftType()));
    break;
  }
  // One operand 
  case ValueKind::DeallocStackInst:
  case ValueKind::ReturnInst: {
    unsigned abbrCode = SILAbbrCodes[SILOneOperandLayout::Code];
    SILOneOperandLayout::emitRecord(Out, ScratchRecord, abbrCode,
                       (unsigned)SI.getKind(),
                       addTypeRef(SI.getOperand(0).getType().getSwiftType()),
                       addValueRef(SI.getOperand(0)));
    break;
  }
  case ValueKind::StoreInst: {
    const StoreInst *StI = cast<StoreInst>(&SI);
    unsigned abbrCode = SILAbbrCodes[SILOneValueOneOperandLayout::Code];
    SILOneValueOneOperandLayout::emitRecord(Out, ScratchRecord, abbrCode,
                       (unsigned)SI.getKind(), addValueRef(StI->getSrc()),
                       addTypeRef(StI->getDest().getType().getSwiftType()),
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

void Serializer::writeSILFunctions(const SILModule *M) {
  if (!M)
    return;

  BCBlockRAII restoreBlock(Out, SIL_BLOCK_ID, 4);
  SILSerializer SILSer(*this, TU->Ctx, M, Out);

  // Go through all SILFunctions in M, and if it is transparent,
  // write out the SILFunction.
  for (const SILFunction &F : *M) {
    if (F.isTransparent())
      SILSer.writeSILFunction(F);
  }
}

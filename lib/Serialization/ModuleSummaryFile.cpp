#include "swift/Serialization/ModuleSummaryFile.h"
#include "BCReadingExtras.h"
#include "memory"
#include "swift/AST/FileSystem.h"
#include "llvm/Bitstream/BitstreamReader.h"
#include "llvm/Bitstream/BitstreamWriter.h"

namespace swift {

namespace modulesummary {

static llvm::Optional<FunctionSummary::EdgeTy::Kind>
getEdgeKind(unsigned edgeKind) {
  if (edgeKind < unsigned(FunctionSummary::EdgeTy::Kind::kindCount))
    return FunctionSummary::EdgeTy::Kind(edgeKind);
  return None;
}


static llvm::Optional<VirtualMethodSlot::KindTy>
getSlotKind(unsigned kind) {
  if (kind < unsigned(FunctionSummary::EdgeTy::Kind::kindCount))
    return VirtualMethodSlot::KindTy(kind);
  return None;
}

class Serializer {
  SmallVector<char, 0> Buffer;
  llvm::BitstreamWriter Out{Buffer};

  /// A reusable buffer for emitting records.
  SmallVector<uint64_t, 64> ScratchRecord;

  void writeSignature();
  void writeBlockInfoBlock();
  void emitBlockID(unsigned ID, StringRef name,
                   SmallVectorImpl<unsigned char> &nameBuffer);

  void emitRecordID(unsigned ID, StringRef name,
                    SmallVectorImpl<unsigned char> &nameBuffer);

public:

  void emitHeader();
  void emitModuleSummary(const ModuleSummaryIndex &index);
  void write(llvm::raw_ostream &os);
};

void Serializer::emitBlockID(unsigned ID, StringRef name,
                             llvm::SmallVectorImpl<unsigned char> &nameBuffer) {
  SmallVector<unsigned, 1> idBuffer;
  idBuffer.push_back(ID);
  Out.EmitRecord(llvm::bitc::BLOCKINFO_CODE_SETBID, idBuffer);

  // Emit the block name if present.
  if (name.empty())
    return;
  nameBuffer.resize(name.size());
  memcpy(nameBuffer.data(), name.data(), name.size());
  Out.EmitRecord(llvm::bitc::BLOCKINFO_CODE_BLOCKNAME, nameBuffer);
}

void Serializer::emitRecordID(unsigned ID, StringRef name,
                              SmallVectorImpl<unsigned char> &nameBuffer) {
  assert(ID < 256 && "can't fit record ID in next to name");
  nameBuffer.resize(name.size() + 1);
  nameBuffer[0] = ID;
  memcpy(nameBuffer.data() + 1, name.data(), name.size());
  Out.EmitRecord(llvm::bitc::BLOCKINFO_CODE_SETRECORDNAME, nameBuffer);
}

void Serializer::writeSignature() {
  for (auto c : MODULE_SUMMARY_SIGNATURE)
    Out.Emit((unsigned) c, 8);
}

void Serializer::writeBlockInfoBlock() {
  llvm::BCBlockRAII restoreBlock(Out, llvm::bitc::BLOCKINFO_BLOCK_ID, 2);

  SmallVector<unsigned char, 64> nameBuffer;
#define BLOCK(X) emitBlockID(X##_ID, #X, nameBuffer)
#define BLOCK_RECORD(K, X) emitRecordID(K::X, #X, nameBuffer)

  BLOCK(MODULE_SUMMARY);
  BLOCK_RECORD(module_summary, MODULE_METADATA);

  BLOCK(FUNCTION_SUMMARY);
  BLOCK_RECORD(function_summary, METADATA);
  BLOCK_RECORD(function_summary, CALL_GRAPH_EDGE);

  BLOCK(VIRTUAL_METHOD_INFO);
  BLOCK_RECORD(virtual_method_info, METHOD_METADATA);
  BLOCK_RECORD(virtual_method_info, METHOD_IMPL);

}

void Serializer::emitHeader() {
  writeSignature();
  writeBlockInfoBlock();
}

void Serializer::emitModuleSummary(const ModuleSummaryIndex &index) {
  using namespace module_summary;

  llvm::BCBlockRAII restoreBlock(Out, MODULE_SUMMARY_ID, 4);
  module_summary::MetadataLayout MDLayout(Out);
  MDLayout.emit(ScratchRecord, index.getModuleName());
  {
    for (const auto &pair : index) {
      llvm::BCBlockRAII restoreBlock(Out, FUNCTION_SUMMARY_ID, 32);
      auto &summary = pair.second;
      using namespace function_summary;
      function_summary::MetadataLayout MDlayout(Out);

      MDlayout.emit(ScratchRecord, pair.first, summary->isLive(), summary->isPreserved(), summary->getName());

      for (auto call : summary->calls()) {
        CallGraphEdgeLayout edgeLayout(Out);
        edgeLayout.emit(ScratchRecord, unsigned(call.getKind()),
                        call.getCallee(), call.getName());
      }
    }
  }
  
  {
    for (auto &method : index.virtualMethods()) {
      llvm::BCBlockRAII restoreBlock(Out, VIRTUAL_METHOD_INFO_ID, 32);
      auto &slot = method.first;
      auto impls = method.second;
      using namespace virtual_method_info;

      MethodMetadataLayout MDLayout(Out);

      MDLayout.emit(ScratchRecord,
                    unsigned(slot.Kind),
                    slot.VirtualFuncID);
      
      for (auto impl : impls) {
        MethodImplLayout ImplLayout(Out);
        ImplLayout.emit(ScratchRecord, impl);
      }
    }
  }
}

void Serializer::write(llvm::raw_ostream &os) {
  os.write(Buffer.data(), Buffer.size());
  os.flush();
}

bool emitModuleSummaryIndex(const ModuleSummaryIndex &index,
                            DiagnosticEngine &diags, StringRef path) {
  return withOutputFile(diags, path, [&](llvm::raw_ostream &out) {
    Serializer serializer;
    serializer.emitHeader();
    serializer.emitModuleSummary(index);
    serializer.write(out);
    return false;
  });
}

class Deserializer {
  llvm::BitstreamCursor Cursor;
  SmallVector<uint64_t, 64> Scratch;
  StringRef BlobData;

  ModuleSummaryIndex &moduleSummary;

  bool readSignature();
  bool readModuleSummaryMetadata();
  bool readVirtualMethodInfo();
  bool readFunctionSummary();
  bool readSingleModuleSummary();

public:
  Deserializer(llvm::MemoryBufferRef inputBuffer,
               ModuleSummaryIndex &moduleSummary)
      : Cursor{inputBuffer}, moduleSummary(moduleSummary) {}
  bool readModuleSummary();
};

bool Deserializer::readSignature() {
  for (unsigned char byte : MODULE_SUMMARY_SIGNATURE) {
    if (Cursor.AtEndOfStream())
      return true;
    if (auto maybeRead = Cursor.Read(8)) {
      if (maybeRead.get() != byte)
        return true;
    } else {
      return true;
    }
  }
  return false;
}

bool Deserializer::readModuleSummaryMetadata() {
  llvm::Expected<llvm::BitstreamEntry> maybeEntry = Cursor.advance();
  if (!maybeEntry)
    llvm::report_fatal_error("Should have next entry");

  llvm::BitstreamEntry entry = maybeEntry.get();

  if (entry.Kind != llvm::BitstreamEntry::Record) {
    return true;
  }
  Scratch.clear();
  auto maybeKind = Cursor.readRecord(entry.ID, Scratch, &BlobData);

  if (!maybeKind) {
    consumeError(maybeKind.takeError());
    return true;
  }

  if (maybeKind.get() != module_summary::MODULE_METADATA) {
    return true;
  }

  moduleSummary.setModuleName(BlobData.str());

  return false;
}

bool Deserializer::readFunctionSummary() {
  if (llvm::Error Err = Cursor.EnterSubBlock(FUNCTION_SUMMARY_ID)) {
    llvm::report_fatal_error("Can't enter subblock");
  }

  llvm::Expected<llvm::BitstreamEntry> maybeNext = Cursor.advance();
  if (!maybeNext)
    llvm::report_fatal_error("Should have next entry");

  llvm::BitstreamEntry next = maybeNext.get();

  GUID guid;
  std::string Name;
  FunctionSummary *FS;
  std::unique_ptr<FunctionSummary> NewFSOwner;

  while (next.Kind == llvm::BitstreamEntry::Record) {
    Scratch.clear();

    auto maybeKind = Cursor.readRecord(next.ID, Scratch, &BlobData);

    if (!maybeKind)
      llvm::report_fatal_error("Should have kind");

    switch (maybeKind.get()) {
    case function_summary::METADATA: {
      unsigned isLive, isPreserved;
      function_summary::MetadataLayout::readRecord(Scratch, guid, isLive, isPreserved);
      Name = BlobData.str();
      if (auto info = moduleSummary.getFunctionInfo(guid)) {
        FS = info.getValue();
      } else {
        NewFSOwner = std::make_unique<FunctionSummary>();
        FS = NewFSOwner.get();
      }
      FS->setLive(isLive);
      FS->setPreserved(isPreserved);
      FS->setName(Name);
      break;
    }
    case function_summary::CALL_GRAPH_EDGE: {
      unsigned edgeKindID;
      GUID targetGUID;
      function_summary::CallGraphEdgeLayout::readRecord(Scratch, edgeKindID,
                                                        targetGUID);
      auto edgeKind = getEdgeKind(edgeKindID);
      if (!edgeKind)
        llvm::report_fatal_error("Bad edge kind");
      if (!FS)
        llvm::report_fatal_error("Invalid state");

      FS->addCall(targetGUID, BlobData.str(), edgeKind.getValue());
      break;
    }
    }

    maybeNext = Cursor.advance();
    if (!maybeNext)
      llvm::report_fatal_error("Should have next entry");

    next = maybeNext.get();
  }

  if (auto &FS = NewFSOwner) {
    moduleSummary.addFunctionSummary(std::move(FS));
  }
  return false;
}

bool Deserializer::readVirtualMethodInfo() {
  if (llvm::Error Err = Cursor.EnterSubBlock(VIRTUAL_METHOD_INFO_ID)) {
    llvm::report_fatal_error("Can't enter subblock");
  }

  llvm::Expected<llvm::BitstreamEntry> maybeNext = Cursor.advance();
  if (!maybeNext)
    llvm::report_fatal_error("Should have next entry");

  llvm::BitstreamEntry next = maybeNext.get();

  llvm::Optional<VirtualMethodSlot> slot;

  while (next.Kind == llvm::BitstreamEntry::Record) {
    Scratch.clear();

    auto maybeKind = Cursor.readRecord(next.ID, Scratch, &BlobData);

    if (!maybeKind)
      llvm::report_fatal_error("Should have kind");

    switch (maybeKind.get()) {
    case virtual_method_info::METHOD_METADATA: {
      unsigned methodKindID;
      GUID virtualFuncGUID;
      virtual_method_info::MethodMetadataLayout::readRecord(Scratch, methodKindID, virtualFuncGUID);
      
      auto Kind = getSlotKind(methodKindID);
      if (!Kind)
        llvm::report_fatal_error("Bad method kind");

      slot = VirtualMethodSlot(Kind.getValue(), virtualFuncGUID);
      break;
    }
    case virtual_method_info::METHOD_IMPL: {
      GUID implGUID;
      virtual_method_info::MethodImplLayout::readRecord(Scratch, implGUID);
      if (!slot)
        llvm::report_fatal_error("Slot should be set before impl");
      moduleSummary.addImplementation(slot.getValue(), implGUID);
      break;
    }
    }

    maybeNext = Cursor.advance();
    if (!maybeNext)
      llvm::report_fatal_error("Should have next entry");

    next = maybeNext.get();
  }

  return false;
}

bool Deserializer::readSingleModuleSummary() {
  if (llvm::Error Err = Cursor.EnterSubBlock(MODULE_SUMMARY_ID)) {
    llvm::report_fatal_error("Can't enter subblock");
  }

  if (readModuleSummaryMetadata()) {
    return true;
  }

  llvm::Expected<llvm::BitstreamEntry> maybeNext = Cursor.advance();
  if (!maybeNext)
    llvm::report_fatal_error("Should have next entry");

  llvm::BitstreamEntry next = maybeNext.get();
  while (next.Kind == llvm::BitstreamEntry::SubBlock) {
    switch (next.ID) {
    case FUNCTION_SUMMARY_ID: {
      if (readFunctionSummary()) {
        llvm::report_fatal_error("Failed to read FS");
      }
      break;
    }
      case VIRTUAL_METHOD_INFO_ID: {
        if (readVirtualMethodInfo()) {
          llvm::report_fatal_error("Failed to read virtual method info");
        }
        break;
      }
    }

    maybeNext = Cursor.advance();
    if (!maybeNext) {
      consumeError(maybeNext.takeError());
      return true;
    }
    next = maybeNext.get();
  }
  return false;
}

bool Deserializer::readModuleSummary() {
  if (readSignature()) {
    llvm::report_fatal_error("Invalid signature");
  }

  while (!Cursor.AtEndOfStream()) {
    llvm::Expected<llvm::BitstreamEntry> maybeEntry =
        Cursor.advance(llvm::BitstreamCursor::AF_DontPopBlockAtEnd);
    if (!maybeEntry) {
      llvm::report_fatal_error("Should have entry");
    }

    auto entry = maybeEntry.get();
    if (entry.Kind != llvm::BitstreamEntry::SubBlock)
      break;

    switch (entry.ID) {
    case llvm::bitc::BLOCKINFO_BLOCK_ID: {
      if (Cursor.SkipBlock()) {
        return true;
      }
      break;
    }
    case MODULE_SUMMARY_ID: {
      if (readSingleModuleSummary()) {
        return true;
      }
      break;
    }
    case FUNCTION_SUMMARY_ID:
    case VIRTUAL_METHOD_INFO_ID: {
      llvm_unreachable("FUNCTION_SUMMARY and VIRTUAL_METHOD_INFO_ID blocks should be handled in "
                       "'readSingleModuleSummary'");
      break;
    }
    }
  }
  return false;
}

bool loadModuleSummaryIndex(llvm::MemoryBufferRef inputBuffer,
                            ModuleSummaryIndex &moduleSummary) {
  Deserializer deserializer(inputBuffer, moduleSummary);
  return deserializer.readModuleSummary();
}

} // namespace modulesummary
} // namespace swift

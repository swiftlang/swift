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
}

void Serializer::emitHeader() {
  writeSignature();
  writeBlockInfoBlock();
}

void Serializer::emitModuleSummary(const ModuleSummaryIndex &index) {
  using namespace module_summary;

  llvm::BCBlockRAII restoreBlock(Out, MODULE_SUMMARY_ID, 3);
  module_summary::MetadataLayout MDLayout(Out);
  MDLayout.emit(ScratchRecord, index.getModuleName());
  {
    llvm::BCBlockRAII restoreBlock(Out, FUNCTION_SUMMARY_ID, 4);
    using namespace function_summary;

    for (const auto &pair : index) {
      auto &info = pair.second;
      using namespace function_summary;
      function_summary::MetadataLayout MDlayout(Out);
      MDlayout.emit(ScratchRecord, pair.first, info.Name);

      for (auto call : info.TheSummary->calls()) {
        CallGraphEdgeLayout edgeLayout(Out);
        edgeLayout.emit(ScratchRecord, unsigned(call.getKind()),
                        call.getCallee());
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

static bool readSignature(llvm::BitstreamCursor Cursor) {
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

std::unique_ptr<ModuleSummaryIndex>
loadModuleSummaryIndex(std::unique_ptr<llvm::MemoryBuffer> inputBuffer) {
  llvm::BitstreamCursor cursor{inputBuffer->getMemBufferRef()};

  if (readSignature(cursor))
    return nullptr;

  auto moduleSummary = std::make_unique<ModuleSummaryIndex>();

  while (!cursor.AtEndOfStream()) {
    llvm::Expected<llvm::BitstreamEntry> maybeEntry =
        cursor.advance(llvm::BitstreamCursor::AF_DontPopBlockAtEnd);
    if (!maybeEntry) {
      llvm::report_fatal_error("Should have entry");
    }

    auto entry = maybeEntry.get();
    if (entry.Kind != llvm::BitstreamEntry::SubBlock)
      break;

    switch (entry.ID) {
    case MODULE_SUMMARY_ID: {
      if (llvm::Error Err = cursor.EnterSubBlock(MODULE_SUMMARY_ID)) {
        llvm::report_fatal_error("Can't enter subblock");
      }

      llvm::Expected<llvm::BitstreamEntry> maybeNext = cursor.advance();
      if (!maybeNext)
        llvm::report_fatal_error("Should have next entry");
      
      llvm::BitstreamEntry next = maybeNext.get();
      llvm::SmallVector<uint64_t, 64> scratch;
      while (next.Kind == llvm::BitstreamEntry::Record) {
        scratch.clear();
        llvm::StringRef blobData;
        llvm::Expected<unsigned> maybeKind =
            cursor.readRecord(next.ID, scratch, &blobData);

        if (!maybeKind)
          llvm::report_fatal_error("Should have kind");

        unsigned kind = maybeKind.get();
        
        switch (kind) {
        case module_summary::MODULE_METADATA: {
          moduleSummary->setModuleName(blobData.str());
          break;
        }
        }
      }
      break;
    }
    case FUNCTION_SUMMARY_ID: {
      if (llvm::Error Err = cursor.EnterSubBlock(FUNCTION_SUMMARY_ID)) {
        llvm::report_fatal_error("Can't enter subblock");
      }

      llvm::Expected<llvm::BitstreamEntry> maybeNext = cursor.advance();
      if (!maybeNext)
        llvm::report_fatal_error("Should have next entry");

      llvm::BitstreamEntry next = maybeNext.get();

      GUID guid;
      std::string Name;
      std::unique_ptr<FunctionSummary> FS;
      std::vector<FunctionSummary::EdgeTy> CGEdges;

      llvm::SmallVector<uint64_t, 64> scratch;
      while (next.Kind == llvm::BitstreamEntry::Record) {
        scratch.clear();
        llvm::StringRef blobData;
        llvm::Expected<unsigned> maybeKind =
            cursor.readRecord(next.ID, scratch, &blobData);

        if (!maybeKind)
          llvm::report_fatal_error("Should have kind");

        unsigned kind = maybeKind.get();
        switch (kind) {
        case function_summary::METADATA: {
          function_summary::MetadataLayout::readRecord(scratch, guid);
          Name = blobData.str();
          FS = std::make_unique<FunctionSummary>();
          break;
        }
        case function_summary::CALL_GRAPH_EDGE: {
          unsigned edgeKindID, targetGUID;
          function_summary::CallGraphEdgeLayout::readRecord(scratch, edgeKindID,
                                                            targetGUID);
          auto edgeKind = getEdgeKind(edgeKindID);
          if (!edgeKind)
            llvm::report_fatal_error("Bad edge kind");
          if (!FS)
            llvm::report_fatal_error("Invalid state");

          FS->addCall(targetGUID, edgeKind.getValue());
          break;
        }
        }
      }

      moduleSummary->addFunctionSummary(Name, std::move(FS));
      break;
    }
    }
  }

  return moduleSummary;
}

} // namespace modulesummary
} // namespace swift

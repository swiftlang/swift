#include "swift/Serialization/ModuleSummaryFile.h"
#include "BCReadingExtras.h"
#include "memory"
#include "llvm/Bitstream/BitstreamReader.h"

namespace swift {

namespace modulesummary {

static llvm::Optional<FunctionSummary::EdgeTy::Kind>
getEdgeKind(unsigned edgeKind) {
  if (edgeKind < unsigned(FunctionSummary::EdgeTy::Kind::kindCount))
    return FunctionSummary::EdgeTy::Kind(edgeKind);
  return None;
}

std::unique_ptr<ModuleSummaryIndex>
loadModuleSummaryIndex(std::unique_ptr<llvm::MemoryBuffer> inputBuffer) {
  llvm::BitstreamCursor cursor{inputBuffer->getMemBufferRef()};

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
    case CONTROL_BLOCK_ID: {
      if (cursor.SkipBlock())
        llvm::report_fatal_error("Can't skip block");
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

      moduleSummary->addFunctionSummary(guid, std::move(FS));
    }
    }
  }

  return moduleSummary;
}

} // namespace modulesummary
} // namespace swift

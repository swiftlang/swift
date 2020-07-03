#ifndef SWIFT_SERIALIZATION_MODULE_SUMMARY_FILE_H
#define SWIFT_SERIALIZATION_MODULE_SUMMARY_FILE_H

#include "swift/SIL/ModuleSummary.h"
#include "llvm/Bitcode/RecordLayout.h"
#include "llvm/Support/MemoryBuffer.h"
#include <memory>

namespace swift {

namespace modulesummary {

const unsigned char MODULE_SUMMARY_SIGNATURE[] = {'M', 'O', 'D', 'S'};

enum BlockID {
  MODULE_BLOCK_ID = llvm::bitc::FIRST_APPLICATION_BLOCKID,

  FUNCTION_SUMMARY_ID,
};

namespace function_summary {
using namespace llvm;
enum {
  METADATA,
  CALL_GRAPH_EDGE,
};

using MetadataLayout = BCRecordLayout<METADATA,
                                      BCVBR<16>, // Function GUID
                                      BCBlob       // Name string
                                      >;
using CallGraphEdgeLayout =
    BCRecordLayout<CALL_GRAPH_EDGE,
                   BCFixed<32>, // FunctionSummary::Edge::Kind
                   BCVBR<16>  // Target GUID
                   >;
} // namespace function_summary

bool emitModuleSummaryIndex(const ModuleSummaryIndex &index,
                            DiagnosticEngine &diags, StringRef path);

std::unique_ptr<ModuleSummaryIndex>
loadModuleSummaryIndex(std::unique_ptr<llvm::MemoryBuffer> inputBuffer);
} // namespace modulesummary
} // namespace swift

#endif

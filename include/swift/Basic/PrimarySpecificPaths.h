//
//  PrimarySpecificPaths.h
//  Swift
//
//  Created by David Ungar on 12/28/17.
//

#ifndef PrimarySpecificPaths_h
#define PrimarySpecificPaths_h

#include "swift/Basic/LLVM.h"
#include "swift/Basic/SupplementaryOutputPaths.h"

#include <string>

namespace swift {
class PrimarySpecificPaths {
public:
  const std::string OutputFilename;
  const SupplementaryOutputPaths SupplementaryOutputs;
  const std::string MainInputFilenameForDebugInfo;

  static PrimarySpecificPaths lldbStub(std::string outputFilename = "") {
    return PrimarySpecificPaths(outputFilename, SupplementaryOutputPaths(),
                                "<lldb>");
  }

  static PrimarySpecificPaths fakeNamesStub() {
    return PrimarySpecificPaths("<fake output filename>",
                                SupplementaryOutputPaths(),
                                "<fake main input filename>");
  }

  PrimarySpecificPaths(std::string OutputFilename,
                       SupplementaryOutputPaths SupplementaryOutputs,
                       std::string MainInputFilenameForDebugInfo)
      : OutputFilename(OutputFilename),
        SupplementaryOutputs(SupplementaryOutputs),
        MainInputFilenameForDebugInfo(MainInputFilenameForDebugInfo) {}
};
} // namespace swift

#endif /* PrimarySpecificPaths_h */

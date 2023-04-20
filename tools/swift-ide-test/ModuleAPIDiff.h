#ifndef SWIFT_IDE_TEST_MODULE_API_DIFF_H
#define SWIFT_IDE_TEST_MODULE_API_DIFF_H

#include "swift/Basic/LLVM.h"
#include <string>

namespace swift {

int doGenerateModuleAPIDescription(StringRef DriverPath,
                                   StringRef MainExecutablePath,
                                   ArrayRef<std::string> Args);

} // end namespace swift

#endif // SWIFT_IDE_TEST_MODULE_API_DIFF_H


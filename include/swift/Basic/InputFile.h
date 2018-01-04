//
//  InputFile.h
//  Swift
//
//  Created by David Ungar on 12/10/17.
//

#ifndef InputFile_h
#define InputFile_h

#include "swift/Basic/PrimarySpecificPaths.h"
#include "swift/Basic/SupplementaryOutputPaths.h"
#include "llvm/Support/MemoryBuffer.h"

#include <string>
#include <vector>

namespace swift {

enum class InputFileKind {
  IFK_None,
  IFK_Swift,
  IFK_Swift_Library,
  IFK_Swift_REPL,
  IFK_SIL,
  IFK_LLVM_IR
};

// Inputs may include buffers that override contents, and eventually should
// always include a buffer.
class InputFile {
  std::string Filename;
  bool IsPrimary;
  /// Null if the contents are not overridden.
  llvm::MemoryBuffer *Buffer;

  /// The specified output file.
  /// If only a single outputfile is generated,
  /// the name of the last specified file is taken.
  std::string OutputFilename;

  /// The supplementary outputs associated with this input:
  /// Temporarily keep in the first output-producing input.
  SupplementaryOutputPaths SupplementaryPaths;

public:
  /// Does not take ownership of \p buffer. Does take ownership of (copy) a
  /// string.
  InputFile(StringRef name, bool isPrimary,
            llvm::MemoryBuffer *buffer = nullptr,
            StringRef outputFilename = StringRef())
      : Filename(
            convertBufferNameFromLLVM_getFileOrSTDIN_toSwiftConventions(name)),
        IsPrimary(isPrimary), Buffer(buffer),
        SupplementaryPaths(SupplementaryOutputPaths()),
        OutputFilename(outputFilename) {
    assert(name.begin() != Filename.c_str());
    assert(!name.empty() && "Empty strings signify no inputs in other places");
  }

  bool isPrimary() const { return IsPrimary; }
  llvm::MemoryBuffer *buffer() const { return Buffer; }
  StringRef file() const { return Filename; }

  /// Return Swift-standard file name from a buffer name set by
  /// llvm::MemoryBuffer::getFileOrSTDIN, which uses "<stdin>" instead of "-".
  static StringRef convertBufferNameFromLLVM_getFileOrSTDIN_toSwiftConventions(
      StringRef filename) {
    return filename.equals("<stdin>") ? "-" : filename;
  }

  const std::string &outputFilename() const { return OutputFilename; }

  void setOutputFilename(StringRef outputFilename) {
    OutputFilename = outputFilename;
  }

  const SupplementaryOutputPaths &supplementaryOutputs() const {
    return SupplementaryPaths;
  }

  void setSupplementaryOutputs(const SupplementaryOutputPaths &outs) {
    SupplementaryPaths = outs;
  }

  void setOutputFileNameAndSupplementaryOutputPaths(
      StringRef outputFilename, const SupplementaryOutputPaths &outs) {
    setOutputFilename(outputFilename);
    setSupplementaryOutputs(outs);
  }

  PrimarySpecificPaths getPSPs() const {
    return PrimarySpecificPaths(outputFilename(), supplementaryOutputs(),
                                file());
  }
};
} // namespace swift

#endif /* InputFile_h */

//
//  InputFile.h
//  Swift
//
//  Created by David Ungar on 12/10/17.
//

#ifndef InputFile_h
#define InputFile_h

#include "swift/Basic/OutputPaths.h"
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

  /// The supplementary outputs associated with this input:
  /// Temporarily keep in the first output-producing input.
  OutputPaths SupplementaryOutputPaths;

public:
  /// Does not take ownership of \p buffer. Does take ownership of (copy) a
  /// string.
  InputFile(StringRef name, bool isPrimary,
            llvm::MemoryBuffer *buffer = nullptr)
      : Filename(name), IsPrimary(isPrimary), Buffer(buffer),
        SupplementaryOutputPaths(OutputPaths()) {
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

  const OutputPaths &supplementaryOutputPaths() const {
    return SupplementaryOutputPaths;
  }

  void setSupplementaryOutputPaths(const OutputPaths &outs) {
    SupplementaryOutputPaths = outs;
  }
};
} // namespace swift

#endif /* InputFile_h */

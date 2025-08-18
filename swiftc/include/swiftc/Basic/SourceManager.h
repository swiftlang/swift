#ifndef SWIFTC_BASIC_SOURCEMANAGER_H
#define SWIFTC_BASIC_SOURCEMANAGER_H

#include "swiftc/Basic/LLVM.h"
#include "swiftc/Basic/SourceLoc.h"
#include <llvm/Support/MemoryBuffer.h>
#include <memory>
#include <string>
#include <vector>

namespace swiftc {

class SourceManager {
public:
  struct SourceFile {
    std::unique_ptr<llvm::MemoryBuffer> Buffer;
    std::string Filename;
    SourceLoc StartLoc;
    
    SourceFile(std::unique_ptr<llvm::MemoryBuffer> buffer, StringRef filename)
        : Buffer(std::move(buffer)), Filename(filename.str()) {}
  };

private:
  std::vector<std::unique_ptr<SourceFile>> Files;
  uint32_t NextStartLoc = 1;

public:
  SourceManager() = default;
  ~SourceManager() = default;

  // Non-copyable
  SourceManager(const SourceManager&) = delete;
  SourceManager& operator=(const SourceManager&) = delete;

  /// Add a source file to the source manager.
  SourceLoc addSourceFile(std::unique_ptr<llvm::MemoryBuffer> buffer,
                          StringRef filename);

  /// Get the source file containing the given location.
  const SourceFile* getSourceFile(SourceLoc loc) const;

  /// Get the buffer for the given location.
  StringRef getBuffer(SourceLoc loc) const;

  /// Get the filename for the given location.
  StringRef getFilename(SourceLoc loc) const;

  /// Get line and column information for a source location.
  std::pair<unsigned, unsigned> getLineAndColumn(SourceLoc loc) const;

  /// Get the character at the given location.
  char getCharacterAt(SourceLoc loc) const;

  /// Check if a location is at the end of file.
  bool isAtEndOfFile(SourceLoc loc) const;
};

} // namespace swiftc

#endif // SWIFTC_BASIC_SOURCEMANAGER_H
#include "swiftc/Basic/SourceManager.h"
#include <llvm/Support/MemoryBuffer.h>
#include <algorithm>

using namespace swiftc;

SourceLoc SourceManager::addSourceFile(std::unique_ptr<llvm::MemoryBuffer> buffer,
                                       StringRef filename) {
  auto file = std::make_unique<SourceFile>(std::move(buffer), filename);
  file->StartLoc = SourceLoc(NextStartLoc);
  
  // Calculate next start location based on buffer size
  NextStartLoc += file->Buffer->getBufferSize() + 1;
  
  SourceLoc startLoc = file->StartLoc;
  Files.push_back(std::move(file));
  return startLoc;
}

const SourceManager::SourceFile* SourceManager::getSourceFile(SourceLoc loc) const {
  if (loc.isInvalid())
    return nullptr;
    
  uint32_t rawLoc = loc.getRawValue();
  
  // Find the file containing this location
  for (const auto& file : Files) {
    uint32_t startLoc = file->StartLoc.getRawValue();
    uint32_t endLoc = startLoc + file->Buffer->getBufferSize();
    
    if (rawLoc >= startLoc && rawLoc < endLoc) {
      return file.get();
    }
  }
  
  return nullptr;
}

StringRef SourceManager::getBuffer(SourceLoc loc) const {
  const SourceFile* file = getSourceFile(loc);
  if (!file)
    return StringRef();
  
  return file->Buffer->getBuffer();
}

StringRef SourceManager::getFilename(SourceLoc loc) const {
  const SourceFile* file = getSourceFile(loc);
  if (!file)
    return StringRef();
  
  return file->Filename;
}

std::pair<unsigned, unsigned> SourceManager::getLineAndColumn(SourceLoc loc) const {
  const SourceFile* file = getSourceFile(loc);
  if (!file)
    return {0, 0};
  
  uint32_t offset = loc.getRawValue() - file->StartLoc.getRawValue();
  StringRef buffer = file->Buffer->getBuffer();
  
  if (offset >= buffer.size())
    return {0, 0};
  
  unsigned line = 1;
  unsigned column = 1;
  
  for (unsigned i = 0; i < offset; ++i) {
    if (buffer[i] == '\n') {
      ++line;
      column = 1;
    } else {
      ++column;
    }
  }
  
  return {line, column};
}

char SourceManager::getCharacterAt(SourceLoc loc) const {
  const SourceFile* file = getSourceFile(loc);
  if (!file)
    return '\0';
  
  uint32_t offset = loc.getRawValue() - file->StartLoc.getRawValue();
  StringRef buffer = file->Buffer->getBuffer();
  
  if (offset >= buffer.size())
    return '\0';
  
  return buffer[offset];
}

bool SourceManager::isAtEndOfFile(SourceLoc loc) const {
  const SourceFile* file = getSourceFile(loc);
  if (!file)
    return true;
  
  uint32_t offset = loc.getRawValue() - file->StartLoc.getRawValue();
  return offset >= file->Buffer->getBufferSize();
}
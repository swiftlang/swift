//===- tapi/Core/YAMLReaderWriter.h - YAML Reader/Writer --------*- C++ -*-===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
///
/// \file
/// \brief Defines the YAML Reader/Writer.
///
//===----------------------------------------------------------------------===//

#ifndef TAPI_CORE_YAML_READER_WRITER_H
#define TAPI_CORE_YAML_READER_WRITER_H

#include "ArchitectureSet.h"
#include "File.h"
#include "LLVM.h"
#include "Registry.h"
#include "Defines.h"
#include "llvm/BinaryFormat/Magic.h"
#include "llvm/Support/Error.h"
#include <string>

namespace llvm {
namespace yaml {
class IO;
} // namespace yaml
} // namespace llvm

TAPI_NAMESPACE_INTERNAL_BEGIN

class YAMLBase;

struct YAMLContext {
  const YAMLBase &base;
  std::string path;
  std::string errorMessage;
  ReadFlags readFlags;
  FileType fileType = FileType::Invalid;

  YAMLContext(const YAMLBase &base) : base(base) {}
};

class DocumentHandler {
public:
  virtual ~DocumentHandler() = default;
  virtual bool canRead(MemoryBufferRef memBufferRef, FileType types) const = 0;
  virtual FileType getFileType(MemoryBufferRef bufferRef) const = 0;
  virtual bool canWrite(const File *file) const = 0;
  virtual bool handleDocument(llvm::yaml::IO &io, const File *&file) const = 0;
};

class YAMLBase {
public:
  bool canRead(MemoryBufferRef memBufferRef, FileType types) const;
  FileType getFileType(MemoryBufferRef bufferRef) const;
  bool canWrite(const File *file) const;
  bool handleDocument(llvm::yaml::IO &io, const File *&file) const;

  void add(std::unique_ptr<DocumentHandler> handler) {
    _documentHandlers.emplace_back(std::move(handler));
  }

private:
  std::vector<std::unique_ptr<DocumentHandler>> _documentHandlers;
};

class YAMLReader final : public YAMLBase, public Reader {
public:
  bool canRead(file_magic magic, MemoryBufferRef memBufferRef,
               FileType types) const override;
  Expected<FileType> getFileType(file_magic magic,
                                 MemoryBufferRef bufferRef) const override;
  Expected<std::unique_ptr<File>>
  readFile(std::unique_ptr<MemoryBuffer> memBuffer, ReadFlags readFlags,
           ArchitectureSet arches) const override;
};

class YAMLWriter final : public YAMLBase, public Writer {
public:
  bool canWrite(const File *file) const override;
  Error writeFile(raw_ostream &os, const File *file) const override;
};

TAPI_NAMESPACE_INTERNAL_END

#endif // TAPI_CORE_YAML_READER_WRITER_H

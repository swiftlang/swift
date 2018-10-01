//===- tapi/Core/File.h - TAPI File -----------------------------*- C++ -*-===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
///
/// \file
/// \brief TAPI File abstraction.
///
//===----------------------------------------------------------------------===//

#ifndef TAPI_CORE_FILE_H
#define TAPI_CORE_FILE_H

#include "LLVM.h"
#include "Defines.h"
#include "llvm/Support/MemoryBuffer.h"
#include "llvm/Support/Path.h"
#include <system_error>
#include <vector>

TAPI_NAMESPACE_INTERNAL_BEGIN

// clang-format off
enum FileType : unsigned {
  /// \brief Invalid file type.
  Invalid                   = 0U,

  /// \brief TAPI Configuration file.
  TAPI_Configuration_V1     = 1U <<  0,

  /// \brief MachO Dynamic Library file.
  MachO_DynamicLibrary      = 1U <<  1,

  /// \brief MachO Dynamic Library Stub file.
  MachO_DynamicLibrary_Stub = 1U <<  2,

  /// \brief MachO Bundle file.
  MachO_Bundle              = 1U <<  3,

  /// \brief Text-based stub file (.tbd) version 1.0
  TBD_V1                    = 1U <<  4,

  /// \brief Text-based stub file (.tbd) version 2.0
  TBD_V2                    = 1U <<  5,

  /// \brief Text-based stub file (.tbd) version 3.0
  TBD_V3                    = 1U <<  6,

  /// \brief JSON Header List
  JSON_V1                   = 1U <<  7,

  /// \brief LD64 re-export file
  ReexportFile              = 1U <<  8,

  /// \brief Text-based API file (.api) version 1.0
  API_V1                    = 1U <<  9,

  /// \brief Text-based SPI file (.spi) version 1.0
  SPI_V1                    = 1U << 10,

  /// \brief SDKDB file (.sdkdb) version 1.0
  SDKDB_V1                  = 1U << 11,

  All                       = ~0U,
};
// clang-format on

inline FileType operator&(const FileType lhs, const FileType rhs) {
  return static_cast<FileType>(static_cast<unsigned>(lhs) &
                               static_cast<unsigned>(rhs));
}

inline FileType operator|(const FileType lhs, const FileType rhs) {
  return static_cast<FileType>(static_cast<unsigned>(lhs) |
                               static_cast<unsigned>(rhs));
}

/// \brief Abstract TAPI file.
class File {
public:
  enum class Kind : unsigned {
    Configuration,
    JSONFile,
    InterfaceFileBase,
    InterfaceFile,
    ExtendedInterfaceFile,
    SDKDBFile,
  };

  virtual ~File() = default;

  template <typename T> void setPath(T &&path) {
    _path = std::forward<T &&>(path);
  }
  const std::string &getPath() const { return _path; }

  llvm::StringRef getFileName() const {
    return llvm::sys::path::filename(_path);
  }

  void setFileType(FileType type) { _fileType = type; }
  FileType getFileType() const { return _fileType; }

  void setMemoryBuffer(std::unique_ptr<MemoryBuffer> memBuffer) {
    _buffer = std::move(memBuffer);
  }

  MemoryBufferRef getMemBufferRef() const { return _buffer->getMemBufferRef(); }

  void addDocument(std::shared_ptr<File> &&document) {
    _documents.emplace_back(std::move(document));
  }

  Kind kind() const { return _kind; }

  std::vector<std::shared_ptr<File>> _documents;

protected:
  File(Kind kind) : _kind(kind) {}
  void setKind(Kind kind) { _kind = kind; }

  // Manually add the default implementations back in. The implicit ones have
  // been removed, because we defined a virtual destructor.
  File(File &&) = default;
  File &operator=(File &&) = default;

private:
  Kind _kind;
  std::string _path;
  FileType _fileType = FileType::Invalid;
  // The backing store this file was derived from. We use this as context for
  // the strings that we reference.
  std::unique_ptr<MemoryBuffer> _buffer;
};

TAPI_NAMESPACE_INTERNAL_END

#endif // TAPI_CORE_FILE_H

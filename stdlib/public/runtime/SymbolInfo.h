//===--- SymbolInfo.h -----------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2022 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_RUNTIME_SYMBOLINFO_H
#define SWIFT_RUNTIME_SYMBOLINFO_H

#include "swift/Runtime/Config.h"

#include <cstdint>

#if defined(_WIN32)
#define WIN32_LEAN_AND_MEAN
#define NOMINMAX
#include <Windows.h>
#endif

#if defined(_WIN32) && !defined(__CYGWIN__)
#include <DbgHelp.h>
#elif SWIFT_STDLIB_HAS_DLADDR
#include <dlfcn.h>
#endif

#include "llvm/ADT/Optional.h"

namespace swift {
struct SymbolInfo {
private:
#if defined(_WIN32) && !defined(__CYGWIN__)
  SYMBOL_INFO_PACKAGE _package;
  mutable llvm::Optional<char *> _imagePath;
#elif SWIFT_STDLIB_HAS_DLADDR
  Dl_info _info;
#endif
  const void *_address;

  SymbolInfo();

public:
  ~SymbolInfo();

  /// Get the address that was originally passed when this instance was created.
  const void *getAddress() const;

  /// Get the path to the image where the symbol was found.
  ///
  /// The image path uses the platform path convention. To consistently get just
  /// the filename of the image, use \c getImageFilename().
  ///
  /// The resulting C string is only valid for the lifetime of \c this.
  const char *getImagePath() const;

  /// Get the file name of the image where the symbol was found.
  ///
  /// The file name is the last path component of the image path. If the
  /// filename cannot be determined from the image path (for instance, because
  /// it is a relative path or was invalidly specified), the complete image path
  /// is returned.
  ///
  /// The resulting C string is only valid for the lifetime of \c this.
  const char *getImageFilename() const;

  /// Get the base address of the image where the symbol was found.
  const void *getImageBaseAddress() const;

  /// Get the name of the symbol.
  ///
  /// If the input address is valid within a loaded image, but does not
  /// correspond to any known symbol, the resulting pointer may be \c nullptr.
  ///
  /// The resulting C string is only valid for the lifetime of \c this.
  const char *getSymbolName() const;

  /// Get the address of the symbol.
  ///
  /// If the input address is valid within a loaded image, but does not
  /// correspond to any known symbol, the resulting pointer may be \c nullptr.
  const void *getSymbolAddress() const;

  /// Look up a symbol by address.
  ///
  /// \param address The address where the symbol is located.
  ///
  /// \returns On success, an instance of \c SymbolInfo containing information
  ///   about the symbol at \a address. On failure, \c llvm::None.
  static llvm::Optional<SymbolInfo> lookup(const void *address);
};

} // end namespace swift

#endif

//===- tapi/Core/IntefaceFile.h - TAPI Interface File --------*- C++ -*-===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
///
/// \file
/// A generic and abstract interface representation for linkable objects.
///        This could be an MachO executable, bundle, dylib, or text-based stub
///        file.
///
//===----------------------------------------------------------------------===//

#ifndef TAPI_CORE_INTERFACE_FILE_H
#define TAPI_CORE_INTERFACE_FILE_H

#include "Architecture.h"
#include "ArchitectureSet.h"
#include "File.h"
#include "InterfaceFileBase.h"
#include "STLExtras.h"
#include "CoreSymbol.h"
#include "Defines.h"
#include "llvm/ADT/StringRef.h"
#include "llvm/ADT/iterator.h"
#include "llvm/Support/Allocator.h"
#include "llvm/Support/Error.h"

namespace llvm {
namespace yaml {
template <typename T> struct MappingTraits;
}
} // namespace llvm

TAPI_NAMESPACE_INTERNAL_BEGIN

class ExtendedInterfaceFile;

class InterfaceFile : public InterfaceFileBase {
public:
  static bool classof(const File *file) {
    return file->kind() == File::Kind::InterfaceFile;
  }

  InterfaceFile() : InterfaceFileBase(File::Kind::InterfaceFile) {}
  InterfaceFile(ExtendedInterfaceFile &&);

  void addSymbol(SymbolKind kind, StringRef name, ArchitectureSet archs,
                 SymbolFlags flags = SymbolFlags::None,
                 bool copyStrings = true);
  void addUndefinedSymbol(SymbolKind kind, StringRef name,
                          ArchitectureSet archs,
                          SymbolFlags flags = SymbolFlags::None,
                          bool copyStrings = true);

  using SymbolSeq = std::vector<Symbol *>;
  using const_symbol_iterator = SymbolSeq::const_iterator;
  using const_symbol_range = llvm::iterator_range<const_symbol_iterator>;

  const_symbol_range symbols() const { return _symbols; }
  const_symbol_range exports() const { return _symbols; }
  const_symbol_range undefineds() const { return _undefineds; }

  bool contains(SymbolKind kind, StringRef name,
                Symbol const **result = nullptr) const;

  llvm::Expected<std::unique_ptr<InterfaceFile>>
  extract(Architecture arch) const;
  llvm::Expected<std::unique_ptr<InterfaceFile>>
  remove(Architecture arch) const;
  llvm::Expected<std::unique_ptr<InterfaceFile>>
  merge(const InterfaceFile *otherInterface,
        bool allowArchitectureMerges = false) const;
  void printSymbols(ArchitectureSet archs) const;

private:
  void addSymbolImpl(SymbolKind kind, StringRef name, ArchitectureSet archs,
                     SymbolFlags flags, bool copyStrings = true);
  void addUndefinedSymbolImpl(SymbolKind kind, StringRef name,
                              ArchitectureSet archs, SymbolFlags flags,
                              bool copyStrings = true);
  void printSymbolsForArch(Architecture arch) const;

protected:
  StringRef copyString(StringRef string) {
    if (string.empty())
      return {};

    void *ptr = allocator.Allocate(string.size(), 1);
    memcpy(ptr, string.data(), string.size());
    return {reinterpret_cast<const char *>(ptr), string.size()};
  }

  llvm::BumpPtrAllocator allocator;
  SymbolSeq _symbols;
  SymbolSeq _undefineds;

  friend struct llvm::yaml::MappingTraits<const InterfaceFile *>;
};

TAPI_NAMESPACE_INTERNAL_END

#endif // TAPI_CORE_INTERFACE_FILE_H

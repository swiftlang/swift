//===- ExtendedIntefaceFile.h - TAPI Extended Interface File ----*- C++ -*-===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
///
/// \file
/// \brief A generic and abstract interface representation for linkable objects.
///        This could be an MachO executable, bundle, dylib, or text-based stub
///        file.
///
//===----------------------------------------------------------------------===//

#ifndef TAPI_CORE_EXTENDED_INTERFACE_FILE_H
#define TAPI_CORE_EXTENDED_INTERFACE_FILE_H

#include "Architecture.h"
#include "ArchitectureSet.h"
#include "File.h"
#include "InterfaceFile.h"
#include "InterfaceFileBase.h"
#include "STLExtras.h"
#include "XPI.h"
#include "XPISet.h"
#include "Defines.h"
#include "llvm/ADT/StringRef.h"
#include "llvm/ADT/iterator.h"
#include "llvm/Support/Error.h"

TAPI_NAMESPACE_INTERNAL_BEGIN

class ExtendedInterfaceFile : public InterfaceFileBase {
public:
  static bool classof(const File *file) {
    return file->kind() == File::Kind::ExtendedInterfaceFile;
  }

  ExtendedInterfaceFile()
      : InterfaceFileBase(File::Kind::ExtendedInterfaceFile),
        _symbols(new XPISet), _undefineds(new XPISet) {}
  ExtendedInterfaceFile(std::unique_ptr<XPISet> &&symbols)
      : InterfaceFileBase(File::Kind::ExtendedInterfaceFile),
        _symbols(std::move(symbols)), _undefineds(new XPISet) {}

  void addSymbol(XPIKind kind, StringRef name, ArchitectureSet archs,
                 SymbolFlags flags = SymbolFlags::None,
                 XPIAccess access = XPIAccess::Exported);

  ObjCClass *addObjCClass(StringRef name, ArchitectureSet archs,
                          XPIAccess access = XPIAccess::Exported,
                          ObjCClass *superClass = nullptr);
  ObjCSelector *addObjCSelector(ObjCContainer *container, StringRef name,
                                ArchitectureSet archs,
                                bool isInstanceMethod = false,
                                bool isDynamic = false,
                                XPIAccess access = XPIAccess::Exported);
  ObjCCategory *addObjCCategory(ObjCClass *baseClass, StringRef name,
                                ArchitectureSet archs,
                                XPIAccess access = XPIAccess::Exported);
  ObjCProtocol *addObjCProtocol(StringRef name, ArchitectureSet archs,
                                XPIAccess access = XPIAccess::Exported);

  void addUndefinedSymbol(XPIKind kind, StringRef name, ArchitectureSet archs,
                          SymbolFlags flags = SymbolFlags::None);

  using const_symbol_range = XPISet::const_symbol_range;
  using const_export_range = XPISet::const_export_range;
  using const_selector_range = XPISet::const_selector_range;

  const_symbol_range symbols() const { return _symbols->symbols(); }
  const_export_range exports() const { return _symbols->exports(); }
  const_selector_range selectors() const { return _symbols->selectors(); }
  const_symbol_range undefineds() const { return _undefineds->symbols(); }

  const XPISet &getSymbolSet() const { return *_symbols; }

  bool contains(XPIKind kind, StringRef name,
                XPI const **result = nullptr) const;

  llvm::Expected<std::unique_ptr<ExtendedInterfaceFile>>
  merge(const ExtendedInterfaceFile *otherInterface,
        bool allowArchitectureMerges = false) const;

  bool removeSymbol(XPIKind kind, StringRef name);
  bool removeSymbol(SymbolKind kind, StringRef name);

  void printSymbols(ArchitectureSet archs) const;

private:
  void printSymbolsForArch(Architecture arch) const;

protected:
  std::unique_ptr<XPISet> _symbols;
  std::unique_ptr<XPISet> _undefineds;
};

TAPI_NAMESPACE_INTERNAL_END

#endif // TAPI_CORE_EXTENDED_INTERFACE_FILE_H

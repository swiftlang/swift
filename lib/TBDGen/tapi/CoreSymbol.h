//===- tapi/Core/Symbol.h - TAPI Symbol -------------------------*- C++ -*-===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
///
/// \file
/// \brief API Symbol
///
//===----------------------------------------------------------------------===//

#ifndef TAPI_CORE_SYMBOL_H
#define TAPI_CORE_SYMBOL_H

#include "ArchitectureSet.h"
#include "LLVM.h"
#include "Defines.h"
#include "Symbol.h"
#include "tapi.h"
#include "llvm/ADT/StringRef.h"
#include "llvm/Support/Error.h"
#include "llvm/Support/raw_ostream.h"

TAPI_NAMESPACE_INTERNAL_BEGIN

using SymbolFlags = tapi::v1::SymbolFlags;

enum class SymbolKind : unsigned {
  GlobalSymbol,
  ObjectiveCClass,
  ObjectiveCClassEHType,
  ObjectiveCInstanceVariable,
};

class Symbol {
public:
  constexpr Symbol(SymbolKind kind, StringRef name,
                   ArchitectureSet architectures, SymbolFlags flags)
      : kind(kind), name(name), architectures(architectures), flags(flags) {}

  SymbolKind getKind() const { return kind; }
  StringRef getName() const { return name; }
  ArchitectureSet getArchitectures() const { return architectures; }
  void setArchitectures(ArchitectureSet archs) { architectures |= archs; }
  SymbolFlags getFlags() const { return flags; }

  bool isWeakDefined() const {
    return (flags & SymbolFlags::WeakDefined) == SymbolFlags::WeakDefined;
  }

  bool isWeakReferenced() const {
    return (flags & SymbolFlags::WeakReferenced) == SymbolFlags::WeakReferenced;
  }

  bool isThreadLocalValue() const {
    return (flags & SymbolFlags::ThreadLocalValue) ==
           SymbolFlags::ThreadLocalValue;
  }

  std::string getPrettyName(bool demangle) const;
  std::string getAnnotatedName(bool demangle = false) const;

  void print(raw_ostream &os) const;

  /// \brief Print APISymbol in human readable format.
  void dump(raw_ostream &os) const;
  void dump() const { dump(llvm::errs()); }

private:
  SymbolKind kind;
  StringRef name;
  ArchitectureSet architectures;
  SymbolFlags flags;
};

inline raw_ostream &operator<<(raw_ostream &os, const Symbol &symbol) {
  symbol.print(os);
  return os;
}

TAPI_NAMESPACE_INTERNAL_END

#endif // TAPI_CORE_SYMBOL_H

//===-- tapi/Symbol.h - TAPI Symbol -----------------------------*- C++ -*-===*\
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
///
/// \file
/// \brief Defines a symbol.
/// \since 1.0
///
//===----------------------------------------------------------------------===//
#ifndef TAPI_SYMBOL_H
#define TAPI_SYMBOL_H

#include "Defines.h"

///
/// \defgroup TAPI_SYMBOL Symbol API
/// \ingroup TAPI_CPP_API
///
/// @{
///

TAPI_NAMESPACE_V1_BEGIN

///
/// \brief Symbol flags.
/// \since 1.0
///
enum class SymbolFlags : unsigned {
  /// \brief No flags
  /// \since 1.0
  None = 0,

  /// \brief Thread-local value symbol
  /// \since 1.0
  ThreadLocalValue = 1U << 0,

  /// \brief Weak defined symbol
  /// \since 1.0
  WeakDefined = 1U << 1,

  /// \brief Weak referenced symbol
  /// \since 1.0
  WeakReferenced = 1U << 2,
};

inline SymbolFlags operator&(const SymbolFlags &lhs,
                             const SymbolFlags &rhs) noexcept {
  return static_cast<SymbolFlags>(static_cast<unsigned>(lhs) &
                                  static_cast<unsigned>(rhs));
}

///
/// \brief Provides query methods for symbols.
/// \since 1.0
///
class TAPI_PUBLIC Symbol {
public:
  template <typename Tp>
  Symbol(Tp &&name, SymbolFlags flags = SymbolFlags::None)
      : _name(std::forward<Tp>(name)), _flags(flags) {}

  ///
  /// \brief Get the symbol name as string.
  /// \return A string with the symbol name.
  /// \since 1.0
  ///
  inline const std::string &getName() const noexcept { return _name; }

  ///
  /// \brief Obtain the symbol flags.
  /// \return Returns the symbol flags.
  /// \since 1.0
  ///
  inline SymbolFlags getFlags() const noexcept { return _flags; }

  ///
  /// \brief Query if the symbol is thread-local.
  /// \return True if the symbol is a thread-local value, false otherwise.
  /// \since 1.0
  ///
  inline bool isThreadLocalValue() const noexcept {
    return (_flags & SymbolFlags::ThreadLocalValue) ==
           SymbolFlags::ThreadLocalValue;
  }

  ///
  /// \brief Query if the symbol is weak defined.
  /// \return True if the symbol is weak defined, false otherwise.
  /// \since 1.0
  ///
  inline bool isWeakDefined() const noexcept {
    return (_flags & SymbolFlags::WeakDefined) == SymbolFlags::WeakDefined;
  }

  ///
  /// \brief Query if the symbol is weak referenced.
  /// \return True if the symbol is weak referenced, false otherwise.
  /// \since 1.0
  ///
  inline bool isWeakReferenced() const noexcept {
    return (_flags & SymbolFlags::WeakReferenced) ==
           SymbolFlags::WeakReferenced;
  }

private:
  std::string _name;
  SymbolFlags _flags;
};

TAPI_NAMESPACE_V1_END

///
/// @}
///

#endif // TAPI_SYMBOL_H

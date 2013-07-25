//===--- Demangle.h - Interface to Swift symbol demangling -----------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2015 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See http://swift.org/LICENSE.txt for license information
// See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#ifndef __SWIFT_SIL_DEMANGLE_H__
#define __SWIFT_SIL_DEMANGLE_H__

#include "llvm/ADT/StringRef.h"
#include <string>

namespace swift {
namespace Demangle {
/// \brief Demangle the given string as a Swift symbol.
///
/// Typical usage:
/// \code
///   std::string aDemangledName =
/// swift::Demangler::demangleSymbol("SomeSwiftMangledName")
/// \endcode
///
/// \param mangled The mangled string.
///
///
/// \returns The demangled string - or the mangled string on failure.
///
std::string demangleSymbol(llvm::StringRef mangled);

/// \brief Demangle the given string as a Swift type.
///
/// Typical usage:
/// \code
///   std::string aDemangledName =
/// swift::Demangler::demangleType("SomeSwiftMangledName")
/// \endcode
///
/// \param mangled The mangled string.
///
///
/// \returns The demangled string - or the mangled string on failure.
std::string demangleType(llvm::StringRef mangled);

} // end namespace Demangle
} // end namespace swift

#endif

//===-- FunctionNameDemangle.h - Public demangling interface------*- C -*--===//
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
///
/// \file
/// This header declares functions in the libfunctionNameDemangle library,
/// which provides external access to Swift's demangler.
///
//===----------------------------------------------------------------------===//

#ifndef SWIFT_FUNCTION_NAME_DEMANGLE_H
#define SWIFT_FUNCTION_NAME_DEMANGLE_H

/// @{
/// Version constants for libfunctionNameDemangle library.

/// Major version changes when there are ABI or source incompatible changes.
#define FUNCTION_NAME_DEMANGLE_VERSION_MAJOR 0

/// Minor version changes when new APIs are added in ABI- and source-compatible
/// way.
#define FUNCTION_NAME_DEMANGLE_VERSION_MINOR 1

/// @}

#ifdef __cplusplus
extern "C" {
#endif

/// \brief Demangle Swift function names.
///
/// Note that this function has a generic name because it is called from
/// contexts where it is not appropriate to use code names.
///
/// \returns the length of the demangled function name (even if greater than the
/// size of the output buffer) or 0 if the input is not a Swift-mangled function
/// name (in which cases \p OutputBuffer is left untouched).
size_t fnd_get_demangled_name(const char *MangledName, char *OutputBuffer,
                              size_t Length);

#ifdef __cplusplus
} // extern "C"
#endif

#endif // SWIFT_FUNCTION_NAME_DEMANGLE_H


//===--- Paths.h - Swift Runtime path utility functions ---------*- C++ -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2022 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
// Functions that obtain paths that might be useful within the runtime.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_RUNTIME_UTILS_H
#define SWIFT_RUNTIME_UTILS_H

#include "swift/Runtime/Config.h"

/// Return the path of the libswiftCore library.
///
/// This can be used to locate files that are installed alongside the Swift
/// runtime library.
///
/// \return A string containing the full path to libswiftCore.  The string is
///         owned by the runtime and should not be freed.
SWIFT_RUNTIME_EXPORT
const char *
swift_getRuntimeLibraryPath();

/// Return the path of the Swift root.
///
/// If the path to libswiftCore is `/usr/local/swift/lib/libswiftCore.dylib`,
/// this function would return `/usr/local/swift`.
///
/// The path returned here can be overridden by setting the environment variable
/// SWIFT_ROOT.
///
/// \return A string containing the full path to the Swift root directory, based
///         either on the location of the Swift runtime, or on the `SWIFT_ROOT`
///         environment variable if set.  The string is owned by the runtime
///         and should not be freed.
SWIFT_RUNTIME_EXPORT
const char *
swift_getRootPath();

/// Return the path of the specified auxiliary executable.
///
/// This function will search for the auxiliary executable in the following
/// paths:
///
///   <swift-root>/libexec/swift/<platform>/<name>
///   <swift-root>/libexec/swift/<name>
///   <swift-root>/bin/<name>
///   <swift-root>/<name>
///
/// It will return the first of those that exists, but it does not test that
/// the file is indeed executable.
///
/// On Windows, it will automatically add `.exe` to the name, which means you
/// do not need to special case the name for Windows.
///
/// If you are using this function to locate a utility program for use by the
/// runtime, you should provide a way to override its location using an
/// environment variable.
///
/// If the executable cannot be found, it will return nullptr.
///
/// \param name      The name of the executable to locate.
///
/// \return A string containing the full path to the executable.  This string
///         should be released with `free()` when no longer required.
SWIFT_RUNTIME_EXPORT
char *
swift_copyAuxiliaryExecutablePath(const char *name);

#endif // SWIFT_RUNTIME_PATHS_H

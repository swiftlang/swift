//===--- PluginPaths.h - Toolchain-relative plugin path helpers -*- C++ -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2026 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
// Helpers for computing toolchain-relative Swift plugin search paths.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_DRIVER_PLUGINPATHS_H
#define SWIFT_DRIVER_PLUGINPATHS_H

#include "swift/Basic/LLVM.h"
#include "llvm/ADT/SmallVector.h"

namespace swift {
namespace driver {

/// Given a toolchain root (the directory containing bin/, lib/, etc.),
/// append the platform-appropriate in-process plugin server library path.
void appendInProcPluginServerPath(StringRef ToolchainRoot,
                                  llvm::SmallVectorImpl<char> &InProcPluginServerPath);

/// Given a toolchain root, append the platform-appropriate plugin search
/// directory path.
void appendPluginsPath(StringRef ToolchainRoot,
                       llvm::SmallVectorImpl<char> &PluginsPath);

/// Given a toolchain root, append the local plugin search directory path.
/// Only meaningful on Apple/Unix platforms.
#if defined(__APPLE__) || defined(__unix__)
void appendLocalPluginsPath(StringRef ToolchainRoot,
                            llvm::SmallVectorImpl<char> &LocalPluginsPath);
#endif

} // end namespace driver
} // end namespace swift

#endif

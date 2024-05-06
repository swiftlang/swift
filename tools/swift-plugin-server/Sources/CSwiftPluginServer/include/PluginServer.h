//===----------------------------------------------------------------------===//
//
// This source file is part of the Swift open source project
//
// Copyright (c) 2023 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See http://swift.org/LICENSE.txt for license information
// See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_PLUGINSERVER_PLUGINSERVER_H
#define SWIFT_PLUGINSERVER_PLUGINSERVER_H

#include <stddef.h>

#ifdef __cplusplus
extern "C" {
#endif

//===----------------------------------------------------------------------===//
// Dynamic link
//===----------------------------------------------------------------------===//

/// Load a dynamic link library, and return the handle.
void *PluginServer_load(const char *filename, const char **errorMessage);

/// Resolve a type metadata by a pair of the module name and the type name.
/// 'libraryHint' is a
const void *PluginServer_lookupMacroTypeMetadataByExternalName(
    const char *moduleName, const char *typeName, void *libraryHint,
    const char **errorMessage);

#ifdef __cplusplus
}
#endif

#endif /* SWIFT_PLUGINSERVER_PLUGINSERVER_H */

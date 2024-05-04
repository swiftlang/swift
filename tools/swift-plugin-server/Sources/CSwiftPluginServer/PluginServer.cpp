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

#include "PluginServer.h"
#include "swift/ABI/MetadataValues.h"
#include "swift/Demangling/Demangle.h"
#include "llvm/Support/DynamicLibrary.h"

#if defined(_WIN32)
#include <io.h>
#elif defined(__unix__) || defined(__APPLE__)
#include <dlfcn.h>
#include <unistd.h>
#endif

#include <string>
#include <errno.h>
#include <string.h>

using namespace swift;

void *PluginServer_load(const char *plugin, const char **errorMessage) {
  // Use a static allocation for the error as the client will not release the
  // string.  POSIX 2008 (IEEE-1003.1-2008) specifies that it is implementation
  // defined if `dlerror` is re-entrant.  Take advantage of that and make it
  // thread-unsafe.  This ensures that the string outlives the call permitting
  // the client to duplicate it.
  static std::string error;
  auto library = llvm::sys::DynamicLibrary::getLibrary(plugin, &error);
  if (library.isValid())
    return library.getOSSpecificHandle();
  *errorMessage = error.c_str();
  return nullptr;
}

const void *PluginServer_lookupMacroTypeMetadataByExternalName(
    const char *moduleName, const char *typeName, void *libraryHint,
    const char **errorMessage) {
  // Look up the type metadata accessor as a struct, enum, or class.
  const Demangle::Node::Kind typeKinds[] = {
      Demangle::Node::Kind::Structure,
      Demangle::Node::Kind::Enum,
      Demangle::Node::Kind::Class,
  };

  void *accessorAddr = nullptr;
  for (auto typeKind : typeKinds) {
    auto symbolName =
        mangledNameForTypeMetadataAccessor(moduleName, typeName, typeKind);

#if !defined(_WIN32)
    if (libraryHint == nullptr)
      libraryHint = RTLD_DEFAULT;
#endif
    accessorAddr = llvm::sys::DynamicLibrary{libraryHint}
                      .getAddressOfSymbol(symbolName.c_str());
    if (accessorAddr)
      break;
  }

  if (!accessorAddr)
    return nullptr;

  // Call the accessor to form type metadata.
  using MetadataAccessFunc = const void *(MetadataRequest);
  auto accessor = reinterpret_cast<MetadataAccessFunc*>(accessorAddr);
  return accessor(MetadataRequest(MetadataState::Complete));
}

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

#include <dlfcn.h>
#include <errno.h>
#include <string.h>
#include <unistd.h>

using namespace swift;

namespace {
struct ConnectionHandle {
  int inputFD;
  int outputFD;

  ConnectionHandle(int inputFD, int outputFD)
      : inputFD(inputFD), outputFD(outputFD) {}
};
} // namespace

const void *PluginServer_createConnection(const char **errorMessage) {
  // Duplicate the `stdin` file descriptor, which we will then use for
  // receiving messages from the plugin host.
  auto inputFD = dup(STDIN_FILENO);
  if (inputFD < 0) {
    *errorMessage = strerror(errno);
    return nullptr;
  }

  // Having duplicated the original standard-input descriptor, we close
  // `stdin` so that attempts by the plugin to read console input (which
  // are usually a mistake) return errors instead of blocking.
  if (close(STDIN_FILENO) < 0) {
    *errorMessage = strerror(errno);
    return nullptr;
  }

  // Duplicate the `stdout` file descriptor, which we will then use for
  // sending messages to the plugin host.
  auto outputFD = dup(STDOUT_FILENO);
  if (outputFD < 0) {
    *errorMessage = strerror(errno);
    return nullptr;
  }

  // Having duplicated the original standard-output descriptor, redirect
  // `stdout` to `stderr` so that all free-form text output goes there.
  if (dup2(STDERR_FILENO, STDOUT_FILENO) < 0) {
    *errorMessage = strerror(errno);
    return nullptr;
  }

  // Open a message channel for communicating with the plugin host.
  return new ConnectionHandle(inputFD, outputFD);
}

void PluginServer_destroyConnection(const void *connHandle) {
  const auto *conn = static_cast<const ConnectionHandle *>(connHandle);
  delete conn;
}

ssize_t PluginServer_read(const void *connHandle, void *data, size_t nbyte) {
  const auto *conn = static_cast<const ConnectionHandle *>(connHandle);
  return ::read(conn->inputFD, data, nbyte);
}

ssize_t PluginServer_write(const void *connHandle, const void *data,
                           size_t nbyte) {
  const auto *conn = static_cast<const ConnectionHandle *>(connHandle);
  return ::write(conn->outputFD, data, nbyte);
}

void *PluginServer_dlopen(const char *filename, const char **errorMessage) {
  auto *handle = ::dlopen(filename, RTLD_LAZY | RTLD_LOCAL);
  if (!handle) {
    *errorMessage = dlerror();
  }
  return handle;
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

    auto *handle = libraryHint ? libraryHint : RTLD_DEFAULT;
    accessorAddr = ::dlsym(handle, symbolName.c_str());
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

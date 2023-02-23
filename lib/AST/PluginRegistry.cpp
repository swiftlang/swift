//===--- PluginRegistry.cpp -----------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2023 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#include "swift/AST/PluginRegistry.h"

#include "swift/Basic/LLVM.h"
#include "swift/Basic/Program.h"
#include "swift/Basic/Sandbox.h"
#include "swift/Basic/StringExtras.h"
#include "llvm/Support/Endian.h"
#include "llvm/Support/FileSystem.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/Config/config.h"


#if defined(_WIN32)
#define WIN32_LEAN_AND_MEAN
#define NOMINMAX
#include <windows.h>
#else
#include <dlfcn.h>
#endif

#if HAVE_UNISTD_H
#include <unistd.h>
#elif defined(_WIN32)
#include <io.h>
#endif

extern "C" const void *swift_ASTGen_getCompilerPluginCapability(void *handle);
extern "C" void swift_ASTGen_destroyCompilerPluginCapability(void *value);

using namespace swift;

llvm::Error PluginRegistry::loadLibraryPlugin(StringRef path) {
  if (LoadedPluginLibraries.find(path) != LoadedPluginLibraries.end()) {
    // Already loaded.
    return llvm::Error::success();
  }
  void *lib = nullptr;
#if defined(_WIN32)
  lib = LoadLibraryA(path.str().c_str());
  if (!lib) {
    std::error_code ec(GetLastError(), std::system_category());
    return llvm::errorCodeToError(ec);
  }
#else
  lib = dlopen(path.str().c_str(), RTLD_LAZY | RTLD_LOCAL);
  if (!lib) {
    return llvm::createStringError(std::error_code(), dlerror());
  }
#endif
  LoadedPluginLibraries.insert({path, lib});
  return llvm::Error::success();
}

llvm::Expected<LoadedExecutablePlugin *>
PluginRegistry::loadExecutablePlugin(StringRef path) {
  llvm::sys::fs::file_status stat;
  if (auto err = llvm::sys::fs::status(path, stat)) {
    return llvm::errorCodeToError(err);
  }

  // See if the plugin is already loaded.
  auto &plugin = LoadedPluginExecutables[path];
  if (plugin) {
    // See if the loaded one is still usable.
    if (plugin->getLastModificationTime() == stat.getLastModificationTime())
      return plugin.get();

    // The plugin is updated. Close the previously opened plugin.
    plugin = nullptr;
  }

  if (!llvm::sys::fs::exists(stat)) {
    return llvm::createStringError(std::errc::no_such_file_or_directory,
                                   "not found");
  }

  if (!llvm::sys::fs::can_execute(path)) {
    return llvm::createStringError(std::errc::permission_denied,
                                   "not executable");
  }

  // Create command line arguments.
  SmallVector<StringRef, 4> command{path};

  // Apply sandboxing.
  llvm::BumpPtrAllocator Allocator;
  Sandbox::apply(command, Allocator);

  // Launch.
  auto childInfo = ExecuteWithPipe(command[0], command);
  if (!childInfo) {
    return llvm::errorCodeToError(childInfo.getError());
  }

  plugin = std::unique_ptr<LoadedExecutablePlugin>(new LoadedExecutablePlugin(
      childInfo->Pid, stat.getLastModificationTime(),
      childInfo->ReadFileDescriptor, childInfo->WriteFileDescriptor));

  return plugin.get();
}

LoadedExecutablePlugin::LoadedExecutablePlugin(
    llvm::sys::procid_t pid, llvm::sys::TimePoint<> LastModificationTime,
    int inputFileDescriptor, int outputFileDescriptor)
    : pid(pid), LastModificationTime(LastModificationTime),
      inputFileDescriptor(inputFileDescriptor),
      outputFileDescriptor(outputFileDescriptor) {}

LoadedExecutablePlugin::~LoadedExecutablePlugin() {
  // Close the pipes.
  close(inputFileDescriptor);
  close(outputFileDescriptor);

  // Let ASTGen to cleanup things.
  this->cleanup();
}

ssize_t LoadedExecutablePlugin::read(void *buf, size_t nbyte) const {
  ssize_t bytesToRead = nbyte;
  void *ptr = buf;

  while (bytesToRead > 0) {
    ssize_t readingSize = std::min(ssize_t(INT32_MAX), bytesToRead);
    ssize_t readSize = ::read(inputFileDescriptor, ptr, readingSize);
    if (readSize == 0) {
      break;
    }
    ptr = static_cast<char *>(ptr) + readSize;
    bytesToRead -= readSize;
  }

  return nbyte - bytesToRead;
}

ssize_t LoadedExecutablePlugin::write(const void *buf, size_t nbyte) const {
  ssize_t bytesToWrite = nbyte;
  const void *ptr = buf;

  while (bytesToWrite > 0) {
    ssize_t writingSize = std::min(ssize_t(INT32_MAX), bytesToWrite);
    ssize_t writtenSize = ::write(outputFileDescriptor, ptr, writingSize);
    if (writtenSize == 0) {
      break;
    }
    ptr = static_cast<const char *>(ptr) + writtenSize;
    bytesToWrite -= writtenSize;
  }
  return nbyte - bytesToWrite;
}

llvm::Error LoadedExecutablePlugin::sendMessage(llvm::StringRef message) const {
  ssize_t writtenSize = 0;

  const char *data = message.data();
  size_t size = message.size();

  // Write header (message size).
  uint64_t header = llvm::support::endian::byte_swap(
      uint64_t(size), llvm::support::endianness::little);
  writtenSize = write(&header, sizeof(header));
  assert(writtenSize == sizeof(header) &&
         "failed to write plugin message header");

  // Write message.
  writtenSize = write(data, size);
  assert(writtenSize == ssize_t(size) && "failed to write plugin message data");

  return llvm::Error::success();
}

llvm::Expected<std::string> LoadedExecutablePlugin::waitForNextMessage() const {
  ssize_t readSize = 0;

  // Read header (message size).
  uint64_t header;
  readSize = read(&header, sizeof(header));

  // FIXME: Error handling. Disconnection, etc.
  assert(readSize == sizeof(header) && "failed to read plugin message header");

  size_t size = llvm::support::endian::read<uint64_t>(
      &header, llvm::support::endianness::little);

  // Read message.
  std::string message;
  message.reserve(size);
  auto sizeToRead = size;
  while (sizeToRead > 0) {
    char buffer[4096];
    readSize = read(buffer, std::min(sizeof(buffer), sizeToRead));
    sizeToRead -= readSize;
    message.append(buffer, readSize);
  }

  return message;
}

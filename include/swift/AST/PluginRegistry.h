//===--- PluginRegistry.h ---------------------------------------*- C++ -*-===//
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

#include "llvm/ADT/ArrayRef.h"
#include "llvm/ADT/StringMap.h"
#include "llvm/ADT/StringRef.h"
#include "llvm/Support/Chrono.h"
#include "llvm/Support/Error.h"
#include "llvm/Support/Program.h"

#include <mutex>
#include <vector>

namespace swift {

class LoadedExecutablePlugin {
  const llvm::sys::procid_t pid;
  const llvm::sys::TimePoint<> LastModificationTime;
  const int inputFileDescriptor;
  const int outputFileDescriptor;

  /// Opaque value of the protocol capability of the pluugin. This is a
  /// value from ASTGen.
  const void *capability = nullptr;

  /// Cleanup function to call ASTGen.
  std::function<void(void)> cleanup;

  std::mutex mtx;

  ssize_t write(const void *buf, size_t nbyte) const;
  ssize_t read(void *buf, size_t nbyte) const;

public:
  LoadedExecutablePlugin(llvm::sys::procid_t pid,
                         llvm::sys::TimePoint<> LastModificationTime,
                         int inputFileDescriptor, int outputFileDescriptor);
  ~LoadedExecutablePlugin();
  llvm::sys::TimePoint<> getLastModificationTime() const {
    return LastModificationTime;
  }

  void lock() { mtx.lock(); }
  void unlock() { mtx.unlock(); }

  /// Send a message to the plugin.
  llvm::Error sendMessage(llvm::StringRef message) const;

  /// Wait for a message from plugin and returns it.
  llvm::Expected<std::string> waitForNextMessage() const;

  bool isInitialized() const { return bool(cleanup); }
  void setCleanup(std::function<void(void)> cleanup) {
    this->cleanup = cleanup;
  }

  llvm::sys::procid_t getPid() { return pid; }

  const void *getCapability() { return capability; };
  void setCapability(const void *newValue) { capability = newValue; };
};

class PluginRegistry {
  /// Record of loaded plugin library modules.
  llvm::StringMap<void *> LoadedPluginLibraries;

  /// Record of loaded plugin executables.
  llvm::StringMap<std::unique_ptr<LoadedExecutablePlugin>>
      LoadedPluginExecutables;

public:
  llvm::Expected<void *> loadLibraryPlugin(llvm::StringRef path);
  llvm::Expected<LoadedExecutablePlugin *>
  loadExecutablePlugin(llvm::StringRef path);

  const llvm::StringMap<void *> &getLoadedLibraryPlugins() const {
    return LoadedPluginLibraries;
  }
};

} // namespace swift

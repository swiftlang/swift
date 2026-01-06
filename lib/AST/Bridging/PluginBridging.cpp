//===--- Bridging/PluginBridging.cpp --------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2022-2024 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#include "swift/AST/ASTBridging.h"

#include "swift/AST/PluginRegistry.h"

using namespace swift;

//===----------------------------------------------------------------------===//
// MARK: Plugin
//===----------------------------------------------------------------------===//

PluginCapabilityPtr Plugin_getCapability(PluginHandle handle) {
  auto *plugin = static_cast<CompilerPlugin *>(handle);
  return plugin->getCapability();
}

void Plugin_setCapability(PluginHandle handle, PluginCapabilityPtr data) {
  auto *plugin = static_cast<CompilerPlugin *>(handle);
  plugin->setCapability(data);
}

void Plugin_lock(PluginHandle handle) {
  auto *plugin = static_cast<CompilerPlugin *>(handle);
  plugin->lock();
}

void Plugin_unlock(PluginHandle handle) {
  auto *plugin = static_cast<CompilerPlugin *>(handle);
  plugin->unlock();
}

bool Plugin_spawnIfNeeded(PluginHandle handle) {
  auto *plugin = static_cast<CompilerPlugin *>(handle);
  auto error = plugin->spawnIfNeeded();
  bool hadError(error);
  llvm::consumeError(std::move(error));
  return hadError;
}

bool Plugin_sendMessage(PluginHandle handle, const BridgedData data) {
  auto *plugin = static_cast<CompilerPlugin *>(handle);
  StringRef message(data.BaseAddress, data.Length);
  auto error = plugin->sendMessage(message);
  if (error) {
    // FIXME: Pass the error message back to the caller.
    llvm::consumeError(std::move(error));
    //    llvm::handleAllErrors(std::move(error), [](const llvm::ErrorInfoBase
    //    &err) {
    //      llvm::errs() << err.message() << "\n";
    //    });
    return true;
  }
  return false;
}

bool Plugin_waitForNextMessage(PluginHandle handle, BridgedData *out) {
  auto *plugin = static_cast<CompilerPlugin *>(handle);
  auto result = plugin->waitForNextMessage();
  if (!result) {
    // FIXME: Pass the error message back to the caller.
    llvm::consumeError(result.takeError());
    //    llvm::handleAllErrors(result.takeError(), [](const llvm::ErrorInfoBase
    //    &err) {
    //      llvm::errs() << err.message() << "\n";
    //    });
    return true;
  }
  auto &message = result.get();
  auto size = message.size();
  auto outPtr = malloc(size);
  memcpy(outPtr, message.data(), size);
  *out = BridgedData{(const char *)outPtr, size};
  return false;
}

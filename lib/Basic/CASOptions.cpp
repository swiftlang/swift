//===--- CASOptions.cpp - CAS & caching options ---------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2023 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
//  This file defines the CASOptions class, which provides various
//  CAS and caching flags.
//
//===----------------------------------------------------------------------===//

#include "swift/Basic/CASOptions.h"

using namespace swift;

void CASOptions::enumerateCASConfigurationFlags(
      llvm::function_ref<void(llvm::StringRef)> Callback) const {
  if (EnableCaching) {
    Callback("-cache-compile-job");
    if (!Config.CASPath.empty()) {
      Callback("-cas-path");
      Callback(Config.CASPath);
    }
    if (!Config.PluginPath.empty()) {
      Callback("-cas-plugin-path");
      Callback(Config.PluginPath);
      for (auto Opt : Config.PluginOptions) {
        Callback("-cas-plugin-option");
        Callback((llvm::Twine(Opt.first) + "=" + Opt.second).str());
      }
    }
  }
}

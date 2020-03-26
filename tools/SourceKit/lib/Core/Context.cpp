//===--- Context.cpp ------------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2017 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#include "SourceKit/Core/Context.h"
#include "SourceKit/Core/LangSupport.h"
#include "SourceKit/Core/NotificationCenter.h"

using namespace SourceKit;

GlobalConfig::Settings
GlobalConfig::update(Optional<bool> OptimizeForIDE) {
  llvm::sys::ScopedLock L(Mtx);
  if (OptimizeForIDE.hasValue())
    State.OptimizeForIDE = *OptimizeForIDE;
  return State;
};

bool GlobalConfig::shouldOptimizeForIDE() const {
  llvm::sys::ScopedLock L(Mtx);
  return State.OptimizeForIDE;
}

SourceKit::Context::Context(StringRef RuntimeLibPath,
    llvm::function_ref<std::unique_ptr<LangSupport>(Context &)>
    LangSupportFactoryFn,
    bool shouldDispatchNotificationsOnMain) : RuntimeLibPath(RuntimeLibPath),
    NotificationCtr(new NotificationCenter(shouldDispatchNotificationsOnMain)),
    Config(new GlobalConfig()) {
  // Should be called last after everything is initialized.
  SwiftLang = LangSupportFactoryFn(*this);
}

SourceKit::Context::~Context() {
}

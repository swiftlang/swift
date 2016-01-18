//===--- Context.h - --------------------------------------------*- C++ -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2016 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See http://swift.org/LICENSE.txt for license information
// See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#ifndef LLVM_SOURCEKIT_CORE_CONTEXT_H
#define LLVM_SOURCEKIT_CORE_CONTEXT_H

#include "SourceKit/Core/LLVM.h"
#include "llvm/ADT/StringRef.h"
#include <memory>
#include <string>

namespace llvm {
  class MemoryBuffer;
}

namespace SourceKit {
  class LangSupport;
  class NotificationCenter;

class Context {
  std::string RuntimeLibPath;
  std::unique_ptr<LangSupport> SwiftLang;
  std::unique_ptr<NotificationCenter> NotificationCtr;

public:
  explicit Context(StringRef RuntimeLibPath);
  ~Context();

  StringRef getRuntimeLibPath() const { return RuntimeLibPath; }

  LangSupport &getSwiftLangSupport() { return *SwiftLang; }

  NotificationCenter &getNotificationCenter() { return *NotificationCtr; }
};

} // namespace SourceKit

#endif

//===--- NotificationCenter.h - ---------------------------------*- C++ -*-===//
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

#ifndef LLVM_SOURCEKIT_CORE_NOTIFICATIONCENTER_H
#define LLVM_SOURCEKIT_CORE_NOTIFICATIONCENTER_H

#include "SourceKit/Core/LLVM.h"
#include "llvm/Support/Mutex.h"
#include <functional>
#include <vector>

namespace SourceKit {

typedef std::function<void(StringRef DocumentName)>
    DocumentUpdateNotificationReceiver;

class NotificationCenter {
  bool DispatchToMain;
  std::vector<DocumentUpdateNotificationReceiver> DocUpdReceivers;
  mutable llvm::sys::Mutex Mtx;

public:
  explicit NotificationCenter(bool dispatchToMain);
  ~NotificationCenter();

  void addDocumentUpdateNotificationReceiver(
      DocumentUpdateNotificationReceiver Receiver);

  void postDocumentUpdateNotification(StringRef DocumentName) const;
};

} // namespace SourceKit

#endif

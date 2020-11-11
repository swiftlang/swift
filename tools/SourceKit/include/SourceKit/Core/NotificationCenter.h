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
#include "SourceKit/Support/Tracing.h"
#include "SourceKit/Support/UIdent.h"
#include "llvm/Support/Mutex.h"
#include <functional>
#include <vector>

namespace SourceKit {

struct DiagnosticEntryInfo;

typedef std::function<void(StringRef DocumentName)>
    DocumentUpdateNotificationReceiver;

typedef std::function<void(uint64_t CompileID, trace::OperationKind,
                           const trace::SwiftInvocation &)>
    CompileWillStartNotificationReceiver;
typedef std::function<void(uint64_t CompileID, trace::OperationKind,
                           ArrayRef<DiagnosticEntryInfo>)>
    CompileDidFinishNotificationReceiver;

class NotificationCenter {
  bool DispatchToMain;
  std::vector<DocumentUpdateNotificationReceiver> DocUpdReceivers;
  std::vector<std::function<void(void)>> TestReceivers;
  std::vector<std::function<void(void)>> SemaEnabledReceivers;
  std::vector<CompileWillStartNotificationReceiver> CompileWillStartReceivers;
  std::vector<CompileDidFinishNotificationReceiver> CompileDidFinishReceivers;
  mutable llvm::sys::Mutex Mtx;

public:
  explicit NotificationCenter(bool dispatchToMain);
  ~NotificationCenter();

  void addDocumentUpdateNotificationReceiver(
      DocumentUpdateNotificationReceiver Receiver);
  void addTestNotificationReceiver(std::function<void(void)> Receiver);
  void addSemaEnabledNotificationReceiver(std::function<void(void)> Receiver);
  void addCompileWillStartNotificationReceiver(
      CompileWillStartNotificationReceiver Receiver);
  void addCompileDidFinishNotificationReceiver(
      CompileDidFinishNotificationReceiver Receiver);

  void postDocumentUpdateNotification(StringRef DocumentName) const;
  void postTestNotification() const;
  void postSemaEnabledNotification() const;
  void
  postCompileWillStartNotification(uint64_t CompileID,
                                   trace::OperationKind OpKind,
                                   const trace::SwiftInvocation &Inv) const;
  void postCompileDidFinishNotification(
      uint64_t CompileID, trace::OperationKind OpKind,
      ArrayRef<DiagnosticEntryInfo> Diagnostics) const;
};

} // namespace SourceKit

#endif

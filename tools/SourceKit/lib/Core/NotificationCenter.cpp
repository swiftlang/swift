//===--- NotificationCenter.cpp -------------------------------------------===//
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

#include "SourceKit/Core/NotificationCenter.h"
#include "SourceKit/Core/LangSupport.h"
#include "SourceKit/Support/Concurrency.h"

using namespace SourceKit;

NotificationCenter::NotificationCenter(bool dispatchToMain)
  : DispatchToMain(dispatchToMain) {
}
NotificationCenter::~NotificationCenter() {}

void NotificationCenter::addDocumentUpdateNotificationReceiver(
    DocumentUpdateNotificationReceiver Receiver) {

  llvm::sys::ScopedLock L(Mtx);
  DocUpdReceivers.push_back(Receiver);
}

void NotificationCenter::addTestNotificationReceiver(
    std::function<void(void)> Receiver) {
  llvm::sys::ScopedLock L(Mtx);
  TestReceivers.push_back(std::move(Receiver));
}
void NotificationCenter::addSemaEnabledNotificationReceiver(
    std::function<void(void)> Receiver) {
  llvm::sys::ScopedLock L(Mtx);
  SemaEnabledReceivers.push_back(std::move(Receiver));
}
void NotificationCenter::addCompileWillStartNotificationReceiver(
    CompileWillStartNotificationReceiver Receiver) {
  llvm::sys::ScopedLock L(Mtx);
  CompileWillStartReceivers.push_back(std::move(Receiver));
}
void NotificationCenter::addCompileDidFinishNotificationReceiver(
    CompileDidFinishNotificationReceiver Receiver) {
  llvm::sys::ScopedLock L(Mtx);
  CompileDidFinishReceivers.push_back(std::move(Receiver));
}

#define POST_NOTIFICATION(Receivers, Args...)                                  \
  do {                                                                         \
    decltype(Receivers) recvs;                                                 \
    {                                                                          \
      llvm::sys::ScopedLock L(Mtx);                                            \
      recvs = Receivers;                                                       \
    }                                                                          \
    auto sendNote = [=] {                                                      \
      for (auto &Fn : recvs)                                                   \
        Fn(Args);                                                              \
    };                                                                         \
    if (DispatchToMain)                                                        \
      WorkQueue::dispatchOnMain(sendNote);                                     \
    else                                                                       \
      sendNote();                                                              \
  } while (0)

void NotificationCenter::postDocumentUpdateNotification(
    StringRef DocumentName) const {
  std::string docName = DocumentName.str();
  POST_NOTIFICATION(DocUpdReceivers, docName);
}
void NotificationCenter::postTestNotification() const {
  POST_NOTIFICATION(TestReceivers, );
}
void NotificationCenter::postSemaEnabledNotification() const {
  POST_NOTIFICATION(SemaEnabledReceivers, );
}
void NotificationCenter::postCompileWillStartNotification(
    uint64_t CompileID, trace::OperationKind OpKind,
    const trace::SwiftInvocation &Inv) const {
  trace::SwiftInvocation inv(Inv);
  POST_NOTIFICATION(CompileWillStartReceivers, CompileID, OpKind, inv);
}
void NotificationCenter::postCompileDidFinishNotification(
    uint64_t CompileID, trace::OperationKind OpKind,
    ArrayRef<DiagnosticEntryInfo> Diagnostics) const {
  std::vector<DiagnosticEntryInfo> diags(Diagnostics);
  POST_NOTIFICATION(CompileDidFinishReceivers, CompileID, OpKind, diags);
}

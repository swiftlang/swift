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

void NotificationCenter::postDocumentUpdateNotification(
    StringRef DocumentName) const {

  std::vector<DocumentUpdateNotificationReceiver> recvs;
  {
    llvm::sys::ScopedLock L(Mtx);
    recvs = DocUpdReceivers;
  }
  std::string docName = DocumentName.str();
  auto sendNote = [recvs, docName]{
    for (auto &Fn : recvs)
      Fn(docName);
  };
  if (DispatchToMain)
    WorkQueue::dispatchOnMain(sendNote);
  else
    sendNote();
}

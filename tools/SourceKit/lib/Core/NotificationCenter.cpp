#include "SourceKit/Core/NotificationCenter.h"
#include "SourceKit/Support/Concurrency.h"

using namespace SourceKit;

void NotificationCenter::addDocumentUpdateNotificationReceiver(
    DocumentUpdateNotificationReceiver Receiver) {

  WorkQueue::dispatchOnMain([this, Receiver]{
    DocUpdReceivers.push_back(Receiver);
  });
}

void NotificationCenter::postDocumentUpdateNotification(
    StringRef DocumentName) const {
  
  std::string DocName = DocumentName;
  WorkQueue::dispatchOnMain([this, DocName]{
    for (auto &Fn : DocUpdReceivers)
      Fn(DocName);
  });
}

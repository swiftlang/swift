#ifndef LLVM_SOURCEKIT_CORE_NOTIFICATIONCENTER_H
#define LLVM_SOURCEKIT_CORE_NOTIFICATIONCENTER_H

#include "SourceKit/Core/LLVM.h"
#include <functional>
#include <vector>

namespace SourceKit {

typedef std::function<void(StringRef DocumentName)>
    DocumentUpdateNotificationReceiver;

class NotificationCenter {
  std::vector<DocumentUpdateNotificationReceiver> DocUpdReceivers;

public:
  void addDocumentUpdateNotificationReceiver(
      DocumentUpdateNotificationReceiver Receiver);

  void postDocumentUpdateNotification(StringRef DocumentName) const;
};

} // namespace SourceKit

#endif

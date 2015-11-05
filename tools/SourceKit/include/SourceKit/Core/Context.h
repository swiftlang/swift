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

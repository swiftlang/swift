#include "SourceKit/Core/Context.h"
#include "SourceKit/Core/LangSupport.h"
#include "SourceKit/Core/NotificationCenter.h"

using namespace SourceKit;

SourceKit::Context::Context(StringRef RuntimeLibPath)
  : RuntimeLibPath(RuntimeLibPath) {
  NotificationCtr.reset(new NotificationCenter());
  SwiftLang = std::move(LangSupport::createSwiftLangSupport(*this));
}

SourceKit::Context::~Context() {
}

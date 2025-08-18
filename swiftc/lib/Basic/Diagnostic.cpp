#include "swiftc/Basic/Diagnostic.h"

using namespace swiftc;

void DiagnosticEngine::diagnose(DiagnosticLevel level, SourceLoc loc, StringRef message) {
  Diagnostics.emplace_back(level, loc, message);
  
  if (level == DiagnosticLevel::Error) {
    HasErrors = true;
  }
}
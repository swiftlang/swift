#ifndef SWIFTC_BASIC_DIAGNOSTIC_H
#define SWIFTC_BASIC_DIAGNOSTIC_H

#include "swiftc/Basic/LLVM.h"
#include "swiftc/Basic/SourceLoc.h"
#include <string>
#include <vector>

namespace swiftc {

enum class DiagnosticLevel {
  Note,
  Warning,
  Error
};

class DiagnosticEngine {
public:
  struct Diagnostic {
    DiagnosticLevel Level;
    SourceLoc Location;
    std::string Message;
    
    Diagnostic(DiagnosticLevel level, SourceLoc loc, StringRef message)
        : Level(level), Location(loc), Message(message.str()) {}
  };

private:
  std::vector<Diagnostic> Diagnostics;
  bool HasErrors = false;

public:
  DiagnosticEngine() = default;

  void diagnose(DiagnosticLevel level, SourceLoc loc, StringRef message);
  
  void diagnoseNote(SourceLoc loc, StringRef message) {
    diagnose(DiagnosticLevel::Note, loc, message);
  }
  
  void diagnoseWarning(SourceLoc loc, StringRef message) {
    diagnose(DiagnosticLevel::Warning, loc, message);
  }
  
  void diagnoseError(SourceLoc loc, StringRef message) {
    diagnose(DiagnosticLevel::Error, loc, message);
  }

  bool hasErrors() const { return HasErrors; }
  const std::vector<Diagnostic>& getDiagnostics() const { return Diagnostics; }
  
  void clear() {
    Diagnostics.clear();
    HasErrors = false;
  }
};

} // namespace swiftc

#endif // SWIFTC_BASIC_DIAGNOSTIC_H
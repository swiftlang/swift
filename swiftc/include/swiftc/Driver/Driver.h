#ifndef SWIFTC_DRIVER_DRIVER_H
#define SWIFTC_DRIVER_DRIVER_H

#include "swiftc/Basic/LLVM.h"
#include "swiftc/Basic/Diagnostic.h"

namespace swiftc {

/// Main compiler driver that orchestrates all compilation phases.
class Driver {
  DiagnosticEngine Diags;

public:
  Driver();

  /// Compile a Swift source file to an executable.
  bool compileFile(StringRef filename, StringRef outputPath);

  /// Get the diagnostic engine.
  DiagnosticEngine& getDiagnostics() { return Diags; }

  /// Print compiler information.
  void printCompilerInfo();

private:
  /// Link object file to create executable.
  bool linkExecutable(StringRef objectFile, StringRef executablePath);
};

} // namespace swiftc

#endif // SWIFTC_DRIVER_DRIVER_H
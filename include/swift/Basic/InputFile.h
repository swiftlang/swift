//
//  InputFile.h
//  Swift
//
//  Created by David Ungar on 12/10/17.
//

#ifndef InputFile_h
#define InputFile_h

#include "llvm/Support/MemoryBuffer.h"
#include <string>
#include <vector>

namespace swift {

struct OutputPaths {
  /// The specified output file (.o).
  std::string OutputFilename;

  /// The path to which we should emit an Objective-C header for the module.
  std::string ObjCHeaderOutputPath;

  /// The path to which we should emit a serialized module.
  std::string ModuleOutputPath;

  /// The path to which we should emit a module documentation file.
  std::string ModuleDocOutputPath;

  /// The path to which we should output a Make-style dependencies file.
  std::string DependenciesFilePath;

  /// The path to which we should output a Swift reference dependencies file.
  std::string ReferenceDependenciesFilePath;

  /// Path to a file which should contain serialized diagnostics for this
  /// frontend invocation.
  std::string SerializedDiagnosticsPath;

  /// The path to which we should output a loaded module trace file.
  std::string LoadedModuleTracePath;

  /// The path to which we should output a TBD file.
  std::string TBDPath;

  OutputPaths(unsigned i, Optional<std::vector<std::string>> &objCHeaderOutputs,
              Optional<std::vector<std::string>> &moduleOutput,
              Optional<std::vector<std::string>> &moduleDocOutputs,
              Optional<std::vector<std::string>> &dependenciesFiles,
              Optional<std::vector<std::string>> &referenceDependenciesFiles,
              Optional<std::vector<std::string>> &serializedDiagnostics,
              Optional<std::vector<std::string>> &loadedModuleTrace,
              Optional<std::vector<std::string>> &TBDs)
      : ObjCHeaderOutputPath(ith(objCHeaderOutputs, i)),
        ModuleOutputPath(ith(moduleOutput, i)),
        ModuleDocOutputPath(ith(moduleDocOutputs, i)),
        DependenciesFilePath(ith(dependenciesFiles, i)),
        ReferenceDependenciesFilePath(ith(referenceDependenciesFiles, i)),
        SerializedDiagnosticsPath(ith(serializedDiagnostics, i)),
        LoadedModuleTracePath(ith(loadedModuleTrace, i)),
        TBDPath(ith(TBDs, i)) {}

  OutputPaths() = default;
  OutputPaths(const OutputPaths &) = default;

private:
  static std::string ith(Optional<std::vector<std::string>> &names,
                         unsigned i) {
    return !names ? "" : (*names)[i];
  }
};

enum class InputFileKind {
  IFK_None,
  IFK_Swift,
  IFK_Swift_Library,
  IFK_Swift_REPL,
  IFK_SIL,
  IFK_LLVM_IR
};

// Inputs may include buffers that override contents, and eventually should
// always include a buffer.
class InputFile {
  std::string Filename;
  bool IsPrimary;
  /// Null if the contents are not overridden.
  llvm::MemoryBuffer *Buffer;
  OutputPaths Outputs;

public:
  /// Does not take ownership of \p buffer. Does take ownership of (copy) a
  /// string.
  InputFile(StringRef name, bool isPrimary,
            llvm::MemoryBuffer *buffer = nullptr)
      : Filename(name), IsPrimary(isPrimary), Buffer(buffer) {
    assert(name.begin() != Filename.c_str());
    assert(!name.empty() && "Empty strings signify no inputs in other places");
  }

  bool isPrimary() const { return IsPrimary; }
  llvm::MemoryBuffer *buffer() const { return Buffer; }
  StringRef file() const { return Filename; }
  const OutputPaths &outputs() const { return Outputs; }
  // FIXME: dmu can drop malleable?
  OutputPaths &malleableOutputs() { return Outputs; }

  /// Return Swift-standard file name from a buffer name set by
  /// llvm::MemoryBuffer::getFileOrSTDIN, which uses "<stdin>" instead of "-".
  static StringRef convertBufferNameFromLLVM_getFileOrSTDIN_toSwiftConventions(
      StringRef filename) {
    return filename.equals("<stdin>") ? "-" : filename;
  }
};
} // namespace swift

#endif /* InputFile_h */

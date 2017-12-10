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
    
    OutputPaths(unsigned i,
                Optional<std::vector<std::string>> &objCHeaderOutputs,
                Optional<std::vector<std::string>> &moduleOutput,
                Optional<std::vector<std::string>> &moduleDocOutputs,
                Optional<std::vector<std::string>> &dependenciesFiles,
                Optional<std::vector<std::string>> &referenceDependenciesFiles,
                Optional<std::vector<std::string>> &serializedDiagnostics,
                Optional<std::vector<std::string>> &loadedModuleTrace,
                Optional<std::vector<std::string>> &TBDs
                ) :
    ObjCHeaderOutputPath(ith(objCHeaderOutputs, i)),
    ModuleOutputPath(ith(moduleOutput, i)),
    ModuleDocOutputPath(ith(moduleDocOutputs, i)),
    DependenciesFilePath(ith(dependenciesFiles, i)),
    ReferenceDependenciesFilePath(ith(referenceDependenciesFiles, i)),
    SerializedDiagnosticsPath(ith(serializedDiagnostics, i)),
    LoadedModuleTracePath(ith(loadedModuleTrace, i)),
    TBDPath(ith(TBDs, i))
    {  }
    
    OutputPaths() = default;
    OutputPaths(const OutputPaths &) = default;
    
  private:
    static std::string ith(Optional<std::vector<std::string>> &names, unsigned i) {
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
      assert(!name.empty() && "Empty strings signify no inputs in other places");
    }
    
    bool getIsPrimary() const { return IsPrimary; }
    llvm::MemoryBuffer *getBuffer() const { return Buffer; }
    StringRef getFile() const { return Filename; }
    const OutputPaths &outputs() const { return Outputs; }
    OutputPaths &malleableOutputs() { return Outputs; }
  };
}

#endif /* InputFile_h */

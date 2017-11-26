//===--- FrontendOptions.h --------------------------------------*- C++ -*-===//
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

#ifndef SWIFT_FRONTEND_FRONTENDOPTIONS_H
#define SWIFT_FRONTEND_FRONTENDOPTIONS_H

#include "swift/AST/Module.h"
#include "llvm/ADT/Hashing.h"

#include <string>
#include <vector>

namespace llvm {
  class MemoryBuffer;
}

namespace swift {

enum class InputFileKind {
  IFK_None,
  IFK_Swift,
  IFK_Swift_Library,
  IFK_Swift_REPL,
  IFK_SIL,
  IFK_LLVM_IR
};

// Inputs may be files, buffers, or buffers substituting for files.
class InputFileOrBuffer {
  /// Empty if no name
  std::string Filename;
  llvm::MemoryBuffer *Buffer;
  bool IsPrimary;

public:
  /// Does not take ownership of \p buffer. Does take ownership of a string.
  InputFileOrBuffer(StringRef name, llvm::MemoryBuffer *buffer, bool isPrimary)
      : Filename(name), Buffer(buffer), IsPrimary(isPrimary) {}

  InputFileOrBuffer(const InputFileOrBuffer &other)
      : Filename(other.Filename), Buffer(other.Buffer),
        IsPrimary(other.IsPrimary) {}

  InputFileOrBuffer(InputFileOrBuffer &&) = default;

  InputFileOrBuffer &operator=(const InputFileOrBuffer &) = default;

  static InputFileOrBuffer createFile(StringRef Filename, bool isPrimary,
                                      llvm::MemoryBuffer *Buffer = nullptr) {
    return InputFileOrBuffer(Filename, Buffer, isPrimary);
  }
  static InputFileOrBuffer createBuffer(llvm::MemoryBuffer *Buffer,
                                        bool isPrimary) {
    return InputFileOrBuffer(StringRef(), Buffer, isPrimary);
  }

  bool getIsPrimary() const { return IsPrimary; }
  llvm::MemoryBuffer *getBuffer() const { return Buffer; }
  StringRef getFile() const { return Filename; }

  void setBuffer(llvm::MemoryBuffer *buffer) { Buffer = buffer; }

  InputFileOrBuffer asPrimary() {
    return InputFileOrBuffer(Filename, Buffer, true);
  }
};

/// Information about all the inputs to the frontend.
class FrontendInputs {
  friend class ArgsToFrontendInputsConverter;

  std::vector<InputFileOrBuffer> Inputs;
  typedef llvm::StringMap<unsigned> InputFileMap;
  InputFileMap PrimaryFiles;

public:
  // Readers:

  ArrayRef<InputFileOrBuffer> getInputs() const { return Inputs; }

  // std::vector<InputFileOrBuffer> &getMalleableInputs() { return Inputs; }

  void transformInputs(
      llvm::function_ref<InputFileOrBuffer(const InputFileOrBuffer &input)>
          fn) {
    for (auto i : indices(getInputs())) {
      auto &prev = Inputs[i];
      auto next = fn(prev);
      Inputs[i] = next;
      if (prev.getIsPrimary() && !prev.getFile().empty())
        PrimaryFiles.erase(prev.getFile());
      if (next.getIsPrimary() && !next.getFile().empty())
        PrimaryFiles.insert(std::make_pair(next.getFile(), i));
    }
  }

  // Input filename readers
  std::vector<std::string> getInputFilenames() const {
    std::vector<std::string> filenames;
    for (auto &input : getInputs()) {
      if (!input.getFile().empty())
        filenames.push_back(input.getFile());
    }
    return filenames;
  }

  unsigned inputCount() const { return getInputs().size(); }

  bool haveInputFilenames() const {
    for (const auto &input : getInputs())
      if (!input.getFile().empty())
        return true;
    return false;
  }
  unsigned inputFilenameCount() const {
    unsigned N = 0;
    for (const auto &input : getInputs())
      if (!input.getFile().empty())
        ++N;
    return N;
  }

  bool haveUniqueInputFilename() const { return inputFilenameCount() == 1; }
  const StringRef getFilenameOfFirstInput() const {
    assert(haveInputFilenames());
    const InputFileOrBuffer &inp = getInputs()[0];
    StringRef f = inp.getFile();
    assert(!f.empty());
    return f;
  }

  void bePrimaryAt(unsigned index) {
    if (Inputs[index].getIsPrimary())
      return;
    Inputs[index] = Inputs[index].asPrimary();
    if (!Inputs[index].getFile().empty())
      PrimaryFiles.insert(std::make_pair(Inputs[index].getFile(), index));
  }

  bool isReadingFromStdin() const {
    return haveUniqueInputFilename() && getFilenameOfFirstInput() == "-";
  }

  // If we have exactly one input filename, and its extension is "bc" or "ll",
  // treat the input as LLVM_IR.
  bool shouldTreatAsLLVM() const;

  // Primary input readers

private:
  void assertMustNotBeMoreThanOnePrimaryInput() const {
    assert(primaryInputCount() < 2 &&
           "have not implemented >1 primary input yet");
  }

public:
  unsigned primaryInputCount() const {
    unsigned N = 0;
    for (const InputFileOrBuffer &inp : getInputs()) {
      if (inp.getIsPrimary())
        ++N;
    }
    return N;
  }

  // Primary count readers:

  bool haveUniquePrimaryInput() const { return primaryInputCount() == 1; }

  bool havePrimaryInputs() const { return primaryInputCount() > 0; }

  bool isWholeModule() const { return !havePrimaryInputs(); }

  // Count-dependend readers:

  const InputFileOrBuffer *getOptionalUniquePrimaryInput() const {
    assertMustNotBeMoreThanOnePrimaryInput();
    for (const auto &input : getInputs())
      if (input.getIsPrimary())
        return &input;
    return nullptr;
  }

  const InputFileOrBuffer &getRequiredUniquePrimaryInput() const {
    if (const auto *input = getOptionalUniquePrimaryInput())
      return *input;
    assert(false);
  }

  StringRef getRequiredUniquePrimaryInputFilename() const {
    const InputFileOrBuffer &input = getRequiredUniquePrimaryInput();
    assert(!input.getFile().empty());
    return input.getFile();
  }

  bool haveAPrimaryInputFile() const {
    const auto *input = getOptionalUniquePrimaryInput();
    return input != nullptr && !input->getFile().empty();
  }

  StringRef getOptionalUniquePrimaryInputFilename() const {
    const auto *input = getOptionalUniquePrimaryInput();
    return input == nullptr ? StringRef() : input->getFile();
  }

public:
  // Multi-facet readers
  bool shouldTreatAsSIL() const;

  /// Return true for error
  bool verifyInputs(DiagnosticEngine &Diags, bool TreatAsSIL,
                    bool isREPLRequested, bool isNoneRequested) const;

  void addInputFile(StringRef file, llvm::MemoryBuffer *buffer = nullptr) {
    addInput(InputFileOrBuffer::createFile(file, false, buffer));
  }
  void addPrimaryInputFile(StringRef file,
                           llvm::MemoryBuffer *buffer = nullptr) {
    addInput(InputFileOrBuffer::createFile(file.str(), true, buffer));
  }
  void addInputBuffer(llvm::MemoryBuffer *buffer) {
    addInput(InputFileOrBuffer::createBuffer(buffer, false));
  }

  bool isFilePrimary(StringRef file) {
    return PrimaryFiles.find(file) != PrimaryFiles.end();
  }

  void setBuffer(llvm::MemoryBuffer *buffer, unsigned index) {
    Inputs[index].setBuffer(buffer);
  }
  void addPrimaryInputBuffer(llvm::MemoryBuffer *buffer) {
    addInput(InputFileOrBuffer::createBuffer(buffer, true));
  }
  void addInput(const InputFileOrBuffer &input) {
    if (!input.getFile().empty() && input.getIsPrimary())
      PrimaryFiles.insert(std::make_pair(input.getFile(), Inputs.size()));
    Inputs.push_back(input);
  }
  void clearInputs() {
    Inputs.clear();
    PrimaryFiles.clear();
  }
};

/// Options for controlling the behavior of the frontend.
class FrontendOptions {
  friend class FrontendArgsToOptionsConverter;

public:
  FrontendInputs Inputs;

  /// The kind of input on which the frontend should operate.
  InputFileKind InputKind = InputFileKind::IFK_Swift;

  /// The specified output files. If only a single outputfile is generated,
  /// the name of the last specified file is taken.
  std::vector<std::string> OutputFilenames;

  void forAllOutputPaths(std::function<void(const std::string &)> fn) const;

  /// Gets the name of the specified output filename.
  /// If multiple files are specified, the last one is returned.
  StringRef getSingleOutputFilename() const {
    if (OutputFilenames.size() >= 1)
      return OutputFilenames.back();
    return StringRef();
  }
  /// Sets a single filename as output filename.
  void setSingleOutputFilename(const std::string &FileName) {
    OutputFilenames.clear();
    OutputFilenames.push_back(FileName);
  }
  void setOutputFilenameToStdout() { setSingleOutputFilename("-"); }
  bool isOutputFilenameStdout() const {
    return getSingleOutputFilename() == "-";
  }
  bool isOutputFileDirectory() const;
  bool haveNamedOutputFile() const {
    return !OutputFilenames.empty() && !isOutputFilenameStdout();
  }

  /// A list of arbitrary modules to import and make implicitly visible.
  std::vector<std::string> ImplicitImportModuleNames;

  /// An Objective-C header to import and make implicitly visible.
  std::string ImplicitObjCHeaderPath;

  /// The name of the module which the frontend is building.
  std::string ModuleName;

  /// The path to which we should emit a serialized module.
  std::string ModuleOutputPath;

  /// The path to which we should emit a module documentation file.
  std::string ModuleDocOutputPath;

  /// The name of the library to link against when using this module.
  std::string ModuleLinkName;

  /// The path to which we should emit an Objective-C header for the module.
  std::string ObjCHeaderOutputPath;

  /// Path to a file which should contain serialized diagnostics for this
  /// frontend invocation.
  std::string SerializedDiagnosticsPath;

  /// The path to which we should output a Make-style dependencies file.
  std::string DependenciesFilePath;

  /// The path to which we should output a Swift reference dependencies file.
  std::string ReferenceDependenciesFilePath;

  /// The path to which we should output fixits as source edits.
  std::string FixitsOutputPath;

  /// The path to which we should output a loaded module trace file.
  std::string LoadedModuleTracePath;

  /// The path to which we should output a TBD file.
  std::string TBDPath;

  /// Arguments which should be passed in immediate mode.
  std::vector<std::string> ImmediateArgv;

  /// \brief A list of arguments to forward to LLVM's option processing; this
  /// should only be used for debugging and experimental features.
  std::vector<std::string> LLVMArgs;

  /// The path to output swift interface files for the compiled source files.
  std::string DumpAPIPath;

  /// The path to collect the group information for the compiled source files.
  std::string GroupInfoPath;

  /// The path to which we should store indexing data, if any.
  std::string IndexStorePath;

  /// Emit index data for imported serialized swift system modules.
  bool IndexSystemModules = false;

  /// If non-zero, warn when a function body takes longer than this many
  /// milliseconds to type-check.
  ///
  /// Intended for debugging purposes only.
  unsigned WarnLongFunctionBodies = 0;

  /// If non-zero, warn when type-checking an expression takes longer
  /// than this many milliseconds.
  ///
  /// Intended for debugging purposes only.
  unsigned WarnLongExpressionTypeChecking = 0;

  /// If non-zero, overrides the default threshold for how long we let
  /// the expression type checker run before we consider an expression
  /// too complex.
  unsigned SolverExpressionTimeThreshold = 0;

  /// The module for which we should verify all of the generic signatures.
  std::string VerifyGenericSignaturesInModule;

  enum class ActionType {
    NoneAction,        ///< No specific action
    Parse,             ///< Parse only
    Typecheck,         ///< Parse and type-check only
    DumpParse,         ///< Parse only and dump AST
    DumpInterfaceHash, ///< Parse and dump the interface token hash.
    EmitSyntax,        ///< Parse and dump Syntax tree as JSON
    DumpAST,           ///< Parse, type-check, and dump AST
    PrintAST,          ///< Parse, type-check, and pretty-print AST

    /// Parse and dump scope map.
    DumpScopeMaps,

    /// Parse, type-check, and dump type refinement context hierarchy
    DumpTypeRefinementContexts,

    EmitImportedModules, ///< Emit the modules that this one imports
    EmitPCH,             ///< Emit PCH of imported bridging header

    EmitSILGen, ///< Emit raw SIL
    EmitSIL,    ///< Emit canonical SIL

    EmitModuleOnly, ///< Emit module only
    MergeModules,   ///< Merge modules only

    EmitSIBGen, ///< Emit serialized AST + raw SIL
    EmitSIB,    ///< Emit serialized AST + canonical SIL

    Immediate, ///< Immediate mode
    REPL,      ///< REPL mode

    EmitAssembly, ///< Emit assembly
    EmitIR,       ///< Emit LLVM IR
    EmitBC,       ///< Emit LLVM BC
    EmitObject,   ///< Emit object file
  };

  bool isCreatingSIL() { return RequestedAction >= ActionType::EmitSILGen; }

  /// Indicates the action the user requested that the frontend perform.
  ActionType RequestedAction = ActionType::NoneAction;

  /// Indicates that the input(s) should be parsed as the Swift stdlib.
  bool ParseStdlib = false;

  /// If set, emitted module files will always contain options for the
  /// debugger to use.
  bool AlwaysSerializeDebuggingOptions = false;

  /// If set, dumps wall time taken to check each function body to llvm::errs().
  bool DebugTimeFunctionBodies = false;

  /// If set, dumps wall time taken to check each expression.
  bool DebugTimeExpressionTypeChecking = false;

  /// If set, prints the time taken in each major compilation phase to 
  /// llvm::errs().
  ///
  /// \sa swift::SharedTimer
  bool DebugTimeCompilation = false;

  /// The path to which we should output statistics files.
  std::string StatsOutputDir;

  /// Trace changes to stats to files in StatsOutputDir.
  bool TraceStats = false;

  /// Indicates whether function body parsing should be delayed
  /// until the end of all files.
  bool DelayedFunctionBodyParsing = false;

  /// If true, serialization encodes an extra lookup table for use in module-
  /// merging when emitting partial modules (the per-file modules in a non-WMO
  /// build).
  bool EnableSerializationNestedTypeLookupTable = true;

  /// Indicates whether or not an import statement can pick up a Swift source
  /// file (as opposed to a module file).
  bool EnableSourceImport = false;

  /// Indicates whether we are compiling for testing.
  ///
  /// \see ModuleDecl::isTestingEnabled
  bool EnableTesting = false;

  /// Enables the "fully resilient" resilience strategy.
  ///
  /// \see ResilienceStrategy::Resilient
  bool EnableResilience = false;

  /// Indicates that the frontend should emit "verbose" SIL
  /// (if asked to emit SIL).
  bool EmitVerboseSIL = false;

  /// If set, this module is part of a mixed Objective-C/Swift framework, and
  /// the Objective-C half should implicitly be visible to the Swift sources.
  bool ImportUnderlyingModule = false;

  /// If set, the header provided in ImplicitObjCHeaderPath will be rewritten
  /// by the Clang importer as part of semantic analysis.
  bool SerializeBridgingHeader = false;

  /// Indicates whether or not the frontend should print statistics upon
  /// termination.
  bool PrintStats = false;

  /// Indicates whether or not the Clang importer should print statistics upon
  /// termination.
  bool PrintClangStats = false;

  /// Indicates whether the playground transformation should be applied.
  bool PlaygroundTransform = false;
  
  /// Indicates whether the AST should be instrumented to simulate a debugger's
  /// program counter. Similar to the PlaygroundTransform, this will instrument
  /// the AST with function calls that get called when you would see a program
  /// counter move in a debugger. To adopt this implement the
  /// __builtin_pc_before and __builtin_pc_after functions.
  bool PCMacro = false;

  /// Indicates whether the playground transformation should omit
  /// instrumentation that has a high runtime performance impact.
  bool PlaygroundHighPerformance = false;

  /// Indicates whether standard help should be shown.
  bool PrintHelp = false;

  /// Indicates whether full help (including "hidden" options) should be shown.
  bool PrintHelpHidden = false;

  /// Should we sort SIL functions, vtables, witness tables, and global
  /// variables by name when we print it out. This eases diffing of SIL files.
  bool EmitSortedSIL = false;

  /// The different modes for validating TBD against the LLVM IR.
  enum class TBDValidationMode {
    None,           ///< Do no validation.
    MissingFromTBD, ///< Only check for symbols that are in IR but not TBD.
    All, ///< Check for symbols that are in IR but not TBD and TBD but not IR.
  };

  /// Compare the symbols in the IR against the TBD file we would generate.
  TBDValidationMode ValidateTBDAgainstIR = TBDValidationMode::None;

  /// The install_name to use in the TBD file.
  std::string TBDInstallName;

  /// An enum with different modes for automatically crashing at defined times.
  enum class DebugCrashMode {
    None, ///< Don't automatically crash.
    AssertAfterParse, ///< Automatically assert after parsing.
    CrashAfterParse, ///< Automatically crash after parsing.
  };

  /// Indicates a debug crash mode for the frontend.
  DebugCrashMode CrashMode = DebugCrashMode::None;

  /// Line and column for each of the locations to be probed by
  /// -dump-scope-maps.
  SmallVector<std::pair<unsigned, unsigned>, 2> DumpScopeMapLocations;

  /// Indicates whether the RequestedAction has output.
  bool actionHasOutput() const;

  /// Indicates whether the RequestedAction will immediately run code.
  bool actionIsImmediate() const;

  /// Return a hash code of any components from these options that should
  /// contribute to a Swift Bridging PCH hash.
  llvm::hash_code getPCHHashComponents() const {
    return llvm::hash_value(0);
  }

  StringRef originalPath() const;

  StringRef determineFallbackModuleName() const;

  bool isCompilingExactlyOneSwiftFile() const {
    return InputKind == InputFileKind::IFK_Swift &&
           Inputs.haveUniqueInputFilename();
  }

private:
  static const char *suffixForPrincipalOutputFileForAction(ActionType);

  bool hasUnusedDependenciesFilePath() const;
  static bool canActionEmitDependencies(ActionType);
  bool hasUnusedObjCHeaderOutputPath() const;
  static bool canActionEmitHeader(ActionType);
  bool hasUnusedLoadedModuleTracePath() const;
  static bool canActionEmitLoadedModuleTrace(ActionType);
  bool hasUnusedModuleOutputPath() const;
  static bool canActionEmitModule(ActionType);
  bool hasUnusedModuleDocOutputPath() const;
  static bool canActionEmitModuleDoc(ActionType);

  static bool doesActionProduceOutput(ActionType);
  static bool doesActionProduceTextualOutput(ActionType);
};

}

#endif

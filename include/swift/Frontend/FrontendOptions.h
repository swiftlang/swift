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
  namespace opt {
  class ArgList;
  class Arg;
  } // namespace opt
}

namespace swift {

class SelectedInput {
public:
  /// The index of the input, in either FrontendOptions::InputFilenames or
  /// FrontendOptions::InputBuffers, depending on this SelectedInput's
  /// InputKind.
  unsigned Index;

  enum class InputKind {
    /// Denotes a file input, in FrontendOptions::InputFilenames
    Filename,

    /// Denotes a buffer input, in FrontendOptions::InputBuffers
    Buffer,
  };

  /// The kind of input which this SelectedInput represents.
  InputKind Kind;

  SelectedInput(unsigned Index, InputKind Kind = InputKind::Filename)
      : Index(Index), Kind(Kind) {}

  /// \returns true if the SelectedInput's Kind is a filename
  bool isFilename() const { return Kind == InputKind::Filename; }

  /// \returns true if the SelectedInput's Kind is a buffer
  bool isBuffer() const { return Kind == InputKind::Buffer; }
};

enum class InputFileKind {
  IFK_None,
  IFK_Swift,
  IFK_Swift_Library,
  IFK_Swift_REPL,
  IFK_SIL,
  IFK_LLVM_IR
};

/// Information about all the inputs to the frontend.
class FrontendInputs {
  friend class ArgsToFrontendInputsConverter;

private:
  /// The names of input files to the frontend.
  std::vector<std::string> InputFilenames;

  /// Input buffers which may override the file contents of input files.
  std::vector<llvm::MemoryBuffer *> InputBuffers;

  /// The inputs for which output should be generated. If empty, output will
  /// be generated for the whole module, in other words, whole-module mode.
  /// Even if every input file is mentioned here, it is not the same as
  /// whole-module mode.
  std::vector<SelectedInput> PrimaryInputs;

public:
  // Readers:

  // Input filename readers
  ArrayRef<std::string> getInputFilenames() const { return InputFilenames; }
  bool haveInputFilenames() const { return !getInputFilenames().empty(); }
  unsigned inputFilenameCount() const { return getInputFilenames().size(); }

  bool haveUniqueInputFilename() const { return inputFilenameCount() == 1; }
  const std::string &getFilenameOfFirstInput() const {
    assert(haveInputFilenames());
    return getInputFilenames()[0];
  }

  bool isReadingFromStdin() const {
    return haveUniqueInputFilename() && getFilenameOfFirstInput() == "-";
  }

  // If we have exactly one input filename, and its extension is "bc" or "ll",
  // treat the input as LLVM_IR.
  bool shouldTreatAsLLVM() const;

  // Input buffer readers

  ArrayRef<llvm::MemoryBuffer *> getInputBuffers() const {
    return InputBuffers;
  }
  unsigned inputBufferCount() const { return getInputBuffers().size(); }

  // Primary input readers

private:
  void assertMustNotBeMoreThanOnePrimaryInput() const {
    assert(PrimaryInputs.size() < 2 &&
           "have not implemented >1 primary input yet");
  }

public:
  ArrayRef<SelectedInput> getPrimaryInputs() const { return PrimaryInputs; }

  unsigned primaryInputCount() const { return getPrimaryInputs().size(); }

  // Primary count readers:

  bool haveUniquePrimaryInput() const { return primaryInputCount() == 1; }

  bool havePrimaryInputs() const { return primaryInputCount() > 0; }

  bool isWholeModule() const { return !havePrimaryInputs(); }

  // Count-dependend readers:

  Optional<SelectedInput> getOptionalPrimaryInput() const {
    return havePrimaryInputs() ? Optional<SelectedInput>(getPrimaryInputs()[0])
                               : Optional<SelectedInput>();
  }

  SelectedInput getRequiredUniquePrimaryInput() const {
    assert(haveUniquePrimaryInput());
    return getPrimaryInputs()[0];
  }

  Optional<SelectedInput> getOptionalUniquePrimaryInput() const {
    return haveUniquePrimaryInput()
               ? Optional<SelectedInput>(getPrimaryInputs()[0])
               : Optional<SelectedInput>();
  }

  bool haveAPrimaryInputFile() const {
    return havePrimaryInputs() && getOptionalPrimaryInput()->isFilename();
  }

  Optional<StringRef> getOptionalUniquePrimaryInputFilename() const {
    Optional<SelectedInput> primaryInput = getOptionalUniquePrimaryInput();
    return (primaryInput && primaryInput->isFilename())
               ? Optional<StringRef>(getInputFilenames()[primaryInput->Index])
               : Optional<StringRef>();
  }

  bool isPrimaryInputAFileAt(unsigned i) const {
    assertMustNotBeMoreThanOnePrimaryInput();
    if (Optional<SelectedInput> primaryInput = getOptionalPrimaryInput())
      return primaryInput->isFilename() && primaryInput->Index == i;
    return false;
  }

  Optional<unsigned> primaryInputFileIndex() const {
    return haveAPrimaryInputFile()
               ? Optional<unsigned>(getOptionalPrimaryInput()->Index)
               : None;
  }

  StringRef primaryInputFilenameIfAny() const {
    if (auto Index = primaryInputFileIndex()) {
      return getInputFilenames()[*Index];
    }
    return StringRef();
  }

public:
  // Multi-facet readers
  StringRef baseNameOfOutput(const llvm::opt::ArgList &Args,
                             StringRef ModuleName) const;

  bool shouldTreatAsSIL() const;

  /// Return true for error
  bool verifyInputs(DiagnosticEngine &Diags, bool TreatAsSIL,
                    bool isREPLRequested, bool isNoneRequested) const;

  // Input filename writers

  void addInputFilename(StringRef Filename) {
    InputFilenames.push_back(Filename);
  }
  void transformInputFilenames(
      const llvm::function_ref<std::string(std::string)> &fn);

  // Input buffer writers

  void addInputBuffer(llvm::MemoryBuffer *Buf) { InputBuffers.push_back(Buf); }

  // Primary input writers

private:
  std::vector<SelectedInput> &getMutablePrimaryInputs() {
    assertMustNotBeMoreThanOnePrimaryInput();
    return PrimaryInputs;
  }

public:
  void clearPrimaryInputs() { getMutablePrimaryInputs().clear(); }

  void setPrimaryInputToFirstFile() {
    clearPrimaryInputs();
    addPrimaryInput(SelectedInput(0, SelectedInput::InputKind::Filename));
  }

  void addPrimaryInput(SelectedInput si) { PrimaryInputs.push_back(si); }

  void setPrimaryInput(SelectedInput si) {
    clearPrimaryInputs();
    getMutablePrimaryInputs().push_back(si);
  }

  void addPrimaryInputFilename(unsigned index) {
    addPrimaryInput(SelectedInput(index, SelectedInput::InputKind::Filename));
  }

  void setPrimaryInputForInputFilename(const std::string &inputFilename) {
    setPrimaryInput(!inputFilename.empty() && inputFilename != "-"
                        ? SelectedInput(inputFilenameCount(),
                                        SelectedInput::InputKind::Filename)
                        : SelectedInput(inputBufferCount(),
                                        SelectedInput::InputKind::Buffer));
  }

  // Multi-faceted writers

  void clearInputs() {
    InputFilenames.clear();
    InputBuffers.clear();
  }
};

/// Options for controlling the behavior of the frontend.
class FrontendOptions {
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
  bool isOutputFilePlainFile() const;
  bool haveNamedOutputFile() const {
    return !OutputFilenames.empty() && !isOutputFilenameStdout();
  }
  void setOutputFileList(DiagnosticEngine &Diags,
                         const llvm::opt::ArgList &Args);

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

  void setModuleName(DiagnosticEngine &Diags, const llvm::opt::ArgList &Args);
};

}

#endif

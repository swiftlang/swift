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

#include "swift/Basic/FileTypes.h"
#include "swift/Basic/Version.h"
#include "swift/Frontend/FrontendInputsAndOutputs.h"
#include "swift/Frontend/InputFile.h"
#include "llvm/ADT/Hashing.h"

#include <string>
#include <vector>

namespace llvm {
  class MemoryBuffer;
}

namespace swift {


/// Options for controlling the behavior of the frontend.
class FrontendOptions {
  friend class ArgsToFrontendOptionsConverter;

public:
  FrontendInputsAndOutputs InputsAndOutputs;

  /// The kind of input on which the frontend should operate.
  InputFileKind InputKind = InputFileKind::Swift;

  void forAllOutputPaths(const InputFile &input,
                         llvm::function_ref<void(StringRef)> fn) const;

  bool isOutputFileDirectory() const;

  /// A list of arbitrary modules to import and make implicitly visible.
  std::vector<std::string> ImplicitImportModuleNames;

  /// An Objective-C header to import and make implicitly visible.
  std::string ImplicitObjCHeaderPath;

  /// The name of the module which the frontend is building.
  std::string ModuleName;

  /// The name of the library to link against when using this module.
  std::string ModuleLinkName;

  /// Arguments which should be passed in immediate mode.
  std::vector<std::string> ImmediateArgv;

  /// A list of arguments to forward to LLVM's option processing; this
  /// should only be used for debugging and experimental features.
  std::vector<std::string> LLVMArgs;

  /// The path to output swift interface files for the compiled source files.
  std::string DumpAPIPath;

  /// The path to collect the group information for the compiled source files.
  std::string GroupInfoPath;

  /// The path to which we should store indexing data, if any.
  std::string IndexStorePath;

  /// The path to look in when loading a module interface file, to see if a
  /// binary module has already been built for use by the compiler.
  std::string PrebuiltModuleCachePath;

  /// For these modules, we should prefer using Swift interface when importing them.
  std::vector<std::string> PreferInterfaceForModules;

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
  
  /// If non-zero, overrides the default threshold for how many times
  /// the Space::minus function is called before we consider switch statement
  /// exhaustiveness checking to be too complex.
  unsigned SwitchCheckingInvocationThreshold = 0;

  /// The module for which we should verify all of the generic signatures.
  std::string VerifyGenericSignaturesInModule;

  enum class ActionType {
    NoneAction,        ///< No specific action
    Parse,             ///< Parse only
    ResolveImports,    ///< Parse and resolve imports only
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

    /// Build from a swiftinterface, as close to `import` as possible
    CompileModuleFromInterface,

    EmitSIBGen, ///< Emit serialized AST + raw SIL
    EmitSIB,    ///< Emit serialized AST + canonical SIL

    Immediate, ///< Immediate mode
    REPL,      ///< REPL mode

    EmitAssembly, ///< Emit assembly
    EmitIR,       ///< Emit LLVM IR
    EmitBC,       ///< Emit LLVM BC
    EmitObject,   ///< Emit object file

    DumpTypeInfo, ///< Dump IRGen type info
  };

  /// Indicates the action the user requested that the frontend perform.
  ActionType RequestedAction = ActionType::NoneAction;

  /// Indicates that the input(s) should be parsed as the Swift stdlib.
  bool ParseStdlib = false;

  /// When true, emitted module files will always contain options for the
  /// debugger to use. When unset, the options will only be present if the
  /// module appears to not be a public module.
  Optional<bool> SerializeOptionsForDebugging;

  /// When true, check if all required SwiftOnoneSupport symbols are present in
  /// the module.
  bool CheckOnoneSupportCompleteness = false;

  /// If set, inserts instrumentation useful for testing the debugger.
  bool DebuggerTestingTransform = false;

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

  /// Profile changes to stats to files in StatsOutputDir.
  bool ProfileEvents = false;

  /// Profile changes to stats to files in StatsOutputDir, grouped by source
  /// entity.
  bool ProfileEntities = false;

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

  /// Indicates whether we are compiling for private imports.
  ///
  /// \see ModuleDecl::arePrivateImportsEnabled
  bool EnablePrivateImports = false;


  /// Indicates whether we add implicit dynamic.
  ///
  /// \see ModuleDecl::isImplicitDynamicEnabled
  bool EnableImplicitDynamic = false;

  /// Enables the "fully resilient" resilience strategy.
  ///
  /// \see ResilienceStrategy::Resilient
  bool EnableLibraryEvolution = false;

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

  /// Indicates whether the dependency tracker should track system
  /// dependencies as well.
  bool TrackSystemDeps = false;

  /// Should we serialize the hashes of dependencies (vs. the modification
  /// times) when compiling a module interface?
  bool SerializeModuleInterfaceDependencyHashes = false;

  /// Should we warn if an imported module needed to be rebuilt from a
  /// module interface file?
  bool RemarkOnRebuildFromModuleInterface = false;

  /// The different modes for validating TBD against the LLVM IR.
  enum class TBDValidationMode {
    Default,        ///< Do the default validation for the current platform.
    None,           ///< Do no validation.
    MissingFromTBD, ///< Only check for symbols that are in IR but not TBD.
    All, ///< Check for symbols that are in IR but not TBD and TBD but not IR.
  };

  /// Compare the symbols in the IR against the TBD file we would generate.
  TBDValidationMode ValidateTBDAgainstIR = TBDValidationMode::Default;

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

  /// Indicates whether the action will immediately run code.
  static bool isActionImmediate(ActionType);

  /// \return true if action only parses without doing other compilation steps.
  static bool shouldActionOnlyParse(ActionType);

  /// Return a hash code of any components from these options that should
  /// contribute to a Swift Bridging PCH hash.
  llvm::hash_code getPCHHashComponents() const {
    return llvm::hash_value(0);
  }

  StringRef determineFallbackModuleName() const;

  bool isCompilingExactlyOneSwiftFile() const {
    return InputKind == InputFileKind::Swift &&
           InputsAndOutputs.hasSingleInput();
  }

  const PrimarySpecificPaths &
  getPrimarySpecificPathsForAtMostOnePrimary() const;
  const PrimarySpecificPaths &
      getPrimarySpecificPathsForPrimary(StringRef) const;

private:
  static bool canActionEmitDependencies(ActionType);
  static bool canActionEmitReferenceDependencies(ActionType);
  static bool canActionEmitObjCHeader(ActionType);
  static bool canActionEmitLoadedModuleTrace(ActionType);
  static bool canActionEmitModule(ActionType);
  static bool canActionEmitModuleDoc(ActionType);
  static bool canActionEmitInterface(ActionType);

public:
  static bool doesActionGenerateSIL(ActionType);
  static bool doesActionProduceOutput(ActionType);
  static bool doesActionProduceTextualOutput(ActionType);
  static bool needsProperModuleName(ActionType);
  static file_types::ID formatForPrincipalOutputFileForAction(ActionType);
};

}

#endif

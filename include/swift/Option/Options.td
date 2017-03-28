//===--- Options.td - Options for swift driver ----------------------------===//
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
//
//  This file defines the options accepted by the swift driver.
//
//===----------------------------------------------------------------------===//

// Include the common option parsing interfaces.
include "llvm/Option/OptParser.td"

/////////
// Flags

// The option should be accepted by swift -frontend.
def FrontendOption : OptionFlag;

// The option should be accepted by swift-autolink-extract
def AutolinkExtractOption : OptionFlag;

// The option should be accepted by swift -modulewrap
def ModuleWrapOption : OptionFlag;

// The option should be accepted by swift-format
def SwiftFormatOption : OptionFlag;

// The option should not be accepted by the driver.
def NoDriverOption : OptionFlag;

// Some options should not be available depending on whether this is the
// interactive driver 'swift', or the batch compiler 'swiftc'.
def NoInteractiveOption : OptionFlag;
def NoBatchOption : OptionFlag;

// The option should not force a full rebuild if added, changed, or removed.
def DoesNotAffectIncrementalBuild : OptionFlag;

/////////
// Options

// The internal option ID must be a valid C++ identifier and results in a
// swift::driver::options::OPT_XX enum constant for XX.
//
// We want to unambiguously be able to refer to options from the driver source
// code, for this reason the option name is mangled into an ID. This mangling
// isn't guaranteed to have an inverse, but for practical purposes it does.
//
// The mangling scheme is to ignore the leading '-', and perform the following
// substitutions:
//   _ => __
//   - => _
//   / => _SLASH
//   # => _HASH
//   ? => _QUESTION
//   , => _COMMA
//   = => _EQ
//   C++ => CXX
//   . => _

def internal_Group : OptionGroup<"<swift internal options>">;
def internal_debug_Group :
  OptionGroup<"<swift debug/development internal options>">,
  Group<internal_Group>, HelpText<"DEBUG/DEVELOPMENT OPTIONS">;

class InternalDebugOpt : Group<internal_debug_Group>,
  Flags<[HelpHidden, DoesNotAffectIncrementalBuild]>;

def driver_print_actions : Flag<["-"], "driver-print-actions">,
  InternalDebugOpt, HelpText<"Dump list of actions to perform">;
def driver_print_output_file_map : Flag<["-"], "driver-print-output-file-map">,
  InternalDebugOpt, HelpText<"Dump the contents of the output file map">;
def driver_print_bindings : Flag<["-"], "driver-print-bindings">,
  InternalDebugOpt, HelpText<"Dump list of job inputs and outputs">;
def driver_print_jobs : Flag<["-"], "driver-print-jobs">, InternalDebugOpt,
  HelpText<"Dump list of jobs to execute">;
def _HASH_HASH_HASH : Flag<["-"], "###">, Alias<driver_print_jobs>;
def driver_skip_execution : Flag<["-"], "driver-skip-execution">,
  InternalDebugOpt,
  HelpText<"Skip execution of subtasks when performing compilation">;
def driver_use_frontend_path : Separate<["-"], "driver-use-frontend-path">,
  InternalDebugOpt,
  HelpText<"Use the given executable to perform compilations">;
def driver_show_incremental : Flag<["-"], "driver-show-incremental">,
  InternalDebugOpt,
  HelpText<"With -v, dump information about why files are being rebuilt">;
def driver_show_job_lifecycle : Flag<["-"], "driver-show-job-lifecycle">,
  InternalDebugOpt,
  HelpText<"Show every step in the lifecycle of driver jobs">;
def driver_use_filelists : Flag<["-"], "driver-use-filelists">,
  InternalDebugOpt, HelpText<"Pass input files as filelists whenever possible">;

def driver_always_rebuild_dependents :
  Flag<["-"], "driver-always-rebuild-dependents">, InternalDebugOpt,
  HelpText<"Always rebuild dependents of files that have been modified">;

def driver_mode : Joined<["--"], "driver-mode=">, Flags<[HelpHidden]>,
  HelpText<"Set the driver mode to either 'swift' or 'swiftc'">;

def help : Flag<["-", "--"], "help">,
  Flags<[FrontendOption, AutolinkExtractOption, ModuleWrapOption, SwiftFormatOption]>,
  HelpText<"Display available options">;
def h : Flag<["-"], "h">, Alias<help>;
def help_hidden : Flag<["-", "--"], "help-hidden">,
  Flags<[HelpHidden, FrontendOption]>,
  HelpText<"Display available options, including hidden options">;

def v : Flag<["-"], "v">, Flags<[DoesNotAffectIncrementalBuild]>,
  HelpText<"Show commands to run and use verbose output">;
def version : Flag<["-", "--"], "version">,
  HelpText<"Print version information and exit">;

def parseable_output : Flag<["-"], "parseable-output">,
  Flags<[NoInteractiveOption, DoesNotAffectIncrementalBuild]>,
  HelpText<"Emit textual output in a parseable format">;

// Standard Options
def _DASH_DASH : Option<["--"], "", KIND_REMAINING_ARGS>,
  Flags<[FrontendOption, DoesNotAffectIncrementalBuild]>;

def o : JoinedOrSeparate<["-"], "o">,
  Flags<[FrontendOption, AutolinkExtractOption, ModuleWrapOption,
         NoInteractiveOption, SwiftFormatOption]>,
  HelpText<"Write output to <file>">, MetaVarName<"<file>">;

def j : JoinedOrSeparate<["-"], "j">, Flags<[DoesNotAffectIncrementalBuild]>,
  HelpText<"Number of commands to execute in parallel">, MetaVarName<"<n>">;

def sdk : Separate<["-"], "sdk">, Flags<[FrontendOption]>,
  HelpText<"Compile against <sdk>">, MetaVarName<"<sdk>">;

def swift_version : Separate<["-"], "swift-version">, Flags<[FrontendOption]>,
  HelpText<"Interpret input according to a specific Swift language version number">,
  MetaVarName<"<vers>">;

def tools_directory : Separate<["-"], "tools-directory">,
  Flags<[FrontendOption, NoInteractiveOption, DoesNotAffectIncrementalBuild]>,
  HelpText<"Look for external executables (ld, clang, binutils) in <directory>">, MetaVarName<"<directory>">;

def D : JoinedOrSeparate<["-"], "D">, Flags<[FrontendOption]>,
  HelpText<"Marks a conditional compilation flag as true">;

def F : JoinedOrSeparate<["-"], "F">, Flags<[FrontendOption]>,
  HelpText<"Add directory to framework search path">;
def F_EQ : Joined<["-"], "F=">, Flags<[FrontendOption]>, Alias<F>;

def Fsystem : Separate<["-"], "Fsystem">, Flags<[FrontendOption]>,
  HelpText<"Add directory to system framework search path">;

def I : JoinedOrSeparate<["-"], "I">, Flags<[FrontendOption]>,
  HelpText<"Add directory to the import search path">;
def I_EQ : Joined<["-"], "I=">, Flags<[FrontendOption]>, Alias<I>;

def import_underlying_module : Flag<["-"], "import-underlying-module">,
  Flags<[FrontendOption, NoInteractiveOption]>,
  HelpText<"Implicitly imports the Objective-C half of a module">;

def import_objc_header : Separate<["-"], "import-objc-header">,
  Flags<[FrontendOption, HelpHidden]>,
  HelpText<"Implicitly imports an Objective-C header file">;

def pch_output_dir: Separate<["-"], "pch-output-dir">,
  Flags<[FrontendOption, HelpHidden]>,
  HelpText<"Directory to persist automatically created precompiled bridging headers">;

// FIXME: Unhide this once it doesn't depend on an output file map.
def incremental : Flag<["-"], "incremental">,
  Flags<[NoInteractiveOption, HelpHidden, DoesNotAffectIncrementalBuild]>,
  HelpText<"Perform an incremental build if possible">;

def nostdimport : Flag<["-"], "nostdimport">, Flags<[FrontendOption]>,
  HelpText<"Don't search the standard library import path for modules">;

def output_file_map : Separate<["-"], "output-file-map">,
  Flags<[NoInteractiveOption]>,
  HelpText<"A file which specifies the location of outputs">,
  MetaVarName<"<path>">;
def output_file_map_EQ : Joined<["-"], "output-file-map=">,
  Alias<output_file_map>;

def save_temps : Flag<["-"], "save-temps">,
  Flags<[NoInteractiveOption,DoesNotAffectIncrementalBuild]>,
  HelpText<"Save intermediate compilation results">;
def driver_time_compilation : Flag<["-"], "driver-time-compilation">,
  Flags<[NoInteractiveOption,DoesNotAffectIncrementalBuild]>,
  HelpText<"Prints the total time it took to execute all compilation tasks">;

def emit_dependencies : Flag<["-"], "emit-dependencies">,
  Flags<[FrontendOption, NoInteractiveOption, DoesNotAffectIncrementalBuild]>,
  HelpText<"Emit basic Make-compatible dependencies files">;

def serialize_diagnostics : Flag<["-"], "serialize-diagnostics">,
  Flags<[FrontendOption, NoInteractiveOption, DoesNotAffectIncrementalBuild]>,
  HelpText<"Serialize diagnostics in a binary format">;

def module_cache_path : Separate<["-"], "module-cache-path">,
  Flags<[FrontendOption, DoesNotAffectIncrementalBuild]>,
  HelpText<"Specifies the Clang module cache path">;

def module_name : Separate<["-"], "module-name">, Flags<[FrontendOption]>,
  HelpText<"Name of the module to build">;
def module_name_EQ : Joined<["-"], "module-name=">, Flags<[FrontendOption]>,
  Alias<module_name>;

def module_link_name : Separate<["-"], "module-link-name">,
  Flags<[FrontendOption]>,
  HelpText<"Library to link against when using this module">;
def module_link_name_EQ : Joined<["-"], "module-link-name=">,
  Flags<[FrontendOption]>, Alias<module_link_name>;
def autolink_force_load : Flag<["-"], "autolink-force-load">,
  Flags<[FrontendOption, HelpHidden]>,
  HelpText<"Force ld to link against this module even if no symbols are used">;

def emit_module : Flag<["-"], "emit-module">,
  Flags<[FrontendOption, NoInteractiveOption, DoesNotAffectIncrementalBuild]>,
  HelpText<"Emit an importable module">;
def emit_module_path : Separate<["-"], "emit-module-path">,
  Flags<[FrontendOption, NoInteractiveOption, DoesNotAffectIncrementalBuild]>,
  HelpText<"Emit an importable module to <path>">,
  MetaVarName<"<path>">;
def emit_module_path_EQ : Joined<["-"], "emit-module-path=">,
  Flags<[FrontendOption, NoInteractiveOption]>, Alias<emit_module_path>;

def emit_objc_header : Flag<["-"], "emit-objc-header">,
  Flags<[FrontendOption, NoInteractiveOption, DoesNotAffectIncrementalBuild]>,
  HelpText<"Emit an Objective-C header file">;
def emit_objc_header_path : Separate<["-"], "emit-objc-header-path">,
  Flags<[FrontendOption, NoInteractiveOption, DoesNotAffectIncrementalBuild]>,
  MetaVarName<"<path>">, HelpText<"Emit an Objective-C header file to <path>">;

def import_cf_types : Flag<["-"], "import-cf-types">,
  Flags<[FrontendOption, HelpHidden]>,
  HelpText<"Recognize and import CF types as class types">;

def solver_memory_threshold : Separate<["-"], "solver-memory-threshold">,
  Flags<[FrontendOption, HelpHidden, DoesNotAffectIncrementalBuild]>,
  HelpText<"Set the upper bound for memory consumption, in bytes, by the constraint solver">;

def disable_swift_bridge_attr : Flag<["-"], "disable-swift-bridge-attr">,
  Flags<[FrontendOption, HelpHidden]>,
  HelpText<"Disable using the swift bridge attribute">;

def enable_swift3_objc_inference : Flag<["-"], "enable-swift3-objc-inference">,
  Flags<[FrontendOption, HelpHidden]>,
HelpText<"Enable Swift 3's @objc inference rules for NSObject-derived classes and 'dynamic' members (emulates Swift 3 behavior)">;

def disable_swift3_objc_inference :
  Flag<["-"], "disable-swift3-objc-inference">,
  Flags<[FrontendOption, HelpHidden]>,
  HelpText<"Disable Swift 3's @objc inference rules for NSObject-derived classes and 'dynamic' members (emulates Swift 4 behavior)">;

def enable_bridging_pch : Flag<["-"], "enable-bridging-pch">,
  Flags<[HelpHidden]>,
  HelpText<"Enable automatic generation of bridging PCH files">;

def disable_bridging_pch : Flag<["-"], "disable-bridging-pch">,
  Flags<[HelpHidden]>,
  HelpText<"Disable automatic generation of bridging PCH files">;

// Diagnostic control options
def suppress_warnings : Flag<["-"], "suppress-warnings">,
  Flags<[FrontendOption]>,
  HelpText<"Suppress all warnings">;

def warnings_as_errors : Flag<["-"], "warnings-as-errors">,
  Flags<[FrontendOption]>,
  HelpText<"Treat warnings as errors">;

def continue_building_after_errors : Flag<["-"], "continue-building-after-errors">,
  Flags<[FrontendOption, DoesNotAffectIncrementalBuild]>,
  HelpText<"Continue building, even after errors are encountered">;

// Platform options.
def enable_app_extension : Flag<["-"], "application-extension">,
  Flags<[FrontendOption, NoInteractiveOption]>,
  HelpText<"Restrict code to those available for App Extensions">;

// Linker options

def linker_option_Group : OptionGroup<"<linker-specific options>">;

def l : Joined<["-"], "l">, Group<linker_option_Group>,
    Flags<[FrontendOption, DoesNotAffectIncrementalBuild]>,
    HelpText<"Specifies a library which should be linked against">;
def framework : Separate<["-"], "framework">, Group<linker_option_Group>,
  Flags<[FrontendOption, DoesNotAffectIncrementalBuild]>,
  HelpText<"Specifies a framework which should be linked against">;

def L : JoinedOrSeparate<["-"], "L">, Group<linker_option_Group>,
  Flags<[FrontendOption, DoesNotAffectIncrementalBuild]>,
  HelpText<"Add directory to library link search path">;
def L_EQ : Joined<["-"], "L=">, Alias<L>;

def link_objc_runtime : Flag<["-"], "link-objc-runtime">,
  Flags<[DoesNotAffectIncrementalBuild]>;
def no_link_objc_runtime : Flag<["-"], "no-link-objc-runtime">,
  Flags<[HelpHidden, DoesNotAffectIncrementalBuild]>,
  HelpText<"Don't link in additions to the Objective-C runtime">;

def static_stdlib: Flag<["-"], "static-stdlib">,
  Flags<[DoesNotAffectIncrementalBuild]>,
  HelpText<"Statically link the Swift standard library">;
def no_static_stdlib: Flag<["-"], "no-static-stdlib">,
  Flags<[HelpHidden,DoesNotAffectIncrementalBuild]>,
  HelpText<"Don't statically link the Swift standard library">;

def static_executable : Flag<["-"], "static-executable">,
  HelpText<"Statically link the executable">;
def no_static_executable : Flag<["-"], "no-static-executable">,
  Flags<[HelpHidden]>,
  HelpText<"Don't statically link the executable">;

def use_ld : Joined<["-"], "use-ld=">,
  Flags<[DoesNotAffectIncrementalBuild]>,
  HelpText<"Specifies the linker to be used">;

def Xlinker : Separate<["-"], "Xlinker">,
  Flags<[DoesNotAffectIncrementalBuild]>,
  HelpText<"Specifies an option which should be passed to the linker">;

// Optimization levels

def O_Group : OptionGroup<"<optimization level options>">;

def Onone : Flag<["-"], "Onone">, Group<O_Group>, Flags<[FrontendOption]>,
  HelpText<"Compile without any optimization">;
def O : Flag<["-"], "O">, Group<O_Group>, Flags<[FrontendOption]>,
  HelpText<"Compile with optimizations">;
def Ounchecked : Flag<["-"], "Ounchecked">, Group<O_Group>,
  Flags<[FrontendOption]>,
  HelpText<"Compile with optimizations and remove runtime safety checks">;
def Oplayground : Flag<["-"], "Oplayground">, Group<O_Group>,
  Flags<[HelpHidden, FrontendOption]>,
  HelpText<"Compile with optimizations appropriate for a playground">;

def AssumeSingleThreaded : Flag<["-"], "assume-single-threaded">,
  Flags<[FrontendOption, HelpHidden]>,
  HelpText<"Assume that code will be executed in a single-threaded "
           "environment">;

// Debug info options

def g_Group : OptionGroup<"<debug info options>">;

def g : Flag<["-"], "g">, Group<g_Group>, Flags<[FrontendOption]>,
  HelpText<"Emit debug info. "
           "This is the preferred setting for debugging with LLDB.">;
def gnone : Flag<["-"], "gnone">, Group<g_Group>, Flags<[FrontendOption]>,
  HelpText<"Don't emit debug info">;
def gline_tables_only : Flag<["-"], "gline-tables-only">,
  Group<g_Group>, Flags<[FrontendOption]>,
  HelpText<"Emit minimal debug info for backtraces only">;
def gdwarf_types : Flag<["-"], "gdwarf-types">,
  Group<g_Group>, Flags<[FrontendOption]>,
  HelpText<"Emit full DWARF type info.">;

// Verify debug info
def verify_debug_info : Flag<["-"], "verify-debug-info">,
  Flags<[NoInteractiveOption, DoesNotAffectIncrementalBuild]>,
  HelpText<"Verify the binary representation of debug output.">;

// Assert configuration identifiers.
def AssertConfig : Separate<["-"], "assert-config">,
  Flags<[FrontendOption]>,
  HelpText<"Specify the assert_configuration replacement. "
           "Possible values are Debug, Release, Unchecked, DisableReplacement.">;

// Code formatting options

def code_formatting_Group : OptionGroup<"<code formatting options>">;

def use_tabs : Flag<["-"], "use-tabs">, Group<code_formatting_Group>,
  Flags<[NoInteractiveOption, NoBatchOption, SwiftFormatOption]>,
  HelpText<"Use tabs for indentation.">;

def indent_switch_case : Flag<["-"], "indent-switch-case">,
  Group<code_formatting_Group>,
  Flags<[NoInteractiveOption, NoBatchOption, SwiftFormatOption]>,
  HelpText<"Indent cases in switch statements.">;

def in_place : Flag<["-"], "in-place">, Group<code_formatting_Group>,
  Flags<[NoInteractiveOption, NoBatchOption, SwiftFormatOption]>,
  HelpText<"Overwrite input file with formatted file.">;

def tab_width : Separate<["-"], "tab-width">, Group<code_formatting_Group>,
  Flags<[NoInteractiveOption, NoBatchOption, SwiftFormatOption]>,
  HelpText<"Width of tab character.">, MetaVarName<"<n>">;

def indent_width : Separate<["-"], "indent-width">, Group<code_formatting_Group>,
  Flags<[NoInteractiveOption, NoBatchOption, SwiftFormatOption]>,
  HelpText<"Number of characters to indent.">, MetaVarName<"<n>">;

def line_range : Separate<["-"], "line-range">, Group<code_formatting_Group>,
  Flags<[NoInteractiveOption, NoBatchOption, SwiftFormatOption]>,
  HelpText<"<start line>:<end line>. Formats a range of lines (1-based). "
           "Can only be used with one input file.">, MetaVarName<"<n:n>">;

// File types

def parse_as_library : Flag<["-"], "parse-as-library">,
  Flags<[FrontendOption, NoInteractiveOption]>,
  HelpText<"Parse the input file(s) as libraries, not scripts">;
def parse_sil : Flag<["-"], "parse-sil">,
  Flags<[FrontendOption, NoInteractiveOption]>,
  HelpText<"Parse the input file as SIL code, not Swift source">;
def parse_stdlib : Flag<["-"], "parse-stdlib">,
  Flags<[FrontendOption, HelpHidden]>,
  HelpText<"Parse the input file(s) as the Swift standard library">;

def modes_Group : OptionGroup<"<mode options>">, HelpText<"MODES">;

class ModeOpt : Group<modes_Group>;

// Output Modes
def emit_executable : Flag<["-"], "emit-executable">,
  HelpText<"Emit a linked executable">, ModeOpt,
  Flags<[NoInteractiveOption, DoesNotAffectIncrementalBuild]>;
def emit_library : Flag<["-"], "emit-library">,
  HelpText<"Emit a linked library">, ModeOpt,
  Flags<[NoInteractiveOption]>;
def emit_object : Flag<["-"], "emit-object">,
  HelpText<"Emit object file(s) (-c)">, ModeOpt,
  Flags<[FrontendOption, NoInteractiveOption, DoesNotAffectIncrementalBuild]>;
def emit_assembly : Flag<["-"], "emit-assembly">,
  HelpText<"Emit assembly file(s) (-S)">, ModeOpt,
  Flags<[FrontendOption, NoInteractiveOption, DoesNotAffectIncrementalBuild]>;
def emit_bc : Flag<["-"], "emit-bc">,
  HelpText<"Emit LLVM BC file(s)">, ModeOpt,
  Flags<[FrontendOption, NoInteractiveOption, DoesNotAffectIncrementalBuild]>;
def emit_ir : Flag<["-"], "emit-ir">,
  HelpText<"Emit LLVM IR file(s)">, ModeOpt,
  Flags<[FrontendOption, NoInteractiveOption, DoesNotAffectIncrementalBuild]>;
def emit_sil : Flag<["-"], "emit-sil">,
  HelpText<"Emit canonical SIL file(s)">, ModeOpt,
  Flags<[FrontendOption, NoInteractiveOption, DoesNotAffectIncrementalBuild]>;
def emit_silgen : Flag<["-"], "emit-silgen">,
  HelpText<"Emit raw SIL file(s)">, ModeOpt,
  Flags<[FrontendOption, NoInteractiveOption, DoesNotAffectIncrementalBuild]>;
def emit_sib : Flag<["-"], "emit-sib">,
  HelpText<"Emit serialized AST + canonical SIL file(s)">, ModeOpt,
  Flags<[FrontendOption, NoInteractiveOption, DoesNotAffectIncrementalBuild]>;
def emit_sibgen : Flag<["-"], "emit-sibgen">,
  HelpText<"Emit serialized AST + raw SIL file(s)">, ModeOpt,
  Flags<[FrontendOption, NoInteractiveOption, DoesNotAffectIncrementalBuild]>;
def emit_imported_modules : Flag<["-"], "emit-imported-modules">,
  HelpText<"Emit a list of the imported modules">, ModeOpt,
  Flags<[FrontendOption, NoInteractiveOption, DoesNotAffectIncrementalBuild]>;
def emit_tbd : Flag<["-"], "emit-tbd">,
  HelpText<"Emit a TBD file">, ModeOpt,
  Flags<[FrontendOption, NoInteractiveOption, DoesNotAffectIncrementalBuild]>;

def c : Flag<["-"], "c">, Alias<emit_object>,
  Flags<[FrontendOption, NoInteractiveOption]>, ModeOpt;
def S: Flag<["-"], "S">, Alias<emit_assembly>,
  Flags<[FrontendOption, NoInteractiveOption]>, ModeOpt;

def update_code : Flag<["-"], "update-code">,
  HelpText<"Update Swift code">, ModeOpt,
  Flags<[HelpHidden, NoInteractiveOption, DoesNotAffectIncrementalBuild]>;

def fixit_code : Flag<["-"], "fixit-code">,
  HelpText<"Get compiler fixits as code edits">,
  Flags<[NoInteractiveOption, DoesNotAffectIncrementalBuild]>;
def fixit_all : Flag<["-"], "fixit-all">,
  HelpText<"Apply all fixits from diagnostics without any filtering">,
  Flags<[FrontendOption, NoInteractiveOption, DoesNotAffectIncrementalBuild]>;

// No Output Modes
def parse: Flag<["-"], "parse">,
  HelpText<"Parse input file(s)">, ModeOpt,
  Flags<[FrontendOption, NoInteractiveOption, DoesNotAffectIncrementalBuild]>;
def typecheck : Flag<["-"], "typecheck">,
  HelpText<"Parse and type-check input file(s)">, ModeOpt,
  Flags<[FrontendOption, NoInteractiveOption, DoesNotAffectIncrementalBuild]>;
def dump_parse : Flag<["-"], "dump-parse">,
  HelpText<"Parse input file(s) and dump AST(s)">, ModeOpt,
  Flags<[FrontendOption, NoInteractiveOption, DoesNotAffectIncrementalBuild]>;
def dump_ast : Flag<["-"], "dump-ast">,
  HelpText<"Parse and type-check input file(s) and dump AST(s)">, ModeOpt,
  Flags<[FrontendOption, NoInteractiveOption, DoesNotAffectIncrementalBuild]>;
def dump_scope_maps : Separate<["-"], "dump-scope-maps">,
  HelpText<"Parse and type-check input file(s) and dump the scope map(s)">,
  MetaVarName<"<expanded-or-list-of-line:column>">,
  ModeOpt,
  Flags<[FrontendOption, NoInteractiveOption, DoesNotAffectIncrementalBuild]>;
def dump_type_refinement_contexts :
  Flag<["-"], "dump-type-refinement-contexts">,
  HelpText<"Type-check input file(s) and dump type refinement contexts(s)">,
  ModeOpt,
  Flags<[FrontendOption, NoInteractiveOption, DoesNotAffectIncrementalBuild]>;
def print_ast : Flag<["-"], "print-ast">,
  HelpText<"Parse and type-check input file(s) and pretty print AST(s)">,
  ModeOpt,
  Flags<[FrontendOption, NoInteractiveOption, DoesNotAffectIncrementalBuild]>;

// Other Modes
def repl : Flag<["-"], "repl">,
  HelpText<"REPL mode (the default if there is no input file)">,
  Flags<[FrontendOption, NoBatchOption, HelpHidden]>, ModeOpt;
def lldb_repl : Flag<["-"], "lldb-repl">, HelpText<"LLDB-enhanced REPL mode">,
  Flags<[NoBatchOption, HelpHidden]>, ModeOpt;
def deprecated_integrated_repl : Flag<["-"], "deprecated-integrated-repl">,
  Flags<[FrontendOption, NoBatchOption]>, ModeOpt;
def i : Flag<["-"], "i">, ModeOpt; // only used to provide diagnostics.

def whole_module_optimization : Flag<["-"], "whole-module-optimization">,
  HelpText<"Optimize input files together instead of individually">,
  Flags<[FrontendOption, NoInteractiveOption]>;

def wmo : Flag<["-"], "wmo">, Alias<whole_module_optimization>,
  Flags<[FrontendOption, NoInteractiveOption, HelpHidden]>;

def force_single_frontend_invocation :
  Flag<["-"], "force-single-frontend-invocation">,
  Alias<whole_module_optimization>,
  Flags<[FrontendOption, NoInteractiveOption, HelpHidden]>;

def num_threads : Separate<["-"], "num-threads">,
  Flags<[FrontendOption, DoesNotAffectIncrementalBuild]>,
  HelpText<"Enable multi-threading and specify number of threads">,
  MetaVarName<"<n>">;

def Xfrontend : Separate<["-"], "Xfrontend">, Flags<[HelpHidden]>,
  MetaVarName<"<arg>">, HelpText<"Pass <arg> to the Swift frontend">;

def Xcc : Separate<["-"], "Xcc">, Flags<[FrontendOption]>,
  MetaVarName<"<arg>">,
  HelpText<"Pass <arg> to the C/C++/Objective-C compiler">;

def Xllvm : Separate<["-"], "Xllvm">,
  Flags<[FrontendOption, HelpHidden]>,
  MetaVarName<"<arg>">, HelpText<"Pass <arg> to LLVM.">;

def resource_dir : Separate<["-"], "resource-dir">,
  Flags<[FrontendOption, HelpHidden]>,
  MetaVarName<"</usr/lib/swift>">,
  HelpText<"The directory that holds the compiler resource files">;

def target : Separate<["-"], "target">,
  Flags<[FrontendOption, ModuleWrapOption]>,
  HelpText<"Generate code for the given target">;
def target_legacy_spelling : Joined<["--"], "target=">,
  Flags<[FrontendOption]>, Alias<target>;

def target_cpu : Separate<["-"], "target-cpu">, Flags<[FrontendOption]>,
  HelpText<"Generate code for a particular CPU variant">;

def profile_generate : Flag<["-"], "profile-generate">,
  Flags<[FrontendOption, NoInteractiveOption]>,
  HelpText<"Generate instrumented code to collect execution counts">;

def profile_coverage_mapping : Flag<["-"], "profile-coverage-mapping">,
  Flags<[FrontendOption, NoInteractiveOption]>,
  HelpText<"Generate coverage data for use with profiled execution counts">;

def embed_bitcode : Flag<["-"], "embed-bitcode">,
  Flags<[FrontendOption, NoInteractiveOption]>,
  HelpText<"Embed LLVM IR bitcode as data">;

def embed_bitcode_marker : Flag<["-"], "embed-bitcode-marker">,
  Flags<[FrontendOption, NoInteractiveOption]>,
  HelpText<"Embed placeholder LLVM IR data as a marker">;

def enable_testing : Flag<["-"], "enable-testing">,
  Flags<[FrontendOption, NoInteractiveOption, HelpHidden]>,
  HelpText<"Allows this module's internal API to be accessed for testing">;

def sanitize_EQ : CommaJoined<["-"], "sanitize=">,
  Flags<[FrontendOption, NoInteractiveOption]>, MetaVarName<"<check>">,
  HelpText<"Turn on runtime checks for erroneous behavior.">;

def sanitize_coverage_EQ : CommaJoined<["-"], "sanitize-coverage=">,
  Flags<[FrontendOption, NoInteractiveOption]>,
  MetaVarName<"<type>">,
  HelpText<"Specify the type of coverage instrumentation for Sanitizers and"
  " additional options separated by commas">;

def enforce_exclusivity_EQ : Joined<["-"], "enforce-exclusivity=">,
  Flags<[FrontendOption]>, MetaVarName<"<enforcement>">,
  HelpText<"Enforce law of exclusivity">;

include "FrontendOptions.td"

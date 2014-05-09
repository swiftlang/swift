//===--- Options.td - Options for swift driver ----------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2015 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See http://swift.org/LICENSE.txt for license information
// See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
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

// The option is a "driver" option, and should not be forwarded to other tools.
def DriverOption : OptionFlag;

// The option should be accepted by swift -frontend.
def FrontendOption : OptionFlag;

// The option should not be accepted by the driver.
def NoDriverOption : OptionFlag;

/////////
// Groups

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
  Flags<[DriverOption, HelpHidden]>;

def driver_print_actions : Flag<["-"], "driver-print-actions">,
  InternalDebugOpt, HelpText<"Dump list of actions to perform">;
def driver_print_output_file_map : Flag<["-"], "driver-print-output-file-map">,
  InternalDebugOpt, HelpText<"Dump the contents of the output file map">;
def driver_print_bindings : Flag<["-"], "driver-print-bindings">,
  InternalDebugOpt, HelpText<"Dump list of bindings">;
def driver_print_jobs : Flag<["-"], "driver-print-jobs">, InternalDebugOpt,
  HelpText<"Dump list of jobs to execute">;
def _HASH_HASH_HASH : Flag<["-"], "###">, Alias<driver_print_jobs>;
def driver_skip_execution : Flag<["-"], "driver-skip-execution">,
  InternalDebugOpt,
  HelpText<"Skip execution of subtasks when performing compilation">;

def help : Flag<["-", "--"], "help">, Flags<[DriverOption, FrontendOption]>,
  HelpText<"Display available options">;
def help_hidden : Flag<["-", "--"], "help-hidden">,
  Flags<[HelpHidden, DriverOption, FrontendOption]>,
  HelpText<"Display available options, including hidden options">;

def v : Flag<["-"], "v">,
  HelpText<"Show commands to run and use verbose output">;
def _version : Flag<["--"], "version">;

// Standard Options
def _DASH_DASH : Option<["--"], "", KIND_REMAINING_ARGS>,
  Flags<[DriverOption, FrontendOption]>;

def o : JoinedOrSeparate<["-"], "o">,
  Flags<[DriverOption, FrontendOption]>,
  HelpText<"Write output to <file>">, MetaVarName<"<file>">;

def j : JoinedOrSeparate<["-"], "j">, Flags<[DriverOption]>,
  HelpText<"Number of commands to execute in parallel">;

def arch : Separate<["-"], "arch">, Flags<[DriverOption]>,
  HelpText<"Compile for architecture <arch>">, MetaVarName<"<arch>">;
def sdk : Separate<["-"], "sdk">, Flags<[DriverOption, FrontendOption]>,
  HelpText<"Compile against <sdk>">, MetaVarName<"<sdk>">;
def g : Flag<["-"], "g">, Flags<[FrontendOption]>,
  HelpText<"Emit debug info">;

def D : JoinedOrSeparate<["-"], "D">, Flags<[FrontendOption]>,
  HelpText<"Specifies one or more build configuration options">;

def F : JoinedOrSeparate<["-"], "F">, Flags<[FrontendOption]>,
  HelpText<"Add directory to framework search path">;
def F_EQ : Joined<["-"], "F=">, Flags<[FrontendOption]>,
  Alias<F>;

def I : JoinedOrSeparate<["-"], "I">, Flags<[FrontendOption]>,
  HelpText<"Add directory to the import search path">;
def I_EQ : Joined<["-"], "I=">, Flags<[FrontendOption]>,
  Alias<I>;

def import_underlying_module : Flag<["-"], "import-underlying-module">,
  Flags<[FrontendOption]>,
  HelpText<"Implicitly imports the Objective-C half of a module">;
def import_objc_header : Separate<["-"], "import-objc-header">,
  Flags<[FrontendOption,HelpHidden]>,
  HelpText<"Implicitly imports an Objective-C header file">;

def nostdimport : Flag<["-"], "nostdimport">,
  Flags<[DriverOption,FrontendOption]>,
  HelpText<"Don't search the standard library import path for modules">;

def output_file_map : Separate<["-"], "output-file-map">, Flags<[DriverOption]>,
  HelpText<"A file which specifies the location of outputs">,
  MetaVarName<"<path>">;
def output_file_map_EQ : Joined<["-"], "output-file-map=">,
  Flags<[DriverOption]>, Alias<output_file_map>;

def serialize_diagnostics : Flag<["-"], "serialize-diagnostics">,
  Flags<[DriverOption, FrontendOption]>,
  HelpText<"Serialize diagnostics in a binary format">;

def module_cache_path : Separate<["-"], "module-cache-path">,
  Flags<[DriverOption, FrontendOption]>,
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

def emit_module : Flag<["-"], "emit-module">, Flags<[FrontendOption]>,
  HelpText<"Emit an importable module">;
def emit_module_path : Separate<["-"], "emit-module-path">,
  Flags<[FrontendOption]>, HelpText<"Emit an importable module to <path>">,
  MetaVarName<"<path>">;
def emit_module_path_EQ : Joined<["-"], "emit-module-path=">,
  Flags<[FrontendOption]>, Alias<emit_module_path>;

def emit_objc_header : Flag<["-"], "emit-objc-header">, Flags<[FrontendOption]>,
  HelpText<"Emit an Objective-C header file">;
def emit_objc_header_path : Separate<["-"], "emit-objc-header-path">,
  Flags<[FrontendOption]>, MetaVarName<"<path>">,
  HelpText<"Emit an Objective-C header file to <path>">;

def split_objc_selectors : Flag<["-"], "split-objc-selectors">,
  Flags<[FrontendOption]>,
  HelpText<"Split imported Objective-C selectors based on prepositions">;

def implicit_objc_with : Flag<["-"], "implicit-objc-with">,
  Flags<[FrontendOption]>,
  HelpText<"Make 'with' in the first initializer argument implicit">;

def import_cf_types : Flag<["-"], "import-cf-types">,
  Flags<[FrontendOption]>,
  HelpText<"Recognize and import CF types as class types">;

// Platform options.
def enable_app_extension : Flag<["-"], "application-extension">,
  Flags<[DriverOption, FrontendOption]>,
  HelpText<"Restrict code to those available for App Extensions">;

// Linker options

def linker_option_Group : OptionGroup<"<linker-specific options>">;

def l : Joined<["-"], "l">, Flags<[FrontendOption]>,
  Group<linker_option_Group>,
  HelpText<"Specifies a library which should be linked against">;
def framework : Separate<["-"], "framework">, Flags<[FrontendOption]>,
  Group<linker_option_Group>,
  HelpText<"Specifies a framework which should be linked against">;

def L : JoinedOrSeparate<["-"], "L">, Group<linker_option_Group>,
  HelpText<"Add directory to library link search path">;
def L_EQ : Joined<["-"], "L=">, Alias<L>;

def link_objc_runtime : Flag<["-"], "link-objc-runtime">;
def no_link_objc_runtime : Flag<["-"], "no-link-objc-runtime">,
  HelpText<"Don't link in additions to the Objective-C runtime">;

def Xlinker : Separate<["-"], "Xlinker">,
  HelpText<"Specifies an option which should be passed to the linker">;

// Optimization levels

def O_Group : OptionGroup<"<optimization level options>">;

def O0 : Flag<["-"], "O0">, Group<O_Group>, Flags<[FrontendOption]>;
def Onone : Flag<["-"], "Onone">, Group<O_Group>, Flags<[FrontendOption]>,
            Alias<O0>;

def O : Joined<["-"], "O">, Group<O_Group>, Flags<[FrontendOption]>;
def Ofast : Flag<["-"], "Ofast">, Group<O_Group>, Flags<[FrontendOption]>;

// Assert configuration identifiers.
def AssertConfig : Separate<["-"], "assert-config">,
  Flags<[FrontendOption]>,
  HelpText<"Specify the assert_configuration replacement. "
           "Possible values are Debug, Release, Replacement.">;

// File types

def parse_as_library : Flag<["-"], "parse-as-library">,
  Flags<[DriverOption, FrontendOption]>,
  HelpText<"Parse the input file(s) as libraries, not scripts">;
def parse_sil : Flag<["-"], "parse-sil">,
  Flags<[DriverOption, FrontendOption]>,
  HelpText<"Parse the input file as SIL code, not Swift source">;
def parse_stdlib : Flag<["-"], "parse-stdlib">,
  Flags<[DriverOption, FrontendOption, HelpHidden]>,
  HelpText<"Parse the input file(s) as the Swift standard library">;

def modes_Group : OptionGroup<"<mode options>">, HelpText<"MODES">;

class ModeOpt : Group<modes_Group>;

// Output Modes
def emit_executable : Flag<["-"], "emit-executable">, Flags<[DriverOption]>,
  HelpText<"Emit a linked executable">, ModeOpt;
def emit_library : Flag<["-"], "emit-library">, Flags<[DriverOption]>,
  HelpText<"Emit a linked library">, ModeOpt;
def c : Flag<["-"], "c">, HelpText<"Emit object file(s)">,
  Flags<[FrontendOption]>, ModeOpt;
def emit_object : Flag<["-"], "emit-object">, Alias<c>, Flags<[FrontendOption]>,
  ModeOpt;
def S : Flag<["-"], "S">, HelpText<"Emit assembly file(s)">,
  Flags<[FrontendOption]>, ModeOpt;
def emit_assembly: Flag<["-"], "emit-assembly">, Alias<S>,
  Flags<[FrontendOption]>, ModeOpt;
def emit_sil : Flag<["-"], "emit-sil">, HelpText<"Emit canonical SIL file(s)">,
  Flags<[FrontendOption]>, ModeOpt;
def emit_silgen : Flag<["-"], "emit-silgen">, HelpText<"Emit raw SIL file(s)">,
  Flags<[FrontendOption]>, ModeOpt;
def emit_ir : Flag<["-"], "emit-ir">,
  HelpText<"Emit LLVM IR file(s)">, Flags<[FrontendOption]>, ModeOpt;
def emit_bc : Flag<["-"], "emit-bc">,
  HelpText<"Emit LLVM BC file(s)">, Flags<[FrontendOption]>, ModeOpt;

// No Output Modes
def parse : Flag<["-"], "parse">, HelpText<"Parse input file(s)">,
  Flags<[FrontendOption]>, ModeOpt;
def dump_parse : Flag<["-"], "dump-parse">, Flags<[FrontendOption]>, ModeOpt,
  HelpText<"Parse input file(s) and dump AST(s)">;
def dump_ast : Flag<["-"], "dump-ast">, Flags<[FrontendOption]>, ModeOpt,
  HelpText<"Parse and type-check input file(s) and dump AST(s)">;
def print_ast : Flag<["-"], "print-ast">, Flags<[FrontendOption]>, ModeOpt,
  HelpText<"Parse and type-check input file(s) and pretty print AST(s)">;

// Other Modes
def repl : Flag<["-"], "repl">, HelpText<"REPL mode">, Flags<[FrontendOption]>,
  ModeOpt;
def lldb_repl : Flag<["-"], "lldb-repl">, HelpText<"LLDB-enhanced REPL mode">,
  ModeOpt;
def integrated_repl : Flag<["-"], "integrated-repl">, Flags<[FrontendOption]>,
  HelpText<"Integrated REPL mode">, ModeOpt;
def experimental_prefer_lldb : Flag<["-"], "experimental-prefer-lldb">,
  Flags<[HelpHidden]>;

def i : Flag<["-"], "i">, HelpText<"Immediate mode">, Flags<[FrontendOption]>,
  ModeOpt;

def force_single_frontend_invocation :
  Flag<["-"], "force-single-frontend-invocation">,
  HelpText<"Compile the module using a single frontend invocation">,
  Flags<[DriverOption, HelpHidden]>;

def Xfrontend : Separate<["-"], "Xfrontend">, Flags<[DriverOption]>,
  MetaVarName<"<arg>">, HelpText<"Pass <arg> to the Swift frontend">;

def Xcc : Separate<["-"], "Xcc">, Flags<[DriverOption, FrontendOption]>,
  MetaVarName<"<arg>">,
  HelpText<"Pass <arg> to the C/C++/Objective-C compiler">;

def Xllvm : Separate<["-"], "Xllvm">, Flags<[DriverOption, FrontendOption]>,
  MetaVarName<"<arg>">,
  HelpText<"Pass <arg> to llvm.">;

def resource_dir : Separate<["-"], "resource-dir">,
  Flags<[DriverOption, FrontendOption, HelpHidden]>,
  MetaVarName<"</usr/lib/swift>">,
  HelpText<"The directory that holds the compiler resource files">;

def target : Joined<["--"], "target=">, Flags<[DriverOption, FrontendOption]>,
  HelpText<"Generate code for the given target">;
def target_legacy_spelling : Separate<["-"], "target">,
  Flags<[DriverOption, FrontendOption]>, Alias<target>;

def target_cpu : Separate<["-"], "target-cpu">,
  Flags<[DriverOption, FrontendOption]>,
  HelpText<"Generate code for a particular CPU variant">;
def target_feature : Separate<["-"], "target-feature">,
  Flags<[DriverOption, FrontendOption]>,
  HelpText<"Generate code with a particular CPU feature enabled or disabled">,
  MetaVarName<"[+-]<feature-name>">;

def working_directory : Separate<["-"], "working-directory">,
  Flags<[FrontendOption]>,
  HelpText<"Resolve file paths relative to the specified directory">;
def working_directory_EQ : Joined<["-"], "working-directory=">,
  Alias<working_directory>;

include "FrontendOptions.td"

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
def driver_print_bindings : Flag<["-"], "driver-print-bindings">,
  InternalDebugOpt, HelpText<"Dump list of bindings">;
def driver_print_jobs : Flag<["-"], "driver-print-jobs">, InternalDebugOpt,
  HelpText<"Dump list of jobs to execute">;

def help : Flag<["-", "--"], "help">, Flags<[DriverOption, FrontendOption]>,
  HelpText<"Display available options">;
def help_hidden : Flag<["-"], "help-hidden">,
  Flags<[HelpHidden, DriverOption, FrontendOption]>,
  HelpText<"Display available options, including hidden options">;
def _help_hidden : Flag<["--"], "help-hidden">, Alias<help_hidden>,
  Flags<[HelpHidden, DriverOption, FrontendOption]>;

def v : Flag<["-"], "v">,
  HelpText<"Show commands to run and use verbose output">;
def _version : Flag<["--"], "version">;

// Standard Options
def _DASH_DASH : Option<["--"], "", KIND_REMAINING_ARGS>,
  Flags<[DriverOption, FrontendOption]>;

def o : JoinedOrSeparate<["-"], "o">, Flags<[DriverOption, FrontendOption]>,
  HelpText<"Write output to <file>">, MetaVarName<"<file>">;

def j : JoinedOrSeparate<["-"], "j">, Flags<[DriverOption]>,
  HelpText<"Number of commands to execute in parallel">;

def arch : Separate<["-"], "arch">, Flags<[DriverOption]>,
  HelpText<"Compile for architecture <arch>">, MetaVarName<"<arch>">;
def sdk : Separate<["-"], "sdk">, Flags<[DriverOption, FrontendOption]>,
  HelpText<"Compile against <sdk>">, MetaVarName<"<sdk>">;
def g : Flag<["-"], "g">, Flags<[FrontendOption]>, HelpText<"Emit debug info">;

def F : JoinedOrSeparate<["-"], "F">, Flags<[FrontendOption]>,
  HelpText<"Add directory to framework search path">;
def F_EQ : Joined<["-"], "F=">, Flags<[FrontendOption]>,
  Alias<F>;

def I : JoinedOrSeparate<["-"], "I">, Flags<[FrontendOption]>,
  HelpText<"Add directory to the import search path">;
def I_EQ : Joined<["-"], "I=">, Flags<[FrontendOption]>,
  Alias<I>;

def serialize_diagnostics : Separate<["-"], "serialize-diagnostics">,
  Flags<[DriverOption, FrontendOption]>,
  HelpText<"File for serializing diagnostics in a binary format">;
def serialize_diagnostics_EQ : Joined<["-"], "serialize-diagnostics=">,
  Flags<[DriverOption, FrontendOption]>, Alias<serialize_diagnostics>;

def module_cache_path : Separate<["-"], "module-cache-path">,
  Flags<[DriverOption, FrontendOption]>,
  HelpText<"Specifies the Clang module cache path">;
def module_cache_path_EQ : Joined<["-"], "module-cache-path=">,
  Flags<[DriverOption, FrontendOption]>, Alias<module_cache_path>;

def module_source_list : Separate<["-"], "module-source-list">,
  Flags<[FrontendOption]>,
  HelpText<"Provide a list of all source files in the module">;
def module_source_list_EQ : Joined<["-"], "module-source-list=">,
  Flags<[FrontendOption]>, Alias<module_source_list>;

def module_name : Separate<["-"], "module-name">, Flags<[FrontendOption]>,
  HelpText<"Name of the module to build">;

def module_name_EQ : Joined<["-"], "module-name=">, Flags<[FrontendOption]>,
  Alias<module_name>;

// Linker inputs

def l : JoinedOrSeparate<["-"], "l">, Flags<[FrontendOption]>,
  HelpText<"Specifies a library which should be linked against">;
def framework : Separate<["-"], "framework">, Flags<[FrontendOption]>,
  HelpText<"Specifies a framework which should be linked against">;

// File types

def parse_as_library : Flag<["-"], "parse-as-library">,
  Flags<[DriverOption, FrontendOption]>,
  HelpText<"Parse the input file(s) as libraries, not scripts">;
def parse_stdlib : Flag<["-"], "parse-stdlib">,
  Flags<[DriverOption, FrontendOption, HelpHidden]>,
  HelpText<"Parse the input file(s) as the Swift standard library">;

def modes_Group : OptionGroup<"<mode options>">, HelpText<"MODES">;

class ModeOpt : Group<modes_Group>;

// Output Modes
def emit_executable : Flag<["-"], "emit-executable">, Flags<[DriverOption]>,
  HelpText<"Emit a linked executable">, ModeOpt;
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
def i : Flag<["-"], "i">, HelpText<"Immediate mode">, Flags<[FrontendOption]>,
  ModeOpt;

def Xfrontend : Separate<["-"], "Xfrontend">, Flags<[DriverOption]>,
  MetaVarName<"<arg>">, HelpText<"Pass <arg> to the Swift frontend">;

def Xcc : Separate<["-"], "Xcc">, Flags<[DriverOption, FrontendOption]>,
  MetaVarName<"<arg>">,
  HelpText<"Pass <arg> to the C/C++/Objective-C compiler">;

def target : Joined<["--"], "target=">, Flags<[DriverOption, FrontendOption]>,
  HelpText<"Generate code for the given target">;
def target_legacy_spelling : Separate<["-"], "target">,
  Flags<[DriverOption, FrontendOption]>, Alias<target>;

def working_directory : Separate<["-"], "working-directory">,
  Flags<[FrontendOption]>,
  HelpText<"Resolve file paths relative to the specified directory">;
def working_directory_EQ : Joined<["-"], "working-directory=">,
  Alias<working_directory>;

include "FrontendOptions.td"

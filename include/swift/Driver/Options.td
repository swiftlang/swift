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

// DriverOption - The option is a "driver" option, and should not be forwarded
// to other tools.
def DriverOption : OptionFlag;

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

def help : Flag<["-", "--"], "help">, HelpText<"Display available options">;
def _help_hidden : Flag<["--"], "help-hidden">,
  HelpText<"Display available options, including hidden options">,
  Flags<[HelpHidden]>;
def v : Flag<["-"], "v">,
  HelpText<"Show commands to run and use verbose output">;
def _version : Flag<["--"], "version">;

// Standard Options
def _DASH_DASH : Option<["--"], "", KIND_REMAINING_ARGS>, Flags<[DriverOption]>;

def o : JoinedOrSeparate<["-"], "o">, Flags<[DriverOption]>,
  HelpText<"Write output to <file>">, MetaVarName<"<file>">;

def arch : Separate<["-"], "arch">, Flags<[DriverOption]>,
  HelpText<"Compile for architecture <arch>">, MetaVarName<"<arch>">;

def modes_Group : OptionGroup<"<mode options>">, HelpText<"MODES">;

class ModeOpt : Group<modes_Group>;

// Output Modes
def emit_executable : Flag<["-"], "emit-executable">, Flags<[DriverOption]>,
  HelpText<"Emit a linked executable">, ModeOpt;
def c : Flag<["-"], "c">, HelpText<"Emit object file(s)">, ModeOpt;
def emit_object : Flag<["-"], "emit-object">, Alias<c>, ModeOpt;
def S : Flag<["-"], "S">, HelpText<"Emit assembly file(s)">, ModeOpt;
def emit_assembly: Flag<["-"], "emit-assembly">, Alias<S>, ModeOpt;
def emit_sil : Flag<["-"], "emit-sil">, HelpText<"Emit canonical SIL file(s)">,
  ModeOpt;
def emit_silgen : Flag<["-"], "emit-silgen">, HelpText<"Emit raw SIL file(s)">,
  ModeOpt;
// -emit-llvm/-emit-llvm-ir/-emit-ir
// -emit-llvm-bc/-emit-bc

// No Output Modes
def parse : Flag<["-"], "parse">, HelpText<"Parse input file(s)">, ModeOpt;
def dump_parse : Flag<["-"], "dump-parse">, ModeOpt,
  HelpText<"Parse input file(s) and dump AST(s)">;
def dump_ast : Flag<["-"], "dump-ast">, ModeOpt,
  HelpText<"Parse and type-check input file(s) and dump AST(s)">;
def print_ast : Flag<["-"], "print-ast">, ModeOpt,
  HelpText<"Parse and type-check input file(s) and pretty print AST(s)">;

// Other Modes
def repl : Flag<["-"], "repl">, HelpText<"REPL mode">, ModeOpt;
def i : Flag<["-"], "i">, HelpText<"Immediate mode">, ModeOpt;

def target : Joined<["--"], "target=">, Flags<[DriverOption]>,
  HelpText<"Generate code for the given target">;
def target_legacy_spelling : Separate<["-"], "target">, Alias<target>;

def working_directory : Separate<["-"], "working-directory">,
  HelpText<"Resolve file paths relative to the specified directory">;
def working_directory_EQ : Joined<["-"], "working-directory=">,
  Alias<working_directory>;

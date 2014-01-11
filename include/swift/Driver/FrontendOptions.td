//===--- FrontendOptions.td - Options for swift -frontend -----------------===//
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
//  This file defines the options accepted by swift -frontend.
//
//===----------------------------------------------------------------------===//

let Flags = [FrontendOption, NoDriverOption] in {

def delayed_function_body_parsing :
  Flag<["-"], "delayed-function-body-parsing">,
  HelpText<"Delay function body parsing until the end of all files">;

def primary_file : Separate<["-"], "primary-file">,
  HelpText<"Produce output for this file, not the whole module">;
def primary_file_EQ : Joined<["-"], "primary-file=">, Alias<primary_file>;

def verify : Flag<["-"], "verify">,
  HelpText<"Verify diagnostics against expected-{error|warning|note} "
           "annotations">;

} // end let Flags = [FrontendOption, NoDriverOption]

let Flags = [FrontendOption, NoDriverOption, HelpHidden] in {

def debug_constraints : Flag<["-"], "debug-constraints">,
  HelpText<"Debug the constraint-based type checker">;

def debug_constraints_attempt : Separate<["-"], "debug-constraints-attempt">,
  HelpText<"Debug the constraint solver at a given attempt">;
def debug_constraints_attempt_EQ : Joined<["-"], "debug-constraints-attempt=">,
  Alias<debug_constraints_attempt>;

def disable_diagnostic_passes : Flag<["-"], "disable-diagnostic-passes">,
  HelpText<"Don't run diagnostic passes">;

def disable_llvm_optzns : Flag<["-"], "disable-llvm-optzns">,
  HelpText<"Don't run LLVM optimization passes">;

def emit_verbose_sil : Flag<["-"], "emit-verbose-sil">,
  HelpText<"Emit locations during SIL emission">;

def enable_dynamic_value_type_layout :
  Flag<["-"], "enable-dynamic-value-type-layout">,
  HelpText<"Enable experimental dynamic generic struct/enum type layout">;

def print_stats : Flag<["-"], "print-stats">,
  HelpText<"Print various statistics">;

def use_malloc : Flag<["-"], "use-malloc">,
  HelpText<"Allocate internal data structures using malloc "
           "(for memory debugging)">;

} // end let Flags = [FrontendOption, NoDriverOption, HelpHidden]

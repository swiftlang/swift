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

def triple : Separate<["-"], "triple">, Alias<target>;

def delayed_function_body_parsing :
  Flag<["-"], "delayed-function-body-parsing">,
  HelpText<"Delay function body parsing until the end of all files">;

def primary_file : Separate<["-"], "primary-file">,
  HelpText<"Produce output for this file, not the whole module">;
def primary_file_EQ : Joined<["-"], "primary-file=">, Alias<primary_file>;

def emit_module_doc : Flag<["-"], "emit-module-doc">,
  HelpText<"Emit a module documentation file based on documentation "
           "comments">;
def emit_module_doc_path
  : Separate<["-"], "emit-module-doc-path">, MetaVarName<"<path>">,
    HelpText<"Output module documentation file <path>">;
def emit_module_doc_path_EQ
  : Joined<["-"], "emit-module-doc-path=">,
    Alias<emit_module_doc_path_EQ>;

def serialize_diagnostics_path
  : Separate<["-"], "serialize-diagnostics-path">, MetaVarName<"<path>">,
    HelpText<"Output serialized diagnostics to <path>">;
def serialize_diagnostics_path_EQ
  : Joined<["-"], "serialize-diagnostics-path=">,
    Alias<serialize_diagnostics_path>;

def verify : Flag<["-"], "verify">,
  HelpText<"Verify diagnostics against expected-{error|warning|note} "
           "annotations">;

} // end let Flags = [FrontendOption, NoDriverOption]

def debug_crash_Group : OptionGroup<"<automatic crashing options>">;
class DebugCrashOpt : Group<debug_crash_Group>;

let Flags = [FrontendOption, NoDriverOption, HelpHidden] in {

def debug_constraints : Flag<["-"], "debug-constraints">,
  HelpText<"Debug the constraint-based type checker">;

def debug_constraints_attempt : Separate<["-"], "debug-constraints-attempt">,
  HelpText<"Debug the constraint solver at a given attempt">;
def debug_constraints_attempt_EQ : Joined<["-"], "debug-constraints-attempt=">,
  Alias<debug_constraints_attempt>;

def debug_assert_immediately : Flag<["-"], "debug-assert-immediately">,
  DebugCrashOpt, HelpText<"Force an assertion failure immediately">;
def debug_assert_after_parse : Flag<["-"], "debug-assert-after-parse">,
  DebugCrashOpt, HelpText<"Force an assertion failure after parsing">;
def debug_crash_immediately : Flag<["-"], "debug-crash-immediately">,
  DebugCrashOpt, HelpText<"Force a crash immediately">;
def debug_crash_after_parse : Flag<["-"], "debug-crash-after-parse">,
  DebugCrashOpt, HelpText<"Force a crash after parsing">;

def debugger_support : Flag<["-"], "debugger-support">,
  HelpText<"Process swift code as if running in the debugger">;

def disable_arc_opts : Flag<["-"], "disable-arc-opts">,
  HelpText<"Don't run SIL ARC optimization passes.">;

def remove_runtime_asserts : Flag<["-"], "remove-runtime-asserts">,
HelpText<"Remove runtime asserts.">;

def disable_all_runtime_checks : Flag<["-"], "disable-all-runtime-checks">,
  HelpText<"Disable all intrinsic runtime safety checks">;

def disable_diagnostic_passes : Flag<["-"], "disable-diagnostic-passes">,
  HelpText<"Don't run diagnostic passes">;

def disable_llvm_optzns : Flag<["-"], "disable-llvm-optzns">,
  HelpText<"Don't run LLVM optimization passes">;

def disable_llvm_arc_opts : Flag<["-"], "disable-llvm-arc-opts">,
  HelpText<"Don't run LLVM ARC optimization passes.">;

def disable_sil_linking : Flag<["-"], "disable-sil-linking">,
  HelpText<"Don't link SIL functions">;

def emit_verbose_sil : Flag<["-"], "emit-verbose-sil">,
  HelpText<"Emit locations during SIL emission">;

def enable_dynamic_value_type_layout :
  Flag<["-"], "enable-dynamic-value-type-layout">,
  HelpText<"Enable experimental dynamic generic struct/enum/class type layout">;

def enable_experimental_patterns : Flag<["-"], "enable-experimental-patterns">,
  HelpText<"Enable experimental 'switch' pattern matching features">;

def enable_source_import : Flag<["-"], "enable-source-import">,
  HelpText<"Enable importing of Swift source files">;

def split_objc_selectors : Flag<["-"], "split-objc-selectors">,
  HelpText<"Split imported Objective-C selectors based on prepositions">;

def split_objc_selectors_before : Flag<["-"], "split-objc-selectors-before">,
  HelpText<"Split imported Objective-C selectors before the last preposition">;

def split_objc_selectors_after : Flag<["-"], "split-objc-selectors-after">,
  HelpText<"Split imported Objective-C selectors after the last preposition">;

def print_stats : Flag<["-"], "print-stats">,
  HelpText<"Print various statistics">;

def playground : Flag<["-"], "playground">,
  HelpText<"Apply the playground transformation">;

def sil_inline_threshold : Separate<["-"], "sil-inline-threshold">,
  MetaVarName<"<50>">,
  HelpText<"Controls the aggressiveness of performance inlining">;

def sil_devirt_threshold : Separate<["-"], "sil-devirt-threshold">,
  MetaVarName<"<0>">,
  HelpText<"Controls the aggressiveness of devirtualization">;

def sil_link_all : Flag<["-"], "sil-link-all">,
  HelpText<"Link all SIL functions">;

def sil_serialize_all : Flag<["-"], "sil-serialize-all">,
  HelpText<"Serialize all generated SIL">;

def sil_verify_all : Flag<["-"], "sil-verify-all">,
  HelpText<"Verify SIL after each transform">;
  
def sil_opt_pass_count : Separate<["-"], "sil-opt-pass-count">,
  MetaVarName<"<N>">,
  HelpText<"Stop optimizing after <N> optimization passes">;
  
def sil_print_all : Flag<["-"], "sil-print-all">,
  HelpText<"Print SIL after each transform">;
  
def sil_time_transforms : Flag<["-"], "sil-time-transforms">,
  HelpText<"Time each SIL transform invocation">;

def sil_debug_serialization : Flag<["-"], "sil-debug-serialization">,
  HelpText<"Do not eliminate functions in Mandatory Inlining/SILCombine dead "
           "functions. (for debugging only)">;

def use_malloc : Flag<["-"], "use-malloc">,
  HelpText<"Allocate internal data structures using malloc "
           "(for memory debugging)">;

} // end let Flags = [FrontendOption, NoDriverOption, HelpHidden]

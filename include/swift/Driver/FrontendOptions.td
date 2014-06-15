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

def target_abi : Separate<["-"], "target-abi">,
  HelpText<"Target a particular ABI">;

def color_diagnostics : Flag<["-"], "color-diagnostics">,
  HelpText<"Print diagnostics in color">;

def delayed_function_body_parsing :
  Flag<["-"], "delayed-function-body-parsing">,
  HelpText<"Delay function body parsing until the end of all files">;

def primary_file : Separate<["-"], "primary-file">,
  HelpText<"Produce output for this file, not the whole module">;

def emit_module_doc : Flag<["-"], "emit-module-doc">,
  HelpText<"Emit a module documentation file based on documentation "
           "comments">;
def emit_module_doc_path
  : Separate<["-"], "emit-module-doc-path">, MetaVarName<"<path>">,
    HelpText<"Output module documentation file <path>">;

def emit_dependencies_path
  : Separate<["-"], "emit-dependencies-path">, MetaVarName<"<path>">,
    HelpText<"Output dependencies file to <path>">;

def serialize_diagnostics_path
  : Separate<["-"], "serialize-diagnostics-path">, MetaVarName<"<path>">,
    HelpText<"Output serialized diagnostics to <path>">;

def verify : Flag<["-"], "verify">,
  HelpText<"Verify diagnostics against expected-{error|warning|note} "
           "annotations">;

def show_diagnostics_after_fatal : Flag<["-"], "show-diagnostics-after-fatal">,
  HelpText<"Keep emitting subsequent diagnostics after a fatal error">;
           
def enable_objc_attr_requires_objc_module :
  Flag<["-"], "enable-objc-attr-requires-objc-module">,
  HelpText<"Enable requiring uses of @objc require importing the "
           "ObjectiveC module">;

def disable_objc_attr_requires_objc_module :
  Flag<["-"], "disable-objc-attr-requires-objc-module">,
  HelpText<"Disable requiring uses of @objc require importing the "
           "ObjectiveC module">;

def print_clang_stats : Flag<["-"], "print-clang-stats">,
  HelpText<"Print Clang importer statistics">;

} // end let Flags = [FrontendOption, NoDriverOption]

def debug_crash_Group : OptionGroup<"<automatic crashing options>">;
class DebugCrashOpt : Group<debug_crash_Group>;

// HIDDEN FLAGS
let Flags = [FrontendOption, NoDriverOption, HelpHidden] in {

def debug_constraints : Flag<["-"], "debug-constraints">,
  HelpText<"Debug the constraint-based type checker">;

def debug_constraints_attempt : Separate<["-"], "debug-constraints-attempt">,
  HelpText<"Debug the constraint solver at a given attempt">;

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

def enable_character_literals : Flag<["-"], "enable-character-literals">,
HelpText<"Enable legacy character literals">;

def enable_objc_implicit_properties :
  Flag<["-"], "enable-objc-implicit-properties">,
  HelpText<"Import Objective-C \"implicit properties\" as properties">;

def strict_keyword_arguments :
  Flag<["-"], "strict-keyword-arguments">,
  HelpText<"Be strict about keyword argument presence/absence/ordering">;

def no_strict_keyword_arguments :
  Flag<["-"], "no-strict-keyword-arguments">,
  HelpText<"Don't be strict about keyword argument presence/absence/ordering">;

def enable_source_import : Flag<["-"], "enable-source-import">,
  HelpText<"Enable importing of Swift source files">;

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

def emit_sorted_sil : Flag<["-"], "emit-sorted-sil">,
  HelpText<"When printing SIL, print out all sil entities sorted by name to "
           "ease diffing">;

def use_malloc : Flag<["-"], "use-malloc">,
  HelpText<"Allocate internal data structures using malloc "
           "(for memory debugging)">;

} // end let Flags = [FrontendOption, NoDriverOption, HelpHidden]

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
    HelpText<"Output basic Make-compatible dependencies file to <path>">;

def emit_reference_dependencies : Flag<["-"], "emit-reference-dependencies">,
  HelpText<"Emit a Swift-style dependencies file">;
def emit_reference_dependencies_path
  : Separate<["-"], "emit-reference-dependencies-path">, MetaVarName<"<path>">,
    HelpText<"Output Swift-style dependencies file to <path>">;

def serialize_diagnostics_path
  : Separate<["-"], "serialize-diagnostics-path">, MetaVarName<"<path>">,
    HelpText<"Output serialized diagnostics to <path>">;

def verify : Flag<["-"], "verify">,
  HelpText<"Verify diagnostics against expected-{error|warning|note} "
           "annotations">;

def show_diagnostics_after_fatal : Flag<["-"], "show-diagnostics-after-fatal">,
  HelpText<"Keep emitting subsequent diagnostics after a fatal error">;

def enable_objc_interop :
  Flag<["-"], "enable-objc-interop">,
  HelpText<"Enable Objective-C interop code generation and config directives">;

def disable_objc_interop :
  Flag<["-"], "disable-objc-interop">,
  HelpText<"Disable Objective-C interop code generation and config directives">;

def enable_objc_attr_requires_foundation_module :
  Flag<["-"], "enable-objc-attr-requires-foundation-module">,
  HelpText<"Enable requiring uses of @objc to require importing the "
           "Foundation module">;

def disable_objc_attr_requires_foundation_module :
  Flag<["-"], "disable-objc-attr-requires-foundation-module">,
  HelpText<"Disable requiring uses of @objc to require importing the "
           "Foundation module">;

def print_clang_stats : Flag<["-"], "print-clang-stats">,
  HelpText<"Print Clang importer statistics">;

def serialize_debugging_options : Flag<["-"], "serialize-debugging-options">,
  HelpText<"Always serialize options for debugging (default: only for apps)">;

} // end let Flags = [FrontendOption, NoDriverOption]

def debug_crash_Group : OptionGroup<"<automatic crashing options>">;
class DebugCrashOpt : Group<debug_crash_Group>;

// HIDDEN FLAGS
let Flags = [FrontendOption, NoDriverOption, HelpHidden] in {

def debug_constraints : Flag<["-"], "debug-constraints">,
  HelpText<"Debug the constraint-based type checker">;

def debug_constraints_attempt : Separate<["-"], "debug-constraints-attempt">,
  HelpText<"Debug the constraint solver at a given attempt">;

def debug_generic_signatures : Flag<["-"], "debug-generic-signatures">,
  HelpText<"Debug generic signatures">;

def debug_forbid_typecheck_prefix : Separate<["-"], "debug-forbid-typecheck-prefix">,
  HelpText<"Triggers llvm fatal_error if typechecker tries to typecheck a decl "
           "with the provided prefix name">;

def debug_time_function_bodies : Flag<["-"], "debug-time-function-bodies">,
  HelpText<"Dumps the time it takes to type-check each function body">;

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

def disable_access_control : Flag<["-"], "disable-access-control">,
  HelpText<"Don't respect access control restrictions">;
def enable_access_control : Flag<["-"], "enable-access-control">,
  HelpText<"Respect access control restrictions">;

def disable_diagnostic_passes : Flag<["-"], "disable-diagnostic-passes">,
  HelpText<"Don't run diagnostic passes">;

def disable_llvm_optzns : Flag<["-"], "disable-llvm-optzns">,
  HelpText<"Don't run LLVM optimization passes">;

def disable_llvm_arc_opts : Flag<["-"], "disable-llvm-arc-opts">,
  HelpText<"Don't run LLVM ARC optimization passes.">;

def disable_llvm_slp_vectorizer : Flag<["-"], "disable-llvm-slp-vectorizer">,
  HelpText<"Don't run LLVM SLP vectorizer">;

def disable_llvm_verify : Flag<["-"], "disable-llvm-verify">,
  HelpText<"Don't run the LLVM IR verifier.">;

def disable_sil_linking : Flag<["-"], "disable-sil-linking">,
  HelpText<"Don't link SIL functions">;

def dump_clang_diagnostics : Flag<["-"], "dump-clang-diagnostics">,
  HelpText<"Dump Clang diagnostics to stderr">;

def emit_verbose_sil : Flag<["-"], "emit-verbose-sil">,
  HelpText<"Emit locations during SIL emission">;

def enable_dynamic_value_type_layout :
  Flag<["-"], "enable-dynamic-value-type-layout">,
  HelpText<"Enable experimental dynamic generic struct/enum/class type layout">;

def enable_experimental_patterns : Flag<["-"], "enable-experimental-patterns">,
  HelpText<"Enable experimental 'switch' pattern matching features">;

def enable_experimental_availability_checking : Flag<["-"],
  "enable-experimental-availability-checking">,
  HelpText<"Enable experimental checking for API availability">;

def enable_experimental_unavailable_as_optional : Flag<["-"],
  "enable-experimental-unavailable-as-optional">,
  HelpText<"Enable experimental treat unavailable symbols as optional">;

def enable_c_function_pointers : Flag<["-"],
  "enable-c-function-pointers">,
  HelpText<"Enable native C function pointer types">;

def enable_character_literals : Flag<["-"], "enable-character-literals">,
  HelpText<"Enable legacy character literals">;

def enable_objc_implicit_properties :
  Flag<["-"], "enable-objc-implicit-properties">,
  HelpText<"Import Objective-C \"implicit properties\" as properties">;

def enable_source_import : Flag<["-"], "enable-source-import">,
  HelpText<"Enable importing of Swift source files">;

def enable_testing : Flag<["-"], "enable-testing">,
  HelpText<"Allows this module's internal API to be accessed for testing">;

def import_module : Separate<["-"], "import-module">,
  HelpText<"Implicitly import the specified module">;

def print_stats : Flag<["-"], "print-stats">,
  HelpText<"Print various statistics">;

def playground : Flag<["-"], "playground">,
  HelpText<"Apply the playground semantics and transformation">;

def disable_playground_transform : Flag<["-"], "disable-playground-transform">,
  HelpText<"Disable playground transformation">;

def use_jit : Flag<["-"], "use-jit">,
  HelpText<"Register Objective-C classes as if the JIT were in use">;

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

def sil_print_pass_name : Flag<["-"], "sil-print-pass-name">,
  HelpText<"Print SIL pass name before each transform">;
  
def sil_time_transforms : Flag<["-"], "sil-time-transforms">,
  HelpText<"Time each SIL transform invocation">;

def sil_debug_serialization : Flag<["-"], "sil-debug-serialization">,
  HelpText<"Do not eliminate functions in Mandatory Inlining/SILCombine dead "
           "functions. (for debugging only)">;

def print_inst_counts : Flag<["-"], "print-inst-counts">,
  HelpText<"Before IRGen, count all the various SIL instructions. Must be used "
           "in conjunction with -print-stats.">;

def emit_sorted_sil : Flag<["-"], "emit-sorted-sil">,
  HelpText<"When printing SIL, print out all sil entities sorted by name to "
           "ease diffing">;

def use_malloc : Flag<["-"], "use-malloc">,
  HelpText<"Allocate internal data structures using malloc "
           "(for memory debugging)">;

def disable_func_sig_opts : Flag<["-"], "disable-func-sig-opts">,
  HelpText<"Disable function signature optimizations">;

def interpret : Flag<["-"], "interpret">, HelpText<"Immediate mode">, ModeOpt;

def verify_type_layout : JoinedOrSeparate<["-"], "verify-type-layout">,
  HelpText<"Verify compile-time and runtime type layout information for type">,
  MetaVarName<"<type>">;

def enable_guaranteed_self : Flag<["-"], "enable-guaranteed-self">,
  HelpText<"Enable the passing of self as a guaranteed parameter">;

def external_pass_pipeline_filename : Separate<["-"], "external-pass-pipeline-filename">,
    HelpText<"Use the pass pipeline defined by <pass_pipeline_file>">,
    MetaVarName<"<pass_pipeline_file>">;

} // end let Flags = [FrontendOption, NoDriverOption, HelpHidden]

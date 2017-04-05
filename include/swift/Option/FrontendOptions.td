//===--- FrontendOptions.td - Options for swift -frontend -----------------===//
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

def filelist : Separate<["-"], "filelist">,
  HelpText<"Specify source inputs in a file rather than on the command line">;
def output_filelist : Separate<["-"], "output-filelist">,
  HelpText<"Specify outputs in a file rather than on the command line">;

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

def emit_fixits_path
  : Separate<["-"], "emit-fixits-path">, MetaVarName<"<path>">,
    HelpText<"Output compiler fixits as source edits to <path>">;

def verify : Flag<["-"], "verify">,
  HelpText<"Verify diagnostics against expected-{error|warning|note} "
           "annotations">;
def verify_apply_fixes : Flag<["-"], "verify-apply-fixes">,
  HelpText<"Like -verify, but updates the original source file">;
def verify_ignore_unknown: Flag<["-"], "verify-ignore-unknown">,
  HelpText<"Allow diagnostics for '<unknown>' location in verify mode">;

def show_diagnostics_after_fatal : Flag<["-"], "show-diagnostics-after-fatal">,
  HelpText<"Keep emitting subsequent diagnostics after a fatal error">;

def enable_objc_interop :
  Flag<["-"], "enable-objc-interop">,
  HelpText<"Enable Objective-C interop code generation and config directives">;

def disable_objc_interop :
  Flag<["-"], "disable-objc-interop">,
  HelpText<"Disable Objective-C interop code generation and config directives">;

def enable_swiftcall : Flag<["-"], "enable-swiftcall">,
  HelpText<"Enable the use of LLVM swiftcall support">;

def enable_objc_attr_requires_foundation_module :
  Flag<["-"], "enable-objc-attr-requires-foundation-module">,
  HelpText<"Enable requiring uses of @objc to require importing the "
           "Foundation module">;

def disable_objc_attr_requires_foundation_module :
  Flag<["-"], "disable-objc-attr-requires-foundation-module">,
  HelpText<"Disable requiring uses of @objc to require importing the "
           "Foundation module">;

def enable_testable_attr_requires_testable_module :
  Flag<["-"], "enable-testable-attr-requires-testable-module">,
  HelpText<"Enable checking of @testable">;

def disable_testable_attr_requires_testable_module :
  Flag<["-"], "disable-testable-attr-requires-testable-module">,
  HelpText<"Disable checking of @testable">;

def enable_target_os_checking :
  Flag<["-"], "enable-target-os-checking">,
  HelpText<"Enable checking the target OS of serialized modules">;

def disable_target_os_checking :
  Flag<["-"], "disable-target-os-checking">,
  HelpText<"Disable checking the target OS of serialized modules">;

def enable_astscope_lookup : Flag<["-"], "enable-astscope-lookup">,
  HelpText<"Enable ASTScope-based unqualified name lookup">;

def print_clang_stats : Flag<["-"], "print-clang-stats">,
  HelpText<"Print Clang importer statistics">;

def serialize_debugging_options : Flag<["-"], "serialize-debugging-options">,
  HelpText<"Always serialize options for debugging (default: only for apps)">;

def autolink_library : Separate<["-"], "autolink-library">,
  HelpText<"Add dependent library">, Flags<[FrontendOption]>;

def disable_typo_correction : Flag<["-"], "disable-typo-correction">,
  HelpText<"Disable typo correction">;

def disable_serialization_nested_type_lookup_table :
  Flag<["-"], "disable-serialization-nested-type-lookup-table">,
  HelpText<"Force module merging to use regular lookups to find nested types">;

} // end let Flags = [FrontendOption, NoDriverOption]

def debug_crash_Group : OptionGroup<"<automatic crashing options>">;
class DebugCrashOpt : Group<debug_crash_Group>;

// HIDDEN FLAGS
let Flags = [FrontendOption, NoDriverOption, HelpHidden] in {

def debug_constraints : Flag<["-"], "debug-constraints">,
  HelpText<"Debug the constraint-based type checker">;

def debug_constraints_attempt : Separate<["-"], "debug-constraints-attempt">,
  HelpText<"Debug the constraint solver at a given attempt">;

def propagate_constraints : Flag<["-"], "propagate-constraints">,
  HelpText<"Enable constraint propagation in the type checker">;

def iterative_type_checker : Flag<["-"], "iterative-type-checker">,
  HelpText<"Enable the iterative type checker">;

def debug_generic_signatures : Flag<["-"], "debug-generic-signatures">,
  HelpText<"Debug generic signatures">;

def debug_forbid_typecheck_prefix : Separate<["-"], "debug-forbid-typecheck-prefix">,
  HelpText<"Triggers llvm fatal_error if typechecker tries to typecheck a decl "
           "with the provided prefix name">;

def debug_time_compilation : Flag<["-"], "debug-time-compilation">,
  HelpText<"Prints the time taken by each compilation phase">;
def debug_time_function_bodies : Flag<["-"], "debug-time-function-bodies">,
  HelpText<"Dumps the time it takes to type-check each function body">;
def debug_time_expression_type_checking : Flag<["-"], "debug-time-expression-type-checking">,
  HelpText<"Dumps the time it takes to type-check each expression">;

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

def enable_guaranteed_closure_contexts : Flag<["-"], "enable-guaranteed-closure-contexts">,
  HelpText<"Use @guaranteed convention for closure context">;

def disable_sil_partial_apply : Flag<["-"], "disable-sil-partial-apply">,
  HelpText<"Disable use of partial_apply in SIL generation">;

def remove_runtime_asserts : Flag<["-"], "remove-runtime-asserts">,
  HelpText<"Remove runtime asserts.">;

def disable_access_control : Flag<["-"], "disable-access-control">,
  HelpText<"Don't respect access control restrictions">;
def enable_access_control : Flag<["-"], "enable-access-control">,
  HelpText<"Respect access control restrictions">;

def code_complete_inits_in_postfix_expr : Flag<["-"], "code-complete-inits-in-postfix-expr">,
  HelpText<"Include initializers when completing a postfix expression">;

def disable_autolink_framework : Separate<["-"],"disable-autolink-framework">,
  HelpText<"Disable autolinking against the provided framework">;

def disable_diagnostic_passes : Flag<["-"], "disable-diagnostic-passes">,
  HelpText<"Don't run diagnostic passes">;

def disable_llvm_optzns : Flag<["-"], "disable-llvm-optzns">,
  HelpText<"Don't run LLVM optimization passes">;

def disable_sil_perf_optzns : Flag<["-"], "disable-sil-perf-optzns">,
  HelpText<"Don't run SIL performance optimization passes">;

def disable_llvm_arc_opts : Flag<["-"], "disable-llvm-arc-opts">,
  HelpText<"Don't run LLVM ARC optimization passes.">;

def disable_llvm_slp_vectorizer : Flag<["-"], "disable-llvm-slp-vectorizer">,
  HelpText<"Don't run LLVM SLP vectorizer">;

def disable_llvm_verify : Flag<["-"], "disable-llvm-verify">,
  HelpText<"Don't run the LLVM IR verifier.">;

def disable_llvm_value_names : Flag<["-"], "disable-llvm-value-names">,
  HelpText<"Don't add names to local values in LLVM IR">;

def enable_llvm_value_names : Flag<["-"], "enable-llvm-value-names">,
  HelpText<"Add names to local values in LLVM IR">;

def disable_reflection_metadata : Flag<["-"], "disable-reflection-metadata">,
  HelpText<"Disable emission of reflection metadata for nominal types">;

def disable_reflection_names : Flag<["-"], "disable-reflection-names">,
  HelpText<"Disable emission of names of stored properties and enum cases in"
           "reflection metadata">;

def stack_promotion_checks : Flag<["-"], "emit-stack-promotion-checks">,
  HelpText<"Emit runtime checks for correct stack promotion of objects.">;

def stack_promotion_limit : Separate<["-"], "stack-promotion-limit">,
  HelpText<"Limit the size of stack promoted objects to the provided number "
           "of bytes.">;

def disable_sil_linking : Flag<["-"], "disable-sil-linking">,
  HelpText<"Don't link SIL functions">;

def dump_clang_diagnostics : Flag<["-"], "dump-clang-diagnostics">,
  HelpText<"Dump Clang diagnostics to stderr">;

def disable_modules_validate_system_headers : Flag<["-"], "disable-modules-validate-system-headers">,
  HelpText<"Disable validating system headers in the Clang importer">;

def emit_verbose_sil : Flag<["-"], "emit-verbose-sil">,
  HelpText<"Emit locations during SIL emission">;

def emit_pch : Flag<["-"], "emit-pch">,
  HelpText<"Emit PCH for imported Objective-C header file">, ModeOpt;

def enable_sil_ownership : Flag<["-"], "enable-sil-ownership">,
  HelpText<"Enable the SIL Ownership Model">;

def enable_mandatory_semantic_arc_opts : Flag<["-"], "enable-mandatory-semantic-arc-opts">,
  HelpText<"Enable the mandatory semantic arc optimizer">;

def assume_parsing_unqualified_ownership_sil : Flag<["-"], "assume-parsing-unqualified-ownership-sil">,
  HelpText<"Assume unqualified SIL ownership when parsing SIL">;

def enable_sil_opaque_values : Flag<["-"], "enable-sil-opaque-values">,
  HelpText<"Enable SIL Opaque Values">;

def enable_experimental_property_behaviors :
  Flag<["-"], "enable-experimental-property-behaviors">,
  HelpText<"Enable experimental property behaviors">;

def enable_cow_existentials : Flag<["-"], "enable-cow-existentials">,
  HelpText<"Enable the copy-on-write existential implementation">;

def disable_availability_checking : Flag<["-"],
  "disable-availability-checking">,
  HelpText<"Disable checking for potentially unavailable APIs">;

def disable_tsan_inout_instrumentation : Flag<["-"],
  "disable-tsan-inout-instrumentation">,
  HelpText<"Disable treatment of inout parameters as Thread Sanitizer accesses">;

def enable_infer_import_as_member :
  Flag<["-"], "enable-infer-import-as-member">,
  HelpText<"Infer when a global could be imported as a member">;

def enable_swift3_objc_inference : Flag<["-"], "enable-swift3-objc-inference">,
  Flags<[FrontendOption, HelpHidden]>,
HelpText<"Enable Swift 3's @objc inference rules for NSObject-derived classes and 'dynamic' members (emulates Swift 3 behavior)">;

def disable_swift3_objc_inference :
  Flag<["-"], "disable-swift3-objc-inference">,
  Flags<[FrontendOption, HelpHidden]>,
  HelpText<"Disable Swift 3's @objc inference rules for NSObject-derived classes and 'dynamic' members (emulates Swift 4 behavior)">;

def warn_long_function_bodies : Separate<["-"], "warn-long-function-bodies">,
  MetaVarName<"<n>">,
  HelpText<"Warns when type-checking a function takes longer than <n> ms">;
def warn_long_function_bodies_EQ : Joined<["-"], "warn-long-function-bodies=">,
  Alias<warn_long_function_bodies>;

def enable_source_import : Flag<["-"], "enable-source-import">,
  HelpText<"Enable importing of Swift source files">;

def enable_throw_without_try : Flag<["-"], "enable-throw-without-try">,
  HelpText<"Allow throwing function calls without 'try'">;

def import_module : Separate<["-"], "import-module">,
  HelpText<"Implicitly import the specified module">;

def print_stats : Flag<["-"], "print-stats">,
  HelpText<"Print various statistics">;

def playground : Flag<["-"], "playground">,
  HelpText<"Apply the playground semantics and transformation">;

def playground_high_performance : Flag<["-"], "playground-high-performance">,
  HelpText<"Omit instrumentation that has a high runtime performance impact">;

def disable_playground_transform : Flag<["-"], "disable-playground-transform">,
  HelpText<"Disable playground transformation">;

def pc_macro : Flag<["-"], "pc-macro">,
  HelpText<"Apply the 'program counter simulation' macro">;

def use_jit : Flag<["-"], "use-jit">,
  HelpText<"Register Objective-C classes as if the JIT were in use">;

def sil_inline_threshold : Separate<["-"], "sil-inline-threshold">,
  MetaVarName<"<50>">,
  HelpText<"Controls the aggressiveness of performance inlining">;

def sil_merge_partial_modules : Flag<["-"], "sil-merge-partial-modules">,
  HelpText<"Merge SIL from all partial swiftmodules into the final module">;

def sil_link_all : Flag<["-"], "sil-link-all">,
  HelpText<"Link all SIL functions">;

def sil_serialize_all : Flag<["-"], "sil-serialize-all">,
  HelpText<"Serialize all generated SIL">;

def sil_verify_all : Flag<["-"], "sil-verify-all">,
  HelpText<"Verify SIL after each transform">;

def sil_debug_serialization : Flag<["-"], "sil-debug-serialization">,
  HelpText<"Do not eliminate functions in Mandatory Inlining/SILCombine dead "
           "functions. (for debugging only)">;

def print_inst_counts : Flag<["-"], "print-inst-counts">,
  HelpText<"Before IRGen, count all the various SIL instructions. Must be used "
           "in conjunction with -print-stats.">;

def debug_on_sil : Flag<["-"], "gsil">,
  HelpText<"Write the SIL into a file and generate debug-info to debug on SIL "
           " level.">;

def print_llvm_inline_tree : Flag<["-"], "print-llvm-inline-tree">,
  HelpText<"Print the LLVM inline tree.">;

def disable_incremental_llvm_codegeneration :
  Flag<["-"], "disable-incremental-llvm-codegen">,
       HelpText<"Disable incremental llvm code generation.">;

def emit_sorted_sil : Flag<["-"], "emit-sorted-sil">,
  HelpText<"When printing SIL, print out all sil entities sorted by name to "
           "ease diffing">;

def use_malloc : Flag<["-"], "use-malloc">,
  HelpText<"Allocate internal data structures using malloc "
           "(for memory debugging)">;

def interpret : Flag<["-"], "interpret">, HelpText<"Immediate mode">, ModeOpt;

def verify_type_layout : JoinedOrSeparate<["-"], "verify-type-layout">,
  HelpText<"Verify compile-time and runtime type layout information for type">,
  MetaVarName<"<type>">;

def external_pass_pipeline_filename : Separate<["-"], "external-pass-pipeline-filename">,
    HelpText<"Use the pass pipeline defined by <pass_pipeline_file>">,
    MetaVarName<"<pass_pipeline_file>">;

def dump_interface_hash : Flag<["-"], "dump-interface-hash">,
   HelpText<"Parse input file(s) and dump interface token hash(es)">,
   ModeOpt;

def dump_api_path : Separate<["-"], "dump-api-path">,
  HelpText<"The path to output swift interface files for the compiled source files">;

def enable_resilience : Flag<["-"], "enable-resilience">,
   HelpText<"Compile the module to export resilient interfaces for all "
            "public declarations by default">;

def enable_class_resilience : Flag<["-"], "enable-class-resilience">,
   HelpText<"Compile the module to export resilient interfaces for all "
            "public classes by default (doesn't work yet)">;

def group_info_path : Separate<["-"], "group-info-path">,
  HelpText<"The path to collect the group information of the compiled module">;

def diagnostics_editor_mode : Flag<["-"], "diagnostics-editor-mode">,
HelpText<"Diagnostics will be used in editor">;

def validate_tbd_against_ir: Flag<["-"], "validate-tbd-against-ir">,
    HelpText<"Compare the symbols in the IR against the TBD file that would be generated.">;

} // end let Flags = [FrontendOption, NoDriverOption, HelpHidden]

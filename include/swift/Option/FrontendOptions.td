//===--- FrontendOptions.td - Options for swift -frontend -----------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2018 Apple Inc. and the Swift project authors
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

def primary_file : Separate<["-"], "primary-file">,
  HelpText<"Produce output for this file, not the whole module">;

def filelist : Separate<["-"], "filelist">,
  HelpText<"Specify source inputs in a file rather than on the command line">;
def primary_filelist : Separate<["-"], "primary-filelist">,
  HelpText<"Specify primary inputs in a file rather than on the command line">;
def output_filelist : Separate<["-"], "output-filelist">,
  HelpText<"Specify outputs in a file rather than on the command line">;
def supplementary_output_file_map : Separate<["-"], "supplementary-output-file-map">,
  HelpText<"Specify supplementary outputs in a file rather than on the command line">;


def emit_module_doc : Flag<["-"], "emit-module-doc">,
  HelpText<"Emit a module documentation file based on documentation "
           "comments">;
def emit_module_doc_path
  : Separate<["-"], "emit-module-doc-path">, MetaVarName<"<path>">,
    HelpText<"Output module documentation file <path>">;

def merge_modules
  : Flag<["-"], "merge-modules">, ModeOpt,
    HelpText<"Merge the input modules without otherwise processing them">;

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

// For transition purposes.
def emit_interface_path
  : Separate<["-"], "emit-interface-path">,
    Alias<emit_parseable_module_interface_path>;

def tbd_install_name
  : Separate<["-"], "tbd-install_name">, MetaVarName<"<path>">,
    HelpText<"The install_name to use in an emitted TBD file">;

def tbd_install_name_EQ : Joined<["-"], "tbd-install_name=">,
  Alias<tbd_install_name>;

def tbd_current_version
  : Separate<["-"], "tbd-current-version">, MetaVarName<"<version>">,
  HelpText<"The current_version to use in an emitted TBD file">;

def tbd_current_version_EQ : Joined<["-"], "tbd-current-version=">,
  Alias<tbd_current_version>;

def tbd_compatibility_version
  : Separate<["-"], "tbd-compatibility-version">, MetaVarName<"<version>">,
  HelpText<"The compatibility_version to use in an emitted TBD file">;

def tbd_compatibility_version_EQ : Joined<["-"], "tbd-compatibility-version=">,
  Alias<tbd_compatibility_version>;

def verify : Flag<["-"], "verify">,
  HelpText<"Verify diagnostics against expected-{error|warning|note} "
           "annotations">;
def verify_apply_fixes : Flag<["-"], "verify-apply-fixes">,
  HelpText<"Like -verify, but updates the original source file">;
def verify_ignore_unknown: Flag<["-"], "verify-ignore-unknown">,
  HelpText<"Allow diagnostics for '<unknown>' location in verify mode">;
def verify_generic_signatures : Separate<["-"], "verify-generic-signatures">,
  MetaVarName<"<module-name>">,
  HelpText<"Verify the generic signatures in the given module">;

def verify_syntax_tree : Flag<["-"], "verify-syntax-tree">,
  HelpText<"Verify that no unknown nodes exist in the libSyntax tree">;

def show_diagnostics_after_fatal : Flag<["-"], "show-diagnostics-after-fatal">,
  HelpText<"Keep emitting subsequent diagnostics after a fatal error">;

def enable_swiftcall : Flag<["-"], "enable-swiftcall">,
  HelpText<"Enable the use of LLVM swiftcall support">;

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


// Flags that are saved into textual interfaces
let Flags = [FrontendOption, NoDriverOption, HelpHidden, TextualInterfaceOption] in {

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

def enable_resilience : Flag<["-"], "enable-resilience">,
   HelpText<"Compile the module to export resilient interfaces for all "
            "public declarations by default">;

def enable_class_resilience : Flag<["-"], "enable-class-resilience">,
   HelpText<"Enable resilient layout for classes containing resilient value types">;

def read_type_info_path_EQ : Joined<["-"], "read-type-info-path=">,
  HelpText<"Read legacy type layout from the given path">;
}


// HIDDEN FLAGS
let Flags = [FrontendOption, NoDriverOption, HelpHidden] in {

def debug_constraints : Flag<["-"], "debug-constraints">,
  HelpText<"Debug the constraint-based type checker">;

def debug_constraints_attempt : Separate<["-"], "debug-constraints-attempt">,
  HelpText<"Debug the constraint solver at a given attempt">;

def disable_named_lazy_member_loading : Flag<["-"], "disable-named-lazy-member-loading">,
  HelpText<"Disable per-name lazy member loading">;

def debug_generic_signatures : Flag<["-"], "debug-generic-signatures">,
  HelpText<"Debug generic signatures">;

def debug_forbid_typecheck_prefix : Separate<["-"], "debug-forbid-typecheck-prefix">,
  HelpText<"Triggers llvm fatal_error if typechecker tries to typecheck a decl "
           "with the provided prefix name">;

def debug_cycles : Flag<["-"], "debug-cycles">,
  HelpText<"Print out debug dumps when cycles are detected in evaluation">;
def output_request_graphviz : Separate<["-"], "output-request-graphviz">,
  HelpText<"Emit GraphViz output visualizing the request graph">;

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

def disable_sil_partial_apply : Flag<["-"], "disable-sil-partial-apply">,
  HelpText<"Disable use of partial_apply in SIL generation">;

def disable_access_control : Flag<["-"], "disable-access-control">,
  HelpText<"Don't respect access control restrictions">;
def enable_access_control : Flag<["-"], "enable-access-control">,
  HelpText<"Respect access control restrictions">;

def code_complete_inits_in_postfix_expr : Flag<["-"], "code-complete-inits-in-postfix-expr">,
  HelpText<"Include initializers when completing a postfix expression">;
def code_complete_call_pattern_heuristics : Flag<["-"], "code-complete-call-pattern-heuristics">,
  HelpText<"Use heuristics to guess whether we want call pattern completions">;

def disable_autolink_framework : Separate<["-"],"disable-autolink-framework">,
  HelpText<"Disable autolinking against the provided framework">;

def disable_diagnostic_passes : Flag<["-"], "disable-diagnostic-passes">,
  HelpText<"Don't run diagnostic passes">;

def disable_llvm_optzns : Flag<["-"], "disable-llvm-optzns">,
  HelpText<"Don't run LLVM optimization passes">;

def disable_sil_perf_optzns : Flag<["-"], "disable-sil-perf-optzns">,
  HelpText<"Don't run SIL performance optimization passes">;

def disable_swift_specific_llvm_optzns : Flag<["-"], "disable-swift-specific-llvm-optzns">,
  HelpText<"Don't run Swift specific LLVM optimization passes.">;

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

def dump_clang_diagnostics : Flag<["-"], "dump-clang-diagnostics">,
  HelpText<"Dump Clang diagnostics to stderr">;

def disable_modules_validate_system_headers : Flag<["-"], "disable-modules-validate-system-headers">,
  HelpText<"Disable validating system headers in the Clang importer">;

def emit_verbose_sil : Flag<["-"], "emit-verbose-sil">,
  HelpText<"Emit locations during SIL emission">;

def emit_pch : Flag<["-"], "emit-pch">,
  HelpText<"Emit PCH for imported Objective-C header file">, ModeOpt;

def pch_disable_validation : Flag<["-"], "pch-disable-validation">,
  HelpText<"Disable validating the persistent PCH">;

def enable_sil_ownership : Flag<["-"], "enable-sil-ownership">,
  HelpText<"Enable the SIL Ownership Model">;

def disable_guaranteed_normal_arguments : Flag<["-"], "disable-guaranteed-normal-arguments">,
  HelpText<"If set to true, all normal parameters (except to inits/setters) will be passed at +1">;

def disable_mandatory_semantic_arc_opts : Flag<["-"], "disable-mandatory-semantic-arc-opts">,
  HelpText<"Disable the mandatory semantic arc optimizer">;

def assume_parsing_unqualified_ownership_sil : Flag<["-"], "assume-parsing-unqualified-ownership-sil">,
  HelpText<"Assume unqualified SIL ownership when parsing SIL">;

def suppress_static_exclusivity_swap : Flag<["-"], "suppress-static-exclusivity-swap">,
  HelpText<"Suppress static violations of exclusive access with swap()">;

def enable_sil_opaque_values : Flag<["-"], "enable-sil-opaque-values">,
  HelpText<"Enable SIL Opaque Values">;
  
def enable_large_loadable_types : Flag<["-"], "enable-large-loadable-types">,
  HelpText<"Enable Large Loadable types IRGen pass">;

def enable_experimental_property_behaviors :
  Flag<["-"], "enable-experimental-property-behaviors">,
  HelpText<"Enable experimental property behaviors">;
  
def enable_deserialization_recovery :
  Flag<["-"], "enable-deserialization-recovery">,
  HelpText<"Attempt to recover from missing xrefs (etc) in swiftmodules">;
def disable_deserialization_recovery :
  Flag<["-"], "disable-deserialization-recovery">,
  HelpText<"Don't attempt to recover from missing xrefs (etc) in swiftmodules">;

def disable_availability_checking : Flag<["-"],
  "disable-availability-checking">,
  HelpText<"Disable checking for potentially unavailable APIs">;

def disable_tsan_inout_instrumentation : Flag<["-"],
  "disable-tsan-inout-instrumentation">,
  HelpText<"Disable treatment of inout parameters as Thread Sanitizer accesses">;

def report_errors_to_debugger : Flag<["-"], "report-errors-to-debugger">,
  HelpText<"Deprecated, will be removed in future versions.">;

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

def enable_nskeyedarchiver_diagnostics :
  Flag<["-"], "enable-nskeyedarchiver-diagnostics">,
  HelpText<"Diagnose classes with unstable mangled names adopting NSCoding">;
def disable_nskeyedarchiver_diagnostics :
  Flag<["-"], "disable-nskeyedarchiver-diagnostics">,
  HelpText<"Allow classes with unstable mangled names to adopt NSCoding">;

def enable_nonfrozen_enum_exhaustivity_diagnostics :
  Flag<["-"], "enable-nonfrozen-enum-exhaustivity-diagnostics">,
  HelpText<"Diagnose switches over non-frozen enums without catch-all cases">;
def disable_nonfrozen_enum_exhaustivity_diagnostics :
  Flag<["-"], "disable-nonfrozen-enum-exhaustivity-diagnostics">,
  HelpText<"Allow switches over non-frozen enums without catch-all cases">;

def warn_long_function_bodies : Separate<["-"], "warn-long-function-bodies">,
  MetaVarName<"<n>">,
  HelpText<"Warns when type-checking a function takes longer than <n> ms">;
def warn_long_function_bodies_EQ : Joined<["-"], "warn-long-function-bodies=">,
  Alias<warn_long_function_bodies>;

def warn_long_expression_type_checking : Separate<["-"], "warn-long-expression-type-checking">,
  MetaVarName<"<n>">,
  HelpText<"Warns when type-checking a function takes longer than <n> ms">;
def warn_long_expression_type_checking_EQ : Joined<["-"], "warn-long-expression-type-checking=">,
  Alias<warn_long_expression_type_checking>;

def solver_expression_time_threshold_EQ : Joined<["-"], "solver-expression-time-threshold=">;

def solver_disable_shrink :
  Flag<["-"], "solver-disable-shrink">,
  HelpText<"Disable the shrink phase of expression type checking">;

def disable_constraint_solver_performance_hacks : Flag<["-"], "disable-constraint-solver-performance-hacks">,
  HelpText<"Disable all the hacks in the constraint solver">;

def enable_operator_designated_types :
  Flag<["-"], "enable-operator-designated-types">,
  HelpText<"Enable operator designated types">;

def solver_enable_operator_designated_types :
  Flag<["-"], "solver-enable-operator-designated-types">,
  HelpText<"Enable operator designated types in constraint solver">;

def switch_checking_invocation_threshold_EQ : Joined<["-"],
    "switch-checking-invocation-threshold=">;

def enable_source_import : Flag<["-"], "enable-source-import">,
  HelpText<"Enable importing of Swift source files">;

def enable_throw_without_try : Flag<["-"], "enable-throw-without-try">,
  HelpText<"Allow throwing function calls without 'try'">;

def import_module : Separate<["-"], "import-module">,
  HelpText<"Implicitly import the specified module">;

def print_stats : Flag<["-"], "print-stats">,
  HelpText<"Print various statistics">;

def debugger_testing_transform : Flag<["-"], "debugger-testing-transform">,
  HelpText<"Instrument the code with calls to an intrinsic that record the expected values of "
           "local variables so they can be compared against the results from the debugger.">;

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

def sil_inline_caller_benefit_reduction_factor : Separate<["-"], "sil-inline-caller-benefit-reduction-factor">,
  MetaVarName<"<2>">,
  HelpText<"Controls the aggressiveness of performance inlining in -Osize "
          "mode by reducing the base benefits of a caller (lower value "
          "permits more inlining!)">;

def sil_unroll_threshold : Separate<["-"], "sil-unroll-threshold">,
  MetaVarName<"<250>">,
  HelpText<"Controls the aggressiveness of loop unrolling">;

def sil_existential_specializer : Flag<["-"], "sil-existential-specializer">,
  HelpText<"Enable SIL existential specializer optimization">;

def sil_merge_partial_modules : Flag<["-"], "sil-merge-partial-modules">,
  HelpText<"Merge SIL from all partial swiftmodules into the final module">;

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

def emit_syntax : Flag<["-"], "emit-syntax">,
  HelpText<"Parse input file(s) and emit the Syntax tree(s) as JSON">, ModeOpt;

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

def index_system_modules : Flag<["-"], "index-system-modules">,
  HelpText<"Emit index data for imported serialized swift system modules">;

def dump_interface_hash : Flag<["-"], "dump-interface-hash">,
   HelpText<"Parse input file(s) and dump interface token hash(es)">,
   ModeOpt;

def enable_resilience_bypass : Flag<["-"], "enable-resilience-bypass">,
   HelpText<"Completely bypass resilience when accessing types in resilient frameworks">;

def dump_api_path : Separate<["-"], "dump-api-path">,
  HelpText<"The path to output swift interface files for the compiled source files">;

def group_info_path : Separate<["-"], "group-info-path">,
  HelpText<"The path to collect the group information of the compiled module">;

def diagnostics_editor_mode : Flag<["-"], "diagnostics-editor-mode">,
HelpText<"Diagnostics will be used in editor">;

def validate_tbd_against_ir_EQ: Joined<["-"], "validate-tbd-against-ir=">,
    HelpText<"Compare the symbols in the IR against the TBD file that would be generated.">,
    MetaVarName<"<level>">;

// FIXME: Remove this flag when void subscripts are implemented.
// This is used to guard preemptive testing for the fix-it.
def fix_string_substring_conversion: Flag<["-"], "fix-string-substring-conversion">,
    HelpText<"Emit a fix-it to append '[]' to String expressions when converting to Substring.">;
    
def bypass_batch_mode_checks: Flag<["-"], "bypass-batch-mode-checks">,
    HelpText<"Bypass checks for batch-mode errors.">;

def enable_verify_exclusivity : Flag<["-"], "enable-verify-exclusivity">,
  HelpText<"Enable verification of access markers used to enforce exclusivity.">;

def disable_verify_exclusivity : Flag<["-"], "disable-verify-exclusivity">,
  HelpText<"Diable verification of access markers used to enforce exclusivity.">;

def enable_key_path_resilience : Flag<["-"], "enable-key-path-resilience">,
  HelpText<"Enable key path resilience.">;

def type_info_dump_filter_EQ : Joined<["-"], "type-info-dump-filter=">,
  Flags<[FrontendOption]>,
  HelpText<"One of 'all', 'resilient' or 'fragile'">;

} // end let Flags = [FrontendOption, NoDriverOption, HelpHidden]

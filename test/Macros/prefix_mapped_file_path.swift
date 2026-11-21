// REQUIRES: swift_swift_parser
// UNSUPPORTED: OS=windows-msvc

// RUN: %empty-directory(%t)
// RUN: %host-build-swift -swift-version 5 -emit-library -o %t/%target-library-name(MacroDefinition) -module-name=MacroDefinition %S/Inputs/syntax_macro_definitions.swift -g -no-toolchain-stdlib-rpath

/// A '#file' produced by a macro expansion evaluates to the path of the file
/// containing the macro use.
// RUN: %target-swift-frontend -typecheck -swift-version 5 -diagnostic-style llvm \
// RUN:   -load-plugin-library %t/%target-library-name(MacroDefinition) %s \
// RUN:   -Wwarning PrefixMappedFilePath -cache-replay-prefix-map %S /original 2>&1 | %FileCheck %s

@freestanding(expression)
macro MagicFile() -> String = #externalMacro(module: "MacroDefinition", type: "MagicFileMacro")

// CHECK: warning: '#file' evaluates to the prefix-mapped path '{{.*}}prefix_mapped_file_path.swift', not the original path of the source file
_ = #MagicFile

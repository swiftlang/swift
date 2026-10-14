// REQUIRES: swift_swift_parser

// This test ensures that checking a macro-generated declaration with a unique
// name for redeclarations doesn't expand the freestanding macros in other
// files. Unique names can't collide, and looking one up at module scope would
// otherwise resolve and expand every top-level freestanding declaration macro
// in the module, type-checking each one's arguments.

// RUN: %empty-directory(%t)
// RUN: %empty-directory(%t/stats)
// RUN: split-file %s %t

// RUN: %host-build-swift -swift-version 5 -emit-library -o %t/%target-library-name(MacroDefinition) -parse-as-library -module-name=MacroDefinition %S/Inputs/syntax_macro_definitions.swift -g -no-toolchain-stdlib-rpath

// RUN: %target-swift-frontend -typecheck -swift-version 5 -load-plugin-library %t/%target-library-name(MacroDefinition) -stats-output-dir %t/stats -fine-grained-timers -print-zero-stats -primary-file %t/a.swift %t/b.swift %t/c.swift

// Only the primary file's macro should be resolved and expanded.
// RUN: %{python} %utils/process-stats-dir.py --evaluate 'ResolveMacroRequest == 1' %t/stats
// RUN: %{python} %utils/process-stats-dir.py --evaluate 'ExpandMacroExpansionDeclRequest == 1' %t/stats

//--- a.swift

@freestanding(declaration)
macro anonymousTypes(_: () -> String) = #externalMacro(module: "MacroDefinition", type: "DefineAnonymousTypesMacro")

#anonymousTypes { "a" }

//--- b.swift

#anonymousTypes { "b" }

//--- c.swift

#anonymousTypes { "c" }

// REQUIRES: swift_swift_parser

// RUN: %empty-directory(%t)
// RUN: %host-build-swift -swift-version 5 -emit-library -o %t/%target-library-name(MacroDefinition) -module-name=MacroDefinition %S/../Macros/Inputs/syntax_macro_definitions.swift -g -no-toolchain-stdlib-rpath

// RUN: %target-swift-emit-silgen-ossa -o /dev/null -enable-sil-opaque-values -load-plugin-library %t/%target-library-name(MacroDefinition) -primary-file %s %S/Inputs/accessor_macro_multifile_other.swift -module-name AccMacroMulti
// RUN: %target-swift-emit-silgen -load-plugin-library %t/%target-library-name(MacroDefinition) -primary-file %s %S/Inputs/accessor_macro_multifile_other.swift -module-name AccMacroMulti | %FileCheck %s

extension Holder {
  public init(action f: () -> Void) {}
}

// CHECK-LABEL: sil [ossa] @$s13AccMacroMulti6HolderV6actionACyyXE_tcfC :
// CHECK-NOT:     struct_element_addr {{.*}} #Holder.foo
// CHECK:       } // end sil function '$s13AccMacroMulti6HolderV6actionACyyXE_tcfC'

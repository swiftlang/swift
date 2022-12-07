macro stringify<T>(_ value: T) -> (T, String) = MacroDefinition.StringifyMacro

func testStringify(a: Int, b: Int) {
  _ = #stringify(a + b)
}

// FIXME: Swift parser is not enabled on Linux CI yet.
// REQUIRES: OS=macosx

// REQUIRES=shell

// RUN: %empty-directory(%t)

//##-- Prepare the macro plugin.
// RUN: %target-build-swift -I %swift-host-lib-dir -L %swift-host-lib-dir -emit-library -o %t/%target-library-name(MacroDefinition) -module-name=MacroDefinition %S/../../Macros/Inputs/syntax_macro_definitions.swift -g -no-toolchain-stdlib-rpath

// RUN: COMPILER_ARGS=( \
// RUN:   -enable-experimental-feature Macros \
// RUN:   -load-plugin-library %t/%target-library-name(MacroDefinition) \
// RUN:   -I %swift-host-lib-dir \
// RUN:   -module-name MacroUser \
// RUN:   %s \
// RUN: )

//##-- cursor-info at '#' position.
// RUN: %sourcekitd-test -req=cursor -pos=4:7 -cursor-action %s -- ${COMPILER_ARGS[@]} | %FileCheck -check-prefix=CURSOR_POUND %s

// CURSOR_POUND-LABEL: ACTIONS BEGIN
// CURSOR_POUND: source.refactoring.kind.expand.macro
// CURSOR_POUND-NEXT: Expand Macro
// CURSOR_POUND: ACTIONS END

//##-- cursor-info at 'stringify' position.
// RUN: %sourcekitd-test -req=cursor -pos=4:8 -cursor-action %s -- ${COMPILER_ARGS[@]} | %FileCheck -check-prefix=CURSOR_MACRONAME %s

// CURSOR_MACRONAME: source.lang.swift.ref.macro (1:7-1:16)
// CURSOR_MACRONAME: (T) -> (T, String)
// CURSOR_MACRONAME: <Declaration>macro stringify&lt;T&gt;(_ value: <Type usr="s:9MacroUser1TL_xmfp">T</Type>) -&gt; (<Type usr="s:9MacroUser1TL_xmfp">T</Type>, <Type usr="s:SS">String</Type>) = MacroDefinition.StringifyMacro</Declaration>
// CURSOR_MACRONAME: <decl.macro><syntaxtype.keyword>macro</syntaxtype.keyword> <decl.name>stringify</decl.name>&lt;<decl.generic_type_param usr="s:9MacroUser1TL_xmfp"><decl.generic_type_param.name>T</decl.generic_type_param.name></decl.generic_type_param>&gt;(<decl.var.parameter><decl.var.parameter.argument_label>_</decl.var.parameter.argument_label> <decl.var.parameter.name>value</decl.var.parameter.name>: <decl.var.parameter.type><ref.generic_type_param usr="s:9MacroUser1TL_xmfp">T</ref.generic_type_param></decl.var.parameter.type></decl.var.parameter>) -&gt; <decl.function.returntype><tuple>(<tuple.element><tuple.element.type><ref.generic_type_param usr="s:9MacroUser1TL_xmfp">T</ref.generic_type_param></tuple.element.type></tuple.element>, <tuple.element><tuple.element.type><ref.struct usr="s:SS">String</ref.struct></tuple.element.type></tuple.element>)</tuple></decl.function.returntype> = MacroDefinition.StringifyMacro</decl.macro>
// CURSOR_MACRONAME-LABEL: ACTIONS BEGIN
// CURSOR_MACRONAME: source.refactoring.kind.rename.global
// CURSOR_MACRONAME-NEXT: Global Rename
// CURSOR_MACRONAME: source.refactoring.kind.expand.macro
// CURSOR_MACRONAME-NEXT: Expand Macro
// CURSOR_MACRONAME: ACTIONS END

//##-- Refactoring at both position.
// RUN: %sourcekitd-test -req=refactoring.expand.macro -pos=4:7 %s -- ${COMPILER_ARGS[@]} | %FileCheck -check-prefix=EXPAND %s
// RUN: %sourcekitd-test -req=refactoring.expand.macro -pos=4:8 %s -- ${COMPILER_ARGS[@]} | %FileCheck -check-prefix=EXPAND %s

// EXPAND: source.edit.kind.active:
// EXPAND-NEXT: 4:7-4:24 "(a + b, "a + b")"


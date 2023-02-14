@freestanding(expression) macro stringify<T>(_ value: T) -> (T, String) = #externalMacro(module: "MacroDefinition", type: "StringifyMacro")

func testStringify(a: Int, b: Int) {
  _ = #stringify(a + b)
}

@attached(memberAttribute)
@attached(member)
macro myTypeWrapper() = #externalMacro(module: "MacroDefinition", type: "TypeWrapperMacro")
@attached(accessor) macro accessViaStorage() = #externalMacro(module: "MacroDefinition", type: "AccessViaStorageMacro")

struct _Storage {
  var x: Int = 0 {
    willSet { print("setting \(newValue)") }
  }
  var y: Int = 0 {
    willSet { print("setting \(newValue)") }
  }
}

@myTypeWrapper
struct S {
  var x: Int
  var y: Int
}

struct S2 {
  private var _storage = _Storage()

  @accessViaStorage
  var x: Int

  @accessViaStorage
  var y: Int = 17
}

@attached(peer)
macro addCompletionHandler() = #externalMacro(module: "MacroDefinition", type: "AddCompletionHandler")

@available(macOS 10.15, *)
struct S3 {
  @addCompletionHandler
  func f(a: Int, for b: String, _ value: Double) async -> String {
    return b
  }
}

// FIXME: Swift parser is not enabled on Linux CI yet.
// REQUIRES: OS=macosx

// REQUIRES: executable_test

// REQUIRES=shell

// RUN: %empty-directory(%t)

//##-- Prepare the macro plugin.
// RUN: %target-build-swift -swift-version 5 -I %swift-host-lib-dir -L %swift-host-lib-dir -emit-library -o %t/%target-library-name(MacroDefinition) -module-name=MacroDefinition %S/../../Macros/Inputs/syntax_macro_definitions.swift -g -no-toolchain-stdlib-rpath

// RUN: COMPILER_ARGS=( \
// RUN:   -swift-version 5 \
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

// CURSOR_MACRONAME: source.lang.swift.ref.macro (1:33-1:42)
// CURSOR_MACRONAME: (T) -> (T, String)
// CURSOR_MACRONAME: <Declaration>@freestanding(expression) macro stringify&lt;T&gt;(_ value: <Type usr="s:9MacroUser1TL_xmfp">T</Type>) -&gt; (<Type usr="s:9MacroUser1TL_xmfp">T</Type>, <Type usr="s:SS">String</Type>) = #externalMacro(module: &quot;MacroDefinition&quot;, type: &quot;StringifyMacro&quot;)</Declaration>
// CURSOR_MACRONAME: <decl.macro><syntaxtype.attribute.builtin><syntaxtype.attribute.name>@freestanding</syntaxtype.attribute.name>(expression)</syntaxtype.attribute.builtin> <syntaxtype.keyword>macro</syntaxtype.keyword> <decl.name>stringify</decl.name>&lt;<decl.generic_type_param usr="s:9MacroUser1TL_xmfp"><decl.generic_type_param.name>T</decl.generic_type_param.name></decl.generic_type_param>&gt;(<decl.var.parameter><decl.var.parameter.argument_label>_</decl.var.parameter.argument_label> <decl.var.parameter.name>value</decl.var.parameter.name>: <decl.var.parameter.type><ref.generic_type_param usr="s:9MacroUser1TL_xmfp">T</ref.generic_type_param></decl.var.parameter.type></decl.var.parameter>) -&gt; <decl.function.returntype><tuple>(<tuple.element><tuple.element.type><ref.generic_type_param usr="s:9MacroUser1TL_xmfp">T</ref.generic_type_param></tuple.element.type></tuple.element>, <tuple.element><tuple.element.type><ref.struct usr="s:SS">String</ref.struct></tuple.element.type></tuple.element>)</tuple></decl.function.returntype> = #externalMacro(module: &quot;MacroDefinition&quot;, type: &quot;StringifyMacro&quot;)</decl.macro>
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

//##-- cursor-info at 'macro name' position following @.
// RUN: %sourcekitd-test -req=cursor -pos=21:2 -cursor-action %s -- ${COMPILER_ARGS[@]} | %FileCheck -check-prefix=CURSOR_ATTACHED_MACRO %s

// CURSOR_ATTACHED_MACRO-LABEL: ACTIONS BEGIN
// CURSOR_ATTACHED_MACRO: source.refactoring.kind.expand.macro
// CURSOR_ATTACHED_MACRO-NEXT: Expand Macro
// CURSOR_ATTACHED_MACRO: ACTIONS END

//##-- Refactoring expanding the attached macro
// RUN: %sourcekitd-test -req=refactoring.expand.macro -pos=21:2 %s -- ${COMPILER_ARGS[@]} | %FileCheck -check-prefix=ATTACHED_EXPAND %s
// ATTACHED_EXPAND: source.edit.kind.active:
// ATTACHED_EXPAND:   23:3-23:3 "@accessViaStorage "
// ATTACHED_EXPAND: source.edit.kind.active:
// ATTACHED_EXPAND:   24:3-24:3 "@accessViaStorage "
// ATTACHED_EXPAND: source.edit.kind.active:
// ATTACHED_EXPAND:   22:11-22:11 "
// ATTACHED_EXPAND: private var _storage = _Storage()
// ATTACHED_EXPAND: source.edit.kind.active:
// ATTACHED_EXPAND:   21:1-21:15 ""

//##-- Refactoring expanding the first accessor macro
// RUN: %sourcekitd-test -req=refactoring.expand.macro -pos=30:4 %s -- ${COMPILER_ARGS[@]} | %FileCheck -check-prefix=ACCESSOR1_EXPAND %s
// ACCESSOR1_EXPAND: source.edit.kind.active:
// ACCESSOR1_EXPAND:   31:13-31:13 "{
// ACCESSOR1_EXPAND:  get { _storage.x }
// ACCESSOR1_EXPAND:  set { _storage.x = newValue }
// ACCESSOR1_EXPAND: }"
// ACCESSOR1_EXPAND: source.edit.kind.active:
// ACCESSOR1_EXPAND:   30:3-30:20 ""

//##-- Refactoring expanding the second accessor macro
// RUN: %sourcekitd-test -req=refactoring.expand.macro -pos=33:13 %s -- ${COMPILER_ARGS[@]} | %FileCheck -check-prefix=ACCESSOR2_EXPAND %s
// ACCESSOR2_EXPAND: source.edit.kind.active:
// ACCESSOR2_EXPAND:   34:14-34:18 "{
// ACCESSOR2_EXPAND:  get { _storage.y }
// ACCESSOR2_EXPAND:  set { _storage.y = newValue }
// ACCESSOR2_EXPAND: }"
// ACCESSOR2_EXPAND: source.edit.kind.active:
// ACCESSOR2_EXPAND:   33:3-33:20 ""

//##-- Refactoring expanding the second accessor macro
// RUN: %sourcekitd-test -req=refactoring.expand.macro -pos=42:5 %s -- ${COMPILER_ARGS[@]} | %FileCheck -check-prefix=PEER_EXPAND %s
// PEER_EXPAND: source.edit.kind.active:
// PEER_EXPAND:   45:4-45:4 "
// PEER_EXPAND: func f(a: Int, for b: String, _ value: Double, completionHandler: (String) -> Void) {
// PEER_EXPAND:  Task {
// PEER_EXPAND:    completionHandler(await f(a: a, for: b, value))
// PEER_EXPAND:  }
// PEER_EXPAND: }
// PEER_EXPAND: source.edit.kind.active:
// PEER_EXPAND:   42:3-42:24 ""

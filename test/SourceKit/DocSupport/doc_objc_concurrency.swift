// REQUIRES: objc_interop
// REQUIRES: concurrency
// REQUIRES: swift_feature_SendableCompletionHandlers

// RUN: %empty-directory(%t)

// RUN: %sourcekitd-test -req=doc-info %S/../Inputs/concurrency/gen_concurrency.swift -- -target %target-triple -I %t -Xfrontend -enable-experimental-concurrency -enable-experimental-feature SendableCompletionHandlers | %FileCheck %s --check-prefix=SWIFT-DOC

// Never output @available (even if explicitly written by the user).

// SWIFT-DOC: key.fully_annotated_decl: "<decl.function.method.instance><syntaxtype.keyword>func</syntaxtype.keyword> <decl.name>foo</decl.name>(<decl.var.parameter><decl.var.parameter.argument_label>_</decl.var.parameter.argument_label> <decl.var.parameter.name>operation</decl.var.parameter.name>: <decl.var.parameter.type><ref.struct usr=\"s:SS\">String</ref.struct></decl.var.parameter.type></decl.var.parameter>, <decl.var.parameter><decl.var.parameter.argument_label>completionHandler</decl.var.parameter.argument_label> <decl.var.parameter.name>handler</decl.var.parameter.name>: <decl.var.parameter.type><syntaxtype.attribute.builtin><syntaxtype.attribute.name>@escaping</syntaxtype.attribute.name></syntaxtype.attribute.builtin> (<decl.var.parameter><decl.var.parameter.type><ref.struct usr=\"s:Si\">Int</ref.struct></decl.var.parameter.type></decl.var.parameter>) -&gt; <decl.function.returntype><ref.typealias usr=\"s:s4Voida\">Void</ref.typealias></decl.function.returntype></decl.var.parameter.type></decl.var.parameter>)</decl.function.method.instance>"

// SWIFT-DOC: key.fully_annotated_decl: "<decl.function.method.instance><syntaxtype.keyword>func</syntaxtype.keyword> <decl.name>foo</decl.name>(<decl.var.parameter><decl.var.parameter.argument_label>_</decl.var.parameter.argument_label> <decl.var.parameter.name>operation</decl.var.parameter.name>: <decl.var.parameter.type><ref.struct usr=\"s:SS\">String</ref.struct></decl.var.parameter.type></decl.var.parameter>) <syntaxtype.keyword>async</syntaxtype.keyword> -&gt; <decl.function.returntype><ref.struct usr=\"s:Si\">Int</ref.struct></decl.function.returntype></decl.function.method.instance>"


// RUN: %sourcekitd-test -req=doc-info -module ConcurrencyHeader -- -Xfrontend -enable-objc-interop -Xfrontend -enable-experimental-concurrency -I %S/../Inputs/concurrency/ -sdk %clang-importer-sdk -enable-experimental-feature SendableCompletionHandlers | %FileCheck %s --check-prefix=OBJC-DOC

// Especially if the @available was implicitly added to an imported Clang decl
// (rdar://76685011).

// OBJC-DOC: key.fully_annotated_decl: "<decl.function.method.instance><syntaxtype.keyword>func</syntaxtype.keyword> <decl.name>method</decl.name>(<decl.var.parameter><decl.var.parameter.argument_label>withHandler</decl.var.parameter.argument_label> <decl.var.parameter.name>operation</decl.var.parameter.name>: <decl.var.parameter.type><ref.struct usr=\"s:SS\">String</ref.struct>!</decl.var.parameter.type></decl.var.parameter>, <decl.var.parameter><decl.var.parameter.argument_label>completionHandler</decl.var.parameter.argument_label> <decl.var.parameter.name>handler</decl.var.parameter.name>: <decl.var.parameter.type>(<syntaxtype.attribute.builtin><syntaxtype.attribute.name>@Sendable</syntaxtype.attribute.name></syntaxtype.attribute.builtin> (<decl.var.parameter><decl.var.parameter.type><ref.struct usr=\"s:Si\">Int</ref.struct></decl.var.parameter.type></decl.var.parameter>) -&gt; <decl.function.returntype><ref.typealias usr=\"s:s4Voida\">Void</ref.typealias></decl.function.returntype>)!</decl.var.parameter.type></decl.var.parameter>)</decl.function.method.instance>"

// OBJC-DOC: key.fully_annotated_decl: "<decl.function.method.instance><syntaxtype.keyword>func</syntaxtype.keyword> <decl.name>method</decl.name>(<decl.var.parameter><decl.var.parameter.argument_label>withHandler</decl.var.parameter.argument_label> <decl.var.parameter.name>operation</decl.var.parameter.name>: <decl.var.parameter.type><ref.struct usr=\"s:SS\">String</ref.struct>!</decl.var.parameter.type></decl.var.parameter>) <syntaxtype.keyword>async</syntaxtype.keyword> -&gt; <decl.function.returntype><ref.struct usr=\"s:Si\">Int</ref.struct></decl.function.returntype></decl.function.method.instance>"

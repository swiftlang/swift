// REQUIRES: objc_interop
// REQUIRES: concurrency

// RUN: %empty-directory(%t)

// RUN: %sourcekitd-test -req=doc-info %S/../Inputs/concurrency/gen_concurrency.swift -- -target %target-triple -I %t -Xfrontend -enable-experimental-concurrency | %FileCheck %s --check-prefix=SWIFT-DOC

// Make sure we print @completionHandlerAsync when it was explicitly written by the user.

// SWIFT-DOC: key.fully_annotated_decl: "<decl.function.method.instance><syntaxtype.attribute.builtin><syntaxtype.attribute.name>@completionHandlerAsync</syntaxtype.attribute.name>(&quot;foo(_:)&quot;, completionHandlerIndex: 1)</syntaxtype.attribute.builtin> <syntaxtype.keyword>func</syntaxtype.keyword> <decl.name>foo</decl.name>(<decl.var.parameter><decl.var.parameter.argument_label>_</decl.var.parameter.argument_label> <decl.var.parameter.name>operation</decl.var.parameter.name>: <decl.var.parameter.type><ref.struct usr=\"s:SS\">String</ref.struct></decl.var.parameter.type></decl.var.parameter>, <decl.var.parameter><decl.var.parameter.argument_label>completionHandler</decl.var.parameter.argument_label> <decl.var.parameter.name>handler</decl.var.parameter.name>: <decl.var.parameter.type><syntaxtype.attribute.builtin><syntaxtype.attribute.name>@escaping</syntaxtype.attribute.name></syntaxtype.attribute.builtin> (<decl.var.parameter><decl.var.parameter.type><ref.struct usr=\"s:Si\">Int</ref.struct></decl.var.parameter.type></decl.var.parameter>) -&gt; <decl.function.returntype><ref.typealias usr=\"s:s4Voida\">Void</ref.typealias></decl.function.returntype></decl.var.parameter.type></decl.var.parameter>)</decl.function.method.instance>"

// SWIFT-DOC: key.fully_annotated_decl: "<decl.function.method.instance><syntaxtype.keyword>func</syntaxtype.keyword> <decl.name>foo</decl.name>(<decl.var.parameter><decl.var.parameter.argument_label>_</decl.var.parameter.argument_label> <decl.var.parameter.name>operation</decl.var.parameter.name>: <decl.var.parameter.type><ref.struct usr=\"s:SS\">String</ref.struct></decl.var.parameter.type></decl.var.parameter>) <syntaxtype.keyword>async</syntaxtype.keyword> -&gt; <decl.function.returntype><ref.struct usr=\"s:Si\">Int</ref.struct></decl.function.returntype></decl.function.method.instance>"


// RUN: %sourcekitd-test -req=doc-info -module ConcurrencyHeader -- -Xfrontend -enable-objc-interop -Xfrontend -enable-experimental-concurrency -I %S/../Inputs/concurrency/ -sdk %clang-importer-sdk | %FileCheck %s --check-prefix=OBJC-DOC

// But don't print @completionHandlerAsync if it was implicitly added to an imported Clang decl (rdar://76685011).

// OBJC-DOC: key.fully_annotated_decl: "<decl.function.method.instance><syntaxtype.keyword>func</syntaxtype.keyword> <decl.name>method</decl.name>(<decl.var.parameter><decl.var.parameter.argument_label>withHandler</decl.var.parameter.argument_label> <decl.var.parameter.name>operation</decl.var.parameter.name>: <decl.var.parameter.type><ref.struct usr=\"s:SS\">String</ref.struct>!</decl.var.parameter.type></decl.var.parameter>, <decl.var.parameter><decl.var.parameter.argument_label>completionHandler</decl.var.parameter.argument_label> <decl.var.parameter.name>handler</decl.var.parameter.name>: <decl.var.parameter.type>((<decl.var.parameter><decl.var.parameter.type><ref.struct usr=\"s:Si\">Int</ref.struct></decl.var.parameter.type></decl.var.parameter>) -&gt; <decl.function.returntype><ref.typealias usr=\"s:s4Voida\">Void</ref.typealias></decl.function.returntype>)!</decl.var.parameter.type></decl.var.parameter>)</decl.function.method.instance>"

// OBJC-DOC: key.fully_annotated_decl: "<decl.function.method.instance><syntaxtype.keyword>func</syntaxtype.keyword> <decl.name>method</decl.name>(<decl.var.parameter><decl.var.parameter.argument_label>withHandler</decl.var.parameter.argument_label> <decl.var.parameter.name>operation</decl.var.parameter.name>: <decl.var.parameter.type><ref.struct usr=\"s:SS\">String</ref.struct>!</decl.var.parameter.type></decl.var.parameter>) <syntaxtype.keyword>async</syntaxtype.keyword> -&gt; <decl.function.returntype><ref.struct usr=\"s:Si\">Int</ref.struct></decl.function.returntype></decl.function.method.instance>"

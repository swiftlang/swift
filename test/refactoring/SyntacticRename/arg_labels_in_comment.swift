// REQUIRES: swift_swift_parser
// RUN: %empty-directory(%t)
// RUN: split-file %s %t
// RUN: %refactor -find-rename-ranges -source-filename %t/input.swift -pos="foo" -is-function-like -old-name "foo(baz:)" > %t/actual-output.swift
// RUN: diff -u %t/expected-output.swift %t/actual-output.swift

//--- input.swift

/// `/*foo:unknown*/foo(baz:)` does very importants stuff
func /*foo:def*/foo(baz: Int) {}

//--- expected-output.swift

/// `/*foo:unknown*/<base>foo</base>(<sel index=0>baz</sel>:)` does very importants stuff
func /*foo:def*/<base>foo</base>(<arglabel index=0>baz</arglabel><param index=0></param>: Int) {}


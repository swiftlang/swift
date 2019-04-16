// RUN: not %swift -emit-sil -target %target-triple %s -emit-fixits-path %t.remap -diagnostics-editor-mode
// RUN: c-arcmt-test %t.remap | arcmt-test -verify-transformed-files %s.result

func foo(var arg: Int) {}
func bar(let arg: Int) {}

func baz(var arg: Int, var arg2: Int) {}

// RUN: %swift-ide-test -code-completion -source-filename %s -code-completion-token=BUILTIN_1 | FileCheck %s -check-prefix=NO_CRASH
// RUN: %swift-ide-test -code-completion -source-filename %s -code-completion-token=BUILTIN_2 | FileCheck %s -check-prefix=NO_CRASH

// NO_CRASH: Begin completions
// NO_CRASH: End completions

//===---
//===--- Test code completion for types from Builtin module.
//===---

// We don't give any useful completions, but we should not crash either.

func testBuiltinRawPointer1(a: UnsafePointer<UInt8>) {
  a.value#^BUILTIN_1^#
}

func testBuiltinRawPointer2(a: UnsafePointer<UInt8>) {
  a.value.#^BUILTIN_2^#
}

// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=BUILTIN_1 | FileCheck %s -check-prefix=NO_CRASH
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=BUILTIN_2 | FileCheck %s -check-prefix=NO_CRASH

// NO_CRASH-NOT: Begin completions

//===---
//===--- Test code completion for types from Builtin module.
//===---

// We don't give any useful completions, but we should not crash either.

func testBuiltinRawPointer1(a: UnsafeMutablePointer<UInt8>) {
  a.value#^BUILTIN_1^#
}

func testBuiltinRawPointer2(a: UnsafeMutablePointer<UInt8>) {
  a.value.#^BUILTIN_2^#
}

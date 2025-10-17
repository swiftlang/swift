// RUN: %swift -typecheck %s -verify -target arm64-apple-none-macho -parse-stdlib
// RUN: %swift -typecheck %s -verify -target arm64-apple-none-elf -parse-stdlib
// RUN: %swift -typecheck %s -verify -target wasm32-unknown-wasi -parse-stdlib
// RUN: %swift -typecheck %s -verify -target x86_64-unknown-windows-msvc -parse-stdlib
// RUN: %swift-ide-test -test-input-complete -source-filename=%s -target arm64-apple-macos

#if objectFormat(MachO) || objectFormat(ELF) || objectFormat(Wasm) || objectFormat(COFF)
class C {}
var x = C()
#endif
var y = x

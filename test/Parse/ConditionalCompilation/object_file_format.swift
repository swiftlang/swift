// RUN: %swift -typecheck %s -verify -target arm64-apple-none-macho -parse-stdlib
// RUN: %swift -typecheck %s -verify -target arm64-apple-none-elf -parse-stdlib
// RUN: %swift -typecheck %s -verify -target wasm32-unknown-wasi -parse-stdlib
// RUN: %swift -typecheck %s -verify -target x86_64-unknown-windows-msvc -parse-stdlib
// RUN: %swift-ide-test -test-input-complete -source-filename=%s -target arm64-apple-macos

#if _objectFileFormat(MachO) || _objectFileFormat(ELF) || _objectFileFormat(Wasm) || _objectFileFormat(COFF)
class C {}
var x = C()
#endif
var y = x

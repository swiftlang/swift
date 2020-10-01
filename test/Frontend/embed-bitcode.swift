// REQUIRES: CPU=x86_64
// RUN: %target-swift-frontend -c -module-name someModule -embed-bitcode-marker  -o %t.o %s
// RUN: llvm-objdump --macho --section="__LLVM,__bitcode" %t.o | %FileCheck -check-prefix=MARKER %s
// RUN: llvm-objdump --macho --section="__LLVM,__swift_cmdline" %t.o | %FileCheck -check-prefix=MARKER-CMD %s

// This file tests Mach-O file output, but Linux variants do not produce Mach-O
// files.
// UNSUPPORTED: OS=linux-gnu
// UNSUPPORTED: OS=linux-gnueabihf
// UNSUPPORTED: OS=freebsd
// UNSUPPORTED: OS=openbsd
// UNSUPPORTED: OS=windows-msvc

// MARKER: Contents of (__LLVM,__bitcode) section
// MARKER-NEXT: 00
// MARKER-CMD: Contents of (__LLVM,__swift_cmdline) section
// MARKER-CMD-NEXT: 00

func test() {
}

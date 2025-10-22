// RUN: %swift -typecheck %s -target arm64-apple-none-macho -parse-stdlib 2>&1 | %FileCheck -check-prefix CHECK-MACHO %s
// RUN: %swift -typecheck %s -target arm64-apple-none-elf -parse-stdlib 2>&1 | %FileCheck -check-prefix CHECK-ELF %s
// RUN: %swift -typecheck %s -target wasm32-unknown-wasi -parse-stdlib 2>&1 | %FileCheck -check-prefix CHECK-WASM %s
// RUN: %swift -typecheck %s -target x86_64-unknown-windows-msvc -parse-stdlib 2>&1 | %FileCheck -check-prefix CHECK-COFF %s

#if objectFormat(MachO)
#warning("I'm MachO")
#else
#warning("I'm not MachO")
#endif

#if objectFormat(ELF)
#warning("I'm ELF")
#else
#warning("I'm not ELF")
#endif

#if objectFormat(Wasm)
#warning("I'm Wasm")
#else
#warning("I'm not Wasm")
#endif

#if objectFormat(COFF)
#warning("I'm COFF")
#else
#warning("I'm not COFF")
#endif

// CHECK-MACHO: I'm MachO
// CHECK-MACHO: I'm not ELF
// CHECK-MACHO: I'm not Wasm
// CHECK-MACHO: I'm not COFF

// CHECK-ELF: I'm not MachO
// CHECK-ELF: I'm ELF
// CHECK-ELF: I'm not Wasm
// CHECK-ELF: I'm not COFF

// CHECK-WASM: I'm not MachO
// CHECK-WASM: I'm not ELF
// CHECK-WASM: I'm Wasm
// CHECK-WASM: I'm not COFF

// CHECK-COFF: I'm not MachO
// CHECK-COFF: I'm not ELF
// CHECK-COFF: I'm not Wasm
// CHECK-COFF: I'm COFF

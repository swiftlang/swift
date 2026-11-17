// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// Protocol conformance descriptors are placed in .rodata on ELF. A descriptor
// for a conformance to a resilient protocol that refines another protocol
// contains a relative reference to a tagged (low bit set) associated
// conformance string. That reference must be recognizable by LLVM as a
// link-time constant, or LLVM will emit the descriptor into a writable
// .rodata section. GNU ld folds .rodata into the text segment, which then
// becomes RWX (rdar://186556339, https://github.com/swiftlang/swift/issues/91943).

// RUN: %swift_frontend_plain -target aarch64-unknown-linux-gnu -parse-stdlib -disable-objc-interop -enable-library-evolution -emit-module %t/Base.swift -module-name Base -o %t/Base.swiftmodule
// RUN: %swift_frontend_plain -target aarch64-unknown-linux-gnu -parse-stdlib -disable-objc-interop -I %t -emit-ir %t/main.swift -module-name main -o - | %FileCheck %s --check-prefix=IR
// RUN: %swift_frontend_plain -target aarch64-unknown-linux-gnu -parse-stdlib -disable-objc-interop -I %t -c %t/main.swift -module-name main -o %t/main-aarch64.o
// RUN: %llvm-readelf -SW %t/main-aarch64.o | %FileCheck %s --check-prefix=ELF
// RUN: %llvm-nm %t/main-aarch64.o | %FileCheck %s --check-prefix=SYMS

// RUN: %swift_frontend_plain -target x86_64-unknown-linux-gnu -parse-stdlib -disable-objc-interop -enable-library-evolution -emit-module %t/Base.swift -module-name Base -o %t/Base.swiftmodule
// RUN: %swift_frontend_plain -target x86_64-unknown-linux-gnu -parse-stdlib -disable-objc-interop -I %t -c %t/main.swift -module-name main -o %t/main-x86_64.o
// RUN: %llvm-readelf -SW %t/main-x86_64.o | %FileCheck %s --check-prefix=ELF
// RUN: %llvm-nm %t/main-x86_64.o | %FileCheck %s --check-prefix=SYMS

// REQUIRES: CODEGENERATOR=AArch64
// REQUIRES: CODEGENERATOR=X86

// The tagged reference must be an inbounds GEP.
// IR: @"$s4main1SV4Base7DerivedAAMc" = {{.*}}constant {{.*}}getelementptr inbounds (i8, ptr @"associated conformance {{.*}}", i64 1){{.*}}, section ".rodata"

// No .rodata section may be writable.
// ELF-NOT: {{\.rodata +PROGBITS +[0-9a-f]+ [0-9a-f]+ [0-9a-f]+ [0-9a-f]+ +[A-Z]*W}}
// ELF: {{\.rodata +PROGBITS}}
// ELF-NOT: {{\.rodata +PROGBITS +[0-9a-f]+ [0-9a-f]+ [0-9a-f]+ [0-9a-f]+ +[A-Z]*W}}

// SYMS: R $s4main1SV4Base7DerivedAAMc

//--- Base.swift
public protocol BaseProto {}
public protocol Derived: BaseProto {}

//--- main.swift
import Base

public struct S: Derived {}

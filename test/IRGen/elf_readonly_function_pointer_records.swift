// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// Async and coroutine function pointer records can't be marked constant in IR,
// because CoroSplit updates their context size. On ELF, a mutable global placed
// in .rodata makes the section writable, and GNU ld folds .rodata into the text
// segment, which then becomes RWX
// (https://github.com/swiftlang/swift/issues/91943). So on ELF the records stay
// in their default section.

// RUN: %swift_frontend_plain -target aarch64-unknown-linux-gnu -parse-stdlib -disable-objc-interop -module-name main -emit-ir %t/async.sil -o - | %FileCheck %s --check-prefix=IR
// RUN: %swift_frontend_plain -target aarch64-unknown-linux-gnu -parse-stdlib -disable-objc-interop -module-name main -enable-callee-allocated-coro-abi -enable-experimental-feature CoroutineAccessors -emit-ir %t/coro.swift -o - | %FileCheck %s --check-prefix=IR

// RUN: %swift_frontend_plain -target aarch64-unknown-linux-gnu -parse-stdlib -disable-objc-interop -module-name main -c %t/async.sil -o %t/async-aarch64.o
// RUN: %llvm-readelf -SW %t/async-aarch64.o | %FileCheck %s --check-prefix=ELF
// RUN: %swift_frontend_plain -target aarch64-unknown-linux-gnu -parse-stdlib -disable-objc-interop -module-name main -enable-callee-allocated-coro-abi -enable-experimental-feature CoroutineAccessors -c %t/coro.swift -o %t/coro-aarch64.o
// RUN: %llvm-readelf -SW %t/coro-aarch64.o | %FileCheck %s --check-prefix=ELF

// RUN: %swift_frontend_plain -target x86_64-unknown-linux-gnu -parse-stdlib -disable-objc-interop -module-name main -c %t/async.sil -o %t/async-x86_64.o
// RUN: %llvm-readelf -SW %t/async-x86_64.o | %FileCheck %s --check-prefix=ELF
// RUN: %swift_frontend_plain -target x86_64-unknown-linux-gnu -parse-stdlib -disable-objc-interop -module-name main -enable-callee-allocated-coro-abi -enable-experimental-feature CoroutineAccessors -c %t/coro.swift -o %t/coro-x86_64.o
// RUN: %llvm-readelf -SW %t/coro-x86_64.o | %FileCheck %s --check-prefix=ELF

// REQUIRES: CODEGENERATOR=AArch64
// REQUIRES: CODEGENERATOR=X86
// REQUIRES: swift_feature_CoroutineAccessors

// The records have no section.
// IR: {{^@fTu|^@"\$s4main1SV3irmBi64_vyTwc"}} = {{.*}}global {{%swift.async_func_pointer|%swift.coro_func_pointer}} <{ {{.*}} }>, align {{[0-9]+$}}

// No .rodata section may be writable.
// ELF-NOT: {{\.rodata +PROGBITS +[0-9a-f]+ [0-9a-f]+ [0-9a-f]+ [0-9a-f]+ +[A-Z]*W}}

//--- async.sil
sil_stage canonical

import Builtin

sil @f : $@async @convention(thin) () -> () {
bb0:
  %0 = tuple ()
  return %0 : $()
}

//--- coro.swift
public struct S {
  public var _i: Builtin.Int64
  public var irm: Builtin.Int64 {
    yielding borrow { yield _i }
  }
}

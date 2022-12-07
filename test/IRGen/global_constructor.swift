// RUN: %target-swift-frontend -parse-stdlib -emit-ir -module-name foo %s | %IRGenFileCheck %s

// RUN: %target-swift-frontend -target x86_64-apple-macosx11.0 -S -parse-stdlib -primary-file %s -module-name foo -o - | %FileCheck %s --check-prefix=CHECK-MACHO
// RUN: %target-swift-frontend -target x86_64-unknown-linux-gnu -S -parse-stdlib -primary-file %s -module-name foo -o - | %FileCheck %s --check-prefix=CHECK-ELF
// RUN: %target-swift-frontend -target x86_64-unknown-windows-itanium -S -parse-stdlib -primary-file %s -module-name foo -o - | %FileCheck %s --check-prefix=CHECK-COFF

@globalConstructor(priority: 0) func first() {}

@globalConstructor(priority: 100) private func third() {}

@globalConstructor(priority: 10) public func second() {}

// CHECK: @llvm.global_ctors = appending global
// CHECK-SAME: { i32 0, void ()* @"$s3foo5firstyyF"
// CHECK-SAME: { i32 100, void ()* @"$s3foo5third
// CHECK-SAME: { i32 10, void ()* @"$s3foo6secondyyF"

// CHECK-MACHO: __mod_init_func
// CHECK-MACHO-NEXT: .p2align
// CHECK-MACHO-NEXT: .quad	_$s3foo5firstyyF
// CHECK-MACHO-NEXT: .quad	_$s3foo6secondyyF
// CHECK-MACHO-NEXT: .quad	_$s3foo5third

// CHECK-ELF: .init_array.0
// CHECK-ELF: .quad	($s3foo5firstyyF)
// CHECK-ELF: .init_array.10
// CHECK-ELF: .quad	($s3foo6secondyyF)
// CHECK-ELF: .init_array.100
// CHECK-ELF: .quad	($s3foo5third

// CHECK-COFF: .CRT$XCA00000
// CHECK-COFF: .p2align
// CHECK-COFF: .quad	($s3foo5firstyyF)
// CHECK-COFF: .CRT$XCA00010
// CHECK-COFF: .quad	($s3foo6secondyyF)
// CHECK-COFF: .CRT$XCA00100
// CHECK-COFF: .quad	($s3foo5third

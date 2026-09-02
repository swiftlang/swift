// RUN: %target-swift-emit-ir %s -enable-experimental-feature Embedded -target aarch64-none-none-elf -wmo | %FileCheck %s --check-prefix CHECK-ELF
// RUN: %target-swift-emit-ir %s -enable-experimental-feature Embedded -target arm64-apple-none-macho -wmo -Xcc -D__MACH__ -Xcc -D__arm64__ -Xcc -D__APPLE__ | %FileCheck %s --check-prefix CHECK-MACHO

// REQUIRES: swift_in_compiler

public func main() {
  print("hello")
}

// CHECK-ELF: @swift_abi = weak_odr constant i32 2, section ".swift_abi"
// CHECK-ELF: @_swift1_autolink_entries =
// CHECK-ELF: @llvm.used = appending global [2 x ptr] [ptr @swift_abi, ptr @_swift1_autolink_entries], section "llvm.metadata" 

// CHECK-MACHO: @swift_abi = weak_odr constant i32 2, section "__TEXT,__swift_abi"
// CHECK-MACHO: @llvm.used = appending global [1 x ptr] [ptr @swift_abi], section "llvm.metadata" 

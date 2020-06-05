// UNSUPPORTED: OS=windows-msvc
// FIXME: ld64 in Xcode toolchain uses older version of LLVM than swiftc, so ld64 can't read module summary for LTO 
//        from bitcode file produced by compiler. This should be fixed before shipping Xcode toolchain by upgrading
//        LLVM version used in ld64.
// XFAIL: OS=macosx
// XFAIL: OS=tvos
// XFAIL: OS=watchos
// XFAIL: OS=ios

// RUN: rm -rf %t
// RUN: %empty-directory(%t/thin-static)

// RUN: %target-swiftc_driver %S/Inputs/lto/lib.swift -static -lto=llvm -emit-library -emit-module -module-name A -working-directory %t/thin-static
// RUN: %target-swiftc_driver %S/Inputs/lto/main.swift -L. -I. -lA -lto=llvm -working-directory %t/thin-static

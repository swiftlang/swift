// RUN: %empty-directory(%t)
// RUN: %target-swiftc_driver -emit-module %S/Inputs/lib.swift -emit-module-path %t
// RUN: %target-swift-frontend -emit-sib %S/Inputs/lib.swift -parse-as-library -o %t/lib.sib
// RUN: %target-swift-frontend -emit-sib -I%t  %S/Inputs/main.swift -o %t/main.sib

// RUN: %sil-opt -emit-sorted-sil %t/lib.sib %t/main.sib | %FileCheck %s

// CHECK: sil hidden @$s3lib4LibXVACycfC : $@convention(method) (@thin LibX.Type) -> LibX {
// CHECK: sil @$s3lib7getLibXAA0C1XVyF : $@convention(thin) () -> LibX {
// CHECK: sil @main : $@convention(c) (Int32, UnsafeMutablePointer<Optional<UnsafeMutablePointer<Int8>>>) -> Int32 {


// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -emit-module -module-name def_class -o %t/stage1.swiftmodule %S/Inputs/def_class.swift -disable-objc-attr-requires-foundation-module -enable-objc-interop
// RUN: %target-swift-frontend -emit-module -parse-as-library -o %t/def_class.swiftmodule %t/stage1.swiftmodule
// RUN: %target-swift-frontend -emit-sil -sil-debug-serialization -I %t %S/class.swift | %FileCheck %s -check-prefix=SIL

// SIL-LABEL: sil public_external [transparent] [serialized] [canonical] @$sSi1poiyS2i_SitFZ : $@convention(method) (Int, Int, @thin Int.Type) -> Int

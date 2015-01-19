// RUN: rm -rf %t
// RUN: mkdir %t
// RUN: %target-swift-frontend -emit-module -module-name def_class -o %t/stage1.swiftmodule %S/Inputs/def_class.swift -disable-objc-attr-requires-foundation-module
// RUN: %target-swift-frontend -emit-module -parse-as-library -o %t/def_class.swiftmodule %t/stage1.swiftmodule
// RUN: %target-swift-frontend -emit-sil -sil-debug-serialization -I %t %S/class.swift | FileCheck %s -check-prefix=SIL

// SIL-LABEL: sil public_external [transparent] [fragile] @_TFSsoi1pFTSiSi_Si : $@thin (Int, Int) -> Int {

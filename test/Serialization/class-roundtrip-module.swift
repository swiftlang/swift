// RUN: rm -rf %t
// RUN: mkdir %t
// RUN: %swift -emit-module -module-name def_class -o %t/stage1.swiftmodule %S/Inputs/def_class.swift
// RUN: %swift -emit-module -parse-as-library -o %t/def_class.swiftmodule %t/stage1.swiftmodule
// RUN: %swift -emit-silgen -enable-sil-linking -I=%t %S/class.swift | FileCheck %s -check-prefix=SIL

// SIL-LABEL: sil deserialized [transparent] @_TSsoi1pFT3lhsSi3rhsSi_Si : $@thin (Int64, Int64) -> Int64 {

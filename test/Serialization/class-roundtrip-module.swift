// RUN: rm -rf %t
// RUN: mkdir %t
// RUN: %swift -emit-module -module-name def_class -o %t/stage1.swiftmodule %S/Inputs/def_class.swift
// RUN: %swift -emit-module -parse-as-library -o %t/def_class.swiftmodule %t/stage1.swiftmodule
// RUN: %swift -emit-sil -sil-debug-serialization -I=%t %S/class.swift | FileCheck %s -check-prefix=SIL

// SIL-LABEL: sil public_external [transparent] @_TFSsoi1pFTSiSi_Si : $@thin (Int, Int) -> Int {

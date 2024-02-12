// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -emit-module -enable-experimental-feature RawLayout -module-name raw_layout_fred -o %t %S/Inputs/raw_layout.swift
// RUN: %target-swift-frontend -I %t -emit-ir %s -verify | %FileCheck %s

import raw_layout_fred

// CHECK: %T15raw_layout_fred4FredVySbG = type <{ [1 x i8] }>

do {
  // CHECK: {{%.*}} = alloca %T15raw_layout_fred4FredVySbG
  // CHECK: call swiftcc void @"$s15raw_layout_fred4FredVACyxGycfC"
  let _ = Fred<Bool>()
}

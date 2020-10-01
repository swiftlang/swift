// RUN: %empty-directory(%t)
// RUN: %empty-directory(%t/modulecache)

// 1) Build .swiftinterface files for MyPoint and MyExtensions, using a non-default module cache path
// RUN: %target-swift-frontend -emit-module-interface-path %t/MyPoint.swiftinterface -module-name MyPoint -emit-module -o /dev/null %S/Inputs/parseable-interface/MyPoint.swift
// RUN: %target-swift-frontend -emit-module-interface-path %t/MyPointExtensions.swiftinterface -module-name MyPointExtensions -emit-module -o /dev/null -module-cache-path %t/modulecache -I %t %S/Inputs/parseable-interface/MyPointExtensions.swift
// RUN: %empty-directory(%t/modulecache)

// 2) Check completion using the default (cold) module cache
// RUN: %target-swift-ide-test -code-completion -code-completion-token=MEMBER -source-filename %s -I %t | %FileCheck %s

// 3) Check completion again with a warm module cache
// RUN: %target-swift-ide-test -code-completion -code-completion-token=MEMBER -source-filename %s -I %t | %FileCheck %s

import MyPoint
import MyPointExtensions

let x = MyPoint(x: 1, y: 10.5)

print(x.#^MEMBER^#)

// CHECK: Begin completions, 5 items
// CHECK: Keyword[self]/CurrNominal:          self[#MyPoint#]; name=self
// CHECK: Decl[InstanceVar]/CurrNominal:      x[#Double#]; name=x
// CHECK: Decl[InstanceVar]/CurrNominal:      y[#Double#]; name=y
// CHECK: Decl[InstanceVar]/CurrNominal:      magnitudeSquared[#Double#]; name=magnitudeSquared
// CHECK: Decl[InstanceVar]/CurrNominal:      magnitude[#Double#]; name=magnitude
// CHECK: End completions

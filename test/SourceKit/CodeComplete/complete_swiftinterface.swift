// RUN: %empty-directory(%t)
// RUN: %empty-directory(%t/modulecache)

// 1) Build .swiftinterface files for MyPoint and MyExtensions
// RUN: %target-swift-frontend -emit-parseable-module-interface-path %t/MyPoint.swiftinterface -module-name MyPoint -emit-module -o /dev/null %S/Inputs/parseable-interface/MyPoint.swift
// RUN: %target-swift-frontend -emit-parseable-module-interface-path %t/MyPointExtensions.swiftinterface -module-name MyPointExtensions -emit-module -o /dev/null -enable-parseable-module-interface -module-cache-path %t/modulecache -I %t %S/Inputs/parseable-interface/MyPointExtensions.swift

// 2) Check completion with a warm module cache from above
// RUN: %swift-ide-test_plain -code-completion -code-completion-token=MEMBER -source-filename %s -I %t -module-cache-path %t/modulecache | %FileCheck %s

// 3) Check completion with a cold module cache
// RUN: %empty-directory(%t/modulecache)
// RUN: %swift-ide-test_plain -code-completion -code-completion-token=MEMBER -source-filename %s -I %t -module-cache-path %t/modulecache | %FileCheck %s

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

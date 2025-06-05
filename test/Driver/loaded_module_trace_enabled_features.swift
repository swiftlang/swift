// RUN: %empty-directory(%t)

// RUN: %target-swift-frontend %s -emit-module -o /dev/null -swift-version 4 \
// RUN:   -emit-loaded-module-trace-path %t/swift4.trace.json
// RUN: %FileCheck -check-prefix=CHECK-SWIFT4 %s < %t/swift4.trace.json

// RUN: %target-swift-frontend %s -emit-module -o /dev/null -swift-version 5 \
// RUN:   -emit-loaded-module-trace-path %t/swift5.trace.json
// RUN: %FileCheck -check-prefix=CHECK-SWIFT5 %s < %t/swift5.trace.json

// RUN: %target-swift-frontend %s -emit-module -o /dev/null -swift-version 5 \
// RUN:   -emit-loaded-module-trace-path %t/swift5_and_features.trace.json \
// RUN:   -enable-experimental-feature ParserValidation \
// RUN:   -enable-upcoming-feature RegionBasedIsolation \
// RUN:   -strict-memory-safety
// RUN: %FileCheck -check-prefix=CHECK-SWIFT5-PLUS %s < %t/swift5_and_features.trace.json

// RUN: %target-swift-frontend %s -emit-module -o /dev/null -swift-version 6 \
// RUN:   -emit-loaded-module-trace-path %t/swift6.trace.json
// RUN: %FileCheck -check-prefix=CHECK-SWIFT6 %s < %t/swift6.trace.json

// NOTE: The matching of the enabledLanguageFeatures lists below is
// intentionally inexact. There are few experimental features (ParserRoundTrip,
// ParserValidation) that are enabled by default in asserts compilers but
// otherwise disabled, so the enabled feature lists will sometimes contain
// additional entries.

// REQUIRES: swift_feature_ParserValidation
// REQUIRES: swift_feature_RegionBasedIsolation

// CHECK-SWIFT4: {
// CHECK-SWIFT4: "version":2
// CHECK-SWIFT4: "arch":"{{[^"]*}}"
// CHECK-SWIFT4: "languageMode":"4"
// CHECK-SWIFT4: "enabledLanguageFeatures":[
// CHECK-SWIFT4: ]
// CHECK-SWIFT4: "strictMemorySafety":false

// CHECK-SWIFT5: {
// CHECK-SWIFT5: "version":2
// CHECK-SWIFT5: "arch":"{{[^"]*}}"
// CHECK-SWIFT5: "languageMode":"5"
// CHECK-SWIFT5: "enabledLanguageFeatures":[
// CHECK-SWIFT5: "NonfrozenEnumExhaustivity"
// CHECK-SWIFT5: ]
// CHECK-SWIFT5: "strictMemorySafety":false

// CHECK-SWIFT5-PLUS: {
// CHECK-SWIFT5-PLUS: "version":2
// CHECK-SWIFT5-PLUS: "arch":"{{[^"]*}}"
// CHECK-SWIFT5-PLUS: "languageMode":"5"
// CHECK-SWIFT5-PLUS: "enabledLanguageFeatures":[
// CHECK-SWIFT5-PLUS: "NonfrozenEnumExhaustivity",
// CHECK-SWIFT5-PLUS: "ParserValidation",
// CHECK-SWIFT5-PLUS: "RegionBasedIsolation",
// CHECK-SWIFT5-PLUS: "StrictMemorySafety"
// CHECK-SWIFT5-PLUS: ]
// CHECK-SWIFT5-PLUS: "strictMemorySafety":true

// CHECK-SWIFT6: {
// CHECK-SWIFT6: "version":2
// CHECK-SWIFT6: "arch":"{{[^"]*}}"
// CHECK-SWIFT6: "languageMode":"6"
// CHECK-SWIFT6: "enabledLanguageFeatures":[
// CHECK-SWIFT6: "BareSlashRegexLiterals",
// CHECK-SWIFT6: "ConciseMagicFile",
// CHECK-SWIFT6: "DeprecateApplicationMain",
// CHECK-SWIFT6: "DisableOutwardActorInference",
// CHECK-SWIFT6: "DynamicActorIsolation",
// CHECK-SWIFT6: "ForwardTrailingClosures",
// CHECK-SWIFT6: "GlobalActorIsolatedTypesUsability",
// CHECK-SWIFT6: "GlobalConcurrency",
// CHECK-SWIFT6: "ImplicitOpenExistentials",
// CHECK-SWIFT6: "ImportObjcForwardDeclarations",
// CHECK-SWIFT6: "InferSendableFromCaptures",
// CHECK-SWIFT6: "IsolatedDefaultValues",
// CHECK-SWIFT6: "NonfrozenEnumExhaustivity",
// CHECK-SWIFT6: "RegionBasedIsolation",
// CHECK-SWIFT6: "StrictConcurrency"
// CHECK-SWIFT6: ]
// CHECK-SWIFT6: "strictMemorySafety":false

import Swift

// RUN: rm -rf %t && mkdir -p %t
// RUN: cp -r %S/Inputs/mixed-framework/Mixed.framework %t

// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -emit-module -o %t/Mixed.framework/Modules/Mixed.swiftmodule/%target-swiftmodule-name %S/Inputs/mixed-framework/Mixed.swift -import-underlying-module -F %t -module-name Mixed -disable-objc-attr-requires-foundation-module

// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -typecheck -F %t -import-objc-header %S/Inputs/import-mixed-framework-with-forward.h %s -verify

// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -emit-pch -F %t %S/Inputs/import-mixed-framework-with-forward.h -o %t/bridge.pch
// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -typecheck -F %t -import-objc-header %t/bridge.pch %s -verify


// REQUIRES: objc_interop

import Mixed

BridgingHeader.takeForward(SwiftClass(x: 42))
BridgingHeader.takeRenamedForward(CustomNameClass())

// Check that we're compiling at all.
BridgingHeader.takeRenamedForward(SwiftClass(x: 42)) // expected-error {{cannot convert value of type 'SwiftClass' to expected argument type 'CustomNameClass!'}}

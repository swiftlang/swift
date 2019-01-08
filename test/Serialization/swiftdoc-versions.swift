// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -emit-module -o %t %S/../Inputs/empty.swift
// RUN: %target-swift-frontend -typecheck -I %t %s

// RUN: cp %S/Inputs/swiftdoc-versions/empty-1.0.swiftdoc %t/empty.swiftdoc
// RUN: %target-swift-frontend -typecheck -I %t %s

// RUN: cp %S/Inputs/swiftdoc-versions/empty-1.1.swiftdoc %t/empty.swiftdoc
// RUN: %target-swift-frontend -typecheck -I %t %s

// RUN: cp %S/Inputs/swiftdoc-versions/empty-1.257.swiftdoc %t/empty.swiftdoc
// RUN: %target-swift-frontend -typecheck -I %t %s

// RUN: cp %S/Inputs/swiftdoc-versions/empty-257.1.swiftdoc %t/empty.swiftdoc
// RUN: %target-swift-frontend -typecheck -I %t %s -verify -show-diagnostics-after-fatal

// RUN: cp %S/Inputs/swiftdoc-versions/empty-0.7.swiftdoc %t/empty.swiftdoc
// RUN: %target-swift-frontend -typecheck -I %t %s -verify -show-diagnostics-after-fatal

import empty // expected-error{{malformed module file}}

// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -emit-module -o %t %S/../Inputs/empty.swift -module-name empty2
// RUN: %target-swift-frontend -typecheck -I %t %s

// RUN: %target-swift-frontend -emit-module -emit-module-doc -o %t %S/../Inputs/empty.swift
// RUN: mv %t/empty.swiftdoc %t/empty2.swiftdoc
// RUN: %target-swift-frontend -typecheck -I %t %s -verify -show-diagnostics-after-fatal

import empty2 // expected-error{{malformed module file}}
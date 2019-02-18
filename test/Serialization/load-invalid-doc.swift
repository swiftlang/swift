// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -emit-module -o %t %S/../Inputs/empty.swift
// RUN: %target-swift-frontend -typecheck -I %t %s

// RUN: touch %t/empty.swiftdoc
// RUN: %target-swift-frontend -typecheck -I %t %s -verify -show-diagnostics-after-fatal

// RUN: echo -n 'a' > %t/empty.swiftdoc
// RUN: %target-swift-frontend -typecheck -I %t %s -verify -show-diagnostics-after-fatal

// RUN: echo -n 'abcd' > %t/empty.swiftdoc
// RUN: %target-swift-frontend -typecheck -I %t %s -verify -show-diagnostics-after-fatal

// RUN: echo -n 'abcde' > %t/empty.swiftdoc
// RUN: %target-swift-frontend -typecheck -I %t %s -verify -show-diagnostics-after-fatal

import empty // expected-error{{malformed module file}}

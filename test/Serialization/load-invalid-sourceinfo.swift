// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -emit-module -o %t %S/../Inputs/empty.swift
// RUN: %target-swift-frontend -typecheck -I %t %s

// RUN: touch %t/empty.swiftsourceinfo
// RUN: %target-swift-frontend -typecheck -I %t %s -verify

// RUN: echo -n 'a' > %t/empty.swiftsourceinfo
// RUN: %target-swift-frontend -typecheck -I %t %s -verify

// RUN: echo -n 'abcd' > %t/empty.swiftsourceinfo
// RUN: %target-swift-frontend -typecheck -I %t %s -verify

// RUN: echo -n 'abcde' > %t/empty.swiftsourceinfo
// RUN: %target-swift-frontend -typecheck -I %t %s -verify

import empty // expected-warning{{unable to use malformed module source info}}

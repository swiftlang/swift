// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -typecheck -update-code -primary-file %s -F %S/mock-sdk -emit-migrated-file-path %t/result.swift -swift-version 4
// RUN: diff -u %s.expected %t/result.swift

import TestMyTime

let zero = kMyTimeZero

let _ = MyTimeAdd(kMyTimeZero, kMyTimeZero)
let _ = MyTimeAdd(
  kMyTimeZero, kMyTimeZero)
let _ = MyTimeAdd(
  kMyTimeZero,
  kMyTimeZero)

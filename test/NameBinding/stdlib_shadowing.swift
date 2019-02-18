// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -emit-module -o %t %S/Inputs/HasResult.swift
// RUN: %target-swift-frontend -typecheck %s -I %t -verify

import HasResult

func foo() -> Result<Int, Error> {
  return Result<Int, Error>.success(42)
}

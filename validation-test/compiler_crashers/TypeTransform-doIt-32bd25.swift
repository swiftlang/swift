// {"kind":"typecheck","signature":"swift::TypeTransform<(anonymous namespace)::TypeSimplifier>::doIt(swift::Type, swift::TypePosition)","signatureAssert":"Assertion failed: (flags.getValueOwnership() == ValueOwnership::Default), function doIt"}
// RUN: not --crash %target-swift-frontend -typecheck %s
func a<b>((__shared b) -> Void)
func c(UnsafeMutablePointer<UInt8>) a {
  c(&$0

// {"kind":"typecheck","signature":"swift::TypeTransform<(anonymous namespace)::TypeSimplifier>::doIt(swift::Type, swift::TypePosition)","signatureAssert":"Assertion failed: (flags.getValueOwnership() == ValueOwnership::Default), function doIt","signatureNext":"ConstraintSystem::simplifyType"}
// The crash depends on the C++ parser's handling of this input, which ASTGen
// handles differently. Expect it to fail, so that we notice if it crashes again.
// XFAIL: parser_astgen
// RUN: not --crash %target-swift-frontend -typecheck %s
func a<b>((__shared b) -> Void)
func c(UnsafeMutablePointer<UInt8>) a {
  c(&$0

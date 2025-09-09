// {"kind":"typecheck","signature":"swift::TypeTransform<swift::Type::transformRec(llvm::function_ref<std::__1::optional<swift::Type> (swift::TypeBase*)>) const::Transform>::doIt(swift::Type, swift::TypePosition)","signatureAssert":"Assertion failed: (flags.getValueOwnership() == ValueOwnership::Default), function doIt"}
// RUN: not --crash %target-swift-frontend -typecheck %s
func a<b>((__shared b) -> Void)
func c(UnsafeMutablePointer<UInt8>) a {
  c(&$0

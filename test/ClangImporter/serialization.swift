// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -emit-module-path %t/SerializationHelper.swiftmodule -I %S/Inputs/custom-modules %S/Inputs/SerializationHelper.swift -sdk "" -enable-objc-interop -disable-objc-attr-requires-foundation-module
// RUN: %target-swift-frontend -enable-objc-interop -typecheck -sdk "" -I %t -I %S/Inputs/custom-modules %s -verify

import SerializationHelper

let obj: InitProto = Impl(int: 42)

let impl = obj as! Impl
impl.takeStruct(testStruct(value: 0))
_ = impl.getEnum()

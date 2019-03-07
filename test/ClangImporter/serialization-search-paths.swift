// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -emit-module-path %t/SerializationHelper.swiftmodule -I %S/Inputs/custom-modules -F %S/Inputs/frameworks -sdk "" -enable-objc-interop -disable-objc-attr-requires-foundation-module %S/Inputs/SerializationHelper.swift
// RUN: %target-swift-frontend -enable-objc-interop -typecheck -I %t %s -sdk "" -verify

import SerializationHelper
import Module

let obj: InitProto = Impl(int: 42)

let impl = obj as! Impl
impl.takeStruct(testStruct(value: 0))
_ = impl.getEnum()

getModuleVersion()

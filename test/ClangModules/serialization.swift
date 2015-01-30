// RUN: rm -rf %t
// RUN: mkdir -p %t
// RUN: %target-swift-frontend -emit-module-path %t/SerializationHelper.swiftmodule -I %S/Inputs/custom-modules %S/Inputs/SerializationHelper.swift
// RUN: %target-swift-frontend -parse -I %t -I %S/Inputs/custom-modules %s -verify

// XFAIL: linux

import SerializationHelper

let obj: InitProto = Impl(int: 42)

let impl = obj as! Impl
impl.takeStruct(testStruct(value: 0))
_ = impl.getEnum()

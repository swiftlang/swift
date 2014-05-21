// RUN: rm -rf %t
// RUN: mkdir -p %t
// RUN: %swift -emit-module-path %t/SerializationHelper.swiftmodule -module-cache-path %t/clang-module-cache -I %S/Inputs/custom-modules %S/Inputs/SerializationHelper.swift
// RUN: %swift -parse -I %t -I %S/Inputs/custom-modules %s -verify

import SerializationHelper

let obj: InitProto = Impl(int: 42)

let impl = (obj as Impl)!
impl.takeStruct(testStruct(value: 0))
_ = impl.getEnum()

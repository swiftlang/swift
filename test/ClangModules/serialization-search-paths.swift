// RUN: rm -rf %t && mkdir -p %t
// RUN: %target-swift-frontend -emit-module-path %t/SerializationHelper.swiftmodule -I %S/Inputs/custom-modules -F %S/Inputs/frameworks %S/Inputs/SerializationHelper.swift
// RUN: %target-swift-frontend -parse -I %t %s -verify

import SerializationHelper
import Module

let obj: InitProto = Impl(int: 42)

let impl = obj as! Impl
impl.takeStruct(testStruct(value: 0))
_ = impl.getEnum()

getModuleVersion()
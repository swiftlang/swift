// RUN: rm -rf %t
// RUN: mkdir -p %t
// RUN: %swift -emit-module-path %t/ProtoWithInitializer.swiftmodule -module-cache-path %t/clang-module-cache -I %S/Inputs/custom-modules %S/Inputs/ProtoWithInitializer.swift
// RUN: %swift -parse -I %t -I %S/Inputs/custom-modules %s -verify

import ProtoWithInitializer

let obj: InitProto = Impl(withInt: 42)

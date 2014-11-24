// RUN: rm -rf %t
// RUN: mkdir %t
// RUN: %swift %clang-importer-sdk -emit-module -o %t %S/Inputs/def_objc_xref.swift
// RUN: %swift %clang-importer-sdk -parse -I=%t %s -verify
// XFAIL: linux

import def_objc_xref

// Trigger deserialization of the MyObjectFactorySub initializer.
let sub = MyObjectFactorySub()

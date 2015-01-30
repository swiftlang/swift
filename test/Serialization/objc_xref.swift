// RUN: rm -rf %t
// RUN: mkdir %t
// RUN: %target-swift-frontend %clang-importer-sdk -emit-module -o %t %S/Inputs/def_objc_xref.swift
// RUN: %target-swift-frontend %clang-importer-sdk -parse -I %t %s -verify

// REQUIRES: objc_interop

import def_objc_xref

// Trigger deserialization of the MyObjectFactorySub initializer.
let sub = MyObjectFactorySub()

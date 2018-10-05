// RUN: %empty-directory(%t)

// FIXME: BEGIN -enable-source-import hackaround
// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -emit-module -o %t %clang-importer-sdk-path/swift-modules/Darwin.swift
// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -emit-module -o %t %clang-importer-sdk-path/swift-modules/Foundation.swift
// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -emit-module -o %t %clang-importer-sdk-path/swift-modules/AppKit.swift
// FIXME: END -enable-source-import hackaround

// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk-nosource -I %t) -emit-module -o %t %S/Inputs/def_objc_xref.swift
// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk-nosource -I %t) -typecheck %s -verify

// REQUIRES: objc_interop

import def_objc_xref

// Trigger deserialization of the MyObjectFactorySub initializer.
let sub = MyObjectFactorySub()

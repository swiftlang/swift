// RUN: %empty-directory(%t)

// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -emit-module -o %t %S/Inputs/def_objc_conforming.swift
// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -I %t -typecheck %s -verify

// REQUIRES: objc_interop

// SR-3917
import def_objc_conforming

func test(x: Foo) { _ = x.badness }

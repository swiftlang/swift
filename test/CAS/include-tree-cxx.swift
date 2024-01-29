// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// RUN: %target-swift-frontend -scan-dependencies -module-name Test -module-cache-path %t/clang-module-cache %t/main.swift -o %t/deps.json -I %t/include -swift-version 4 -cache-compile-job -cas-path %t/cas -cxx-interoperability-mode=default

// RUN: %{python} %S/Inputs/SwiftDepsExtractor.py %t/deps.json clang:MyCXX clangIncludeTree | %FileCheck %s
// RUN: %{python} %S/Inputs/SwiftDepsExtractor.py %t/deps.json clang:MyCXX moduleCacheKey | %FileCheck %s

// CHECK: llvmcas://

//--- main.swift
import MyCXX

//--- include/module.modulemap
module MyCXX {
  header "mycxx.h"
  requires cplusplus
}

//--- include/mycxx.h
#pragma once
class A {};

void test(const A& str);

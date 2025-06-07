// RUN: %empty-directory(%t)
// RUN: %empty-directory(%t/clang-module-cache)

// RUN: %target-swift-frontend -scan-dependencies -module-cache-path %t/clang-module-cache %s -o %t/deps.json -I %S/Inputs/CHeaders -I %S/Inputs/Swift -module-name TautologicalModule -verify
// Check the contents of the JSON output
// RUN: %validate-json %t/deps.json | %FileCheck %s

import TautologicalModule // expected-warning {{file 'tautological_import.swift' is part of module 'TautologicalModule'; ignoring import}}

// CHECK: "mainModuleName": "TautologicalModule"

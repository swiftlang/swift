// Verify that the -emit-pcm command forwards C++ interop even when no VFS
// files are injected (e.g. on Darwin), since the Swift lookup tables written
// into the PCM depend on it.

// RUN: %empty-directory(%t)
// RUN: split-file %s %t
// RUN: mkdir -p %t/module-cache

// Use -module-name Cxx to skip implicitly importing the Cxx overlays.
// RUN: %target-swift-frontend -scan-dependencies -module-name Cxx -parse-stdlib \
// RUN:   -disable-implicit-string-processing-module-import \
// RUN:   -disable-implicit-concurrency-module-import \
// RUN:   -cxx-interoperability-mode=default -I %t/include \
// RUN:   -module-cache-path %t/module-cache %t/main.swift -o %t/deps.json
// RUN: %{python} %S/../CAS/Inputs/BuildCommandExtractor.py %t/deps.json clang:Foo > %t/Foo.cmd
// RUN: %FileCheck %s --input-file=%t/Foo.cmd

// CHECK: "-cxx-interoperability-mode=default"

//--- main.swift
import Foo

//--- include/module.modulemap
module Foo {
  header "Foo.h"
  export *
}

//--- include/Foo.h
#pragma once
struct S { enum { kNested = 1 } e; };

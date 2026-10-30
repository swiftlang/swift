// REQUIRES: CODEGENERATOR=X86
// UNSUPPORTED: OS=windows-msvc

// Verify that the -emit-pcm command for 'std' forwards C++ interop so
// ClangImporter injects the libstdc++ module map.

// RUN: %empty-directory(%t)
// RUN: split-file %s %t
// RUN: cp -R %S/Inputs/fake-libstdcxx-sdk/sdk %S/Inputs/fake-libstdcxx-sdk/resources %t/
// RUN: mkdir -p %t/module-cache

// Use -module-name Cxx to skip implicitly importing the Cxx overlays.
// RUN: %swift_frontend_plain -scan-dependencies -module-name Cxx -parse-stdlib \
// RUN:   -target x86_64-unknown-linux-gnu -sdk %t/sdk -resource-dir %t/resources \
// RUN:   -cxx-interoperability-mode=default -module-cache-path %t/module-cache \
// RUN:   %t/main.swift -o %t/deps.json
// RUN: %{python} %S/../CAS/Inputs/BuildCommandExtractor.py %t/deps.json clang:std > %t/std.cmd
// RUN: %FileCheck %s --input-file=%t/std.cmd

// CHECK: "-cxx-interoperability-mode=default"
// CHECK: "-resource-dir"{{$}}
// CHECK-NEXT: "{{.*}}/resources"{{$}}

// RUN: %swift_frontend_plain @%t/std.cmd

//--- main.swift
import std

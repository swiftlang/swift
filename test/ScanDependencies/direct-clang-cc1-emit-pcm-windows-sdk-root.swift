// REQUIRES: CODEGENERATOR=X86
// UNSUPPORTED: OS=windows-msvc

// ClangImporter injects module maps into the Windows SDK and Visual C++ tools
// directories that the scan located from '-windows-sdk-root' and
// '-visualc-tools-root'. The -emit-pcm command reported for each Clang module
// must carry those options, so that it injects the same module maps.

// RUN: %empty-directory(%t)
// RUN: split-file %s %t
// RUN: mkdir -p %t/module-cache %t/sdk/Include/10.0.22621.0/um %t/sdk/Include/10.0.22621.0/shared
// RUN: mkdir -p %t/sdk/Lib/10.0.22621.0/ucrt/x64 %t/sdk/Lib/10.0.22621.0/um/x64 %t/vc/lib/x64

// RUN: %swift_frontend_plain -scan-dependencies -module-name Test -parse-stdlib \
// RUN:   -target x86_64-unknown-windows-msvc -resource-dir %t/res \
// RUN:   -windows-sdk-root %t/sdk -windows-sdk-version 10.0.22621.0 \
// RUN:   -visualc-tools-root %t/vc -visualc-tools-version 14.42.34433 \
// RUN:   -module-cache-path %t/module-cache %t/main.swift -o %t/deps.json
// RUN: %{python} %S/../CAS/Inputs/BuildCommandExtractor.py %t/deps.json clang:vcruntime > %t/vcruntime.cmd
// RUN: %FileCheck %s --input-file=%t/vcruntime.cmd

// CHECK-DAG: "-windows-sdk-root"
// CHECK-DAG: "-windows-sdk-version"
// CHECK-DAG: "-visualc-tools-root"
// CHECK-DAG: "-visualc-tools-version"

// RUN: %swift_frontend_plain @%t/vcruntime.cmd

//--- main.swift
import vcruntime

//--- res/windows/vcruntime.modulemap
module vcruntime [system] {
  header "vcruntime.h"
  export *
}

//--- vc/include/vcruntime.h
#pragma once
typedef unsigned long long size_t_like;

//--- sdk/Include/10.0.22621.0/ucrt/corecrt.h
#pragma once

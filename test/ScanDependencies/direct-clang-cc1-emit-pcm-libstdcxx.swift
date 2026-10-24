// REQUIRES: CODEGENERATOR=X86
// UNSUPPORTED: OS=windows-msvc

// Verify that the -emit-pcm command for 'std' forwards C++ interop and Clang
// driver arguments (like '--gcc-toolchain') so ClangImporter injects the same
// libstdc++ module map as the scan.

// RUN: %empty-directory(%t)
// RUN: split-file %s %t
// RUN: mkdir -p %t/module-cache

// Use -module-name Cxx to skip implicitly importing the Cxx overlays.
// RUN: %swift_frontend_plain -scan-dependencies -module-name Cxx -parse-stdlib \
// RUN:   -target x86_64-unknown-linux-gnu -sdk %t/sdk -resource-dir %t/resources \
// RUN:   -cxx-interoperability-mode=default -Xcc --gcc-toolchain=%t/gcc \
// RUN:   -module-cache-path %t/module-cache %t/main.swift -o %t/deps.json
// RUN: %{python} %S/../CAS/Inputs/BuildCommandExtractor.py %t/deps.json clang:std > %t/std.cmd
// RUN: %FileCheck %s --input-file=%t/std.cmd

// CHECK-DAG: "-cxx-interoperability-mode=default"
// CHECK-DAG: "-direct-clang-cc1-driver-arg"
// CHECK-DAG: "--gcc-toolchain={{.*}}gcc"
// CHECK-DAG: "-resource-dir"

// RUN: %swift_frontend_plain @%t/std.cmd

//--- main.swift
import std

//--- sdk/usr/include/inttypes.h
//--- sdk/usr/include/stdint.h
//--- sdk/usr/include/unistd.h
//--- gcc/lib/gcc/x86_64-linux-gnu/11/crtbegin.o

//--- gcc/include/c++/11/cstdlib
#pragma once
namespace std { inline int cstdlibMarker() { return 1; } }

//--- gcc/include/c++/11/string
#pragma once
namespace std { inline int stringMarker() { return 1; } }

//--- gcc/include/c++/11/vector
#pragma once
namespace std { inline int vectorMarker() { return 1; } }

//--- resources/linux/libstdcxx.h
#pragma once

//--- resources/linux/libstdcxx.modulemap
module std {
  header "libstdcxx.h"
  header "cstdlib"
  header "string"
  header "vector"
  /// additional headers.
  requires cplusplus
  export *
}

// REQUIRES: CODEGENERATOR=X86
// UNSUPPORTED: OS=windows-msvc

// ClangImporter injects the libstdc++ module map into its in-memory file
// system only when C++ interop is enabled. The -emit-pcm command reported for
// the 'std' module must carry the C++ interop mode, so that it injects the same
// module map that the scanner resolved the module against.

// RUN: %empty-directory(%t)
// RUN: split-file %s %t
// RUN: mkdir -p %t/module-cache

// Naming the main module Cxx keeps the scanner from importing the Cxx and
// CxxStdlib overlays, which this mock SDK does not provide.
// RUN: %swift_frontend_plain -scan-dependencies -module-name Cxx -parse-stdlib \
// RUN:   -target x86_64-unknown-linux-gnu -sdk %t/sdk -resource-dir %t/resources \
// RUN:   -cxx-interoperability-mode=default -module-cache-path %t/module-cache \
// RUN:   %t/main.swift -o %t/deps.json
// RUN: %{python} %S/../CAS/Inputs/BuildCommandExtractor.py %t/deps.json clang:std > %t/std.cmd
// RUN: %FileCheck %s --input-file=%t/std.cmd

// CHECK: "-cxx-interoperability-mode=default"

// The -emit-pcm command finds the default resource directory next to the
// compiler. Point it at the mock one the scanner used instead.
// RUN: %swift_frontend_plain @%t/std.cmd -resource-dir %t/resources

//--- main.swift
import std

//--- sdk/usr/include/inttypes.h
//--- sdk/usr/include/stdint.h
//--- sdk/usr/include/unistd.h
//--- sdk/usr/lib/gcc/x86_64-linux-gnu/11/crtbegin.o

//--- sdk/usr/include/c++/11/cstdlib
#pragma once
namespace std { inline int cstdlibMarker() { return 1; } }

//--- sdk/usr/include/c++/11/string
#pragma once
namespace std { inline int stringMarker() { return 1; } }

//--- sdk/usr/include/c++/11/vector
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

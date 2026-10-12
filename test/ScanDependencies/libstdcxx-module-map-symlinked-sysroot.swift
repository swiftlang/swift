// REQUIRES: CODEGENERATOR=X86
// UNSUPPORTED: OS=windows-msvc

// In a sysroot whose 'lib' links to 'usr/lib', as in merged-/usr Linux
// distributions, Clang finds the GCC installation through the link, so it
// reaches libstdc++ as
// '<sysroot>/lib/gcc/<triple>/<version>/../../../../include/c++/<version>'.
// That is '<sysroot>/usr/include/c++/<version>' on disk, but ClangImporter
// injects the libstdc++ module map into the directory without the '..'
// components, '<sysroot>/include/c++/<version>', which does not exist.
//
// The scanner reports the module map at that path, so the -emit-pcm command
// for 'std' must still find the headers that the module map names, and the
// headers on disk must decide which additional headers the module map lists.

// RUN: %empty-directory(%t)
// RUN: split-file %s %t
// RUN: ln -s usr/lib %t/sdk/lib
// RUN: mkdir -p %t/module-cache %t/implicit-module-cache

// Naming the main module Cxx keeps the scanner from importing the Cxx and
// CxxStdlib overlays, which this mock SDK does not provide.
// RUN: %swift_frontend_plain -scan-dependencies -module-name Cxx -parse-stdlib \
// RUN:   -target x86_64-unknown-linux-gnu -sdk %t/sdk -resource-dir %t/resources \
// RUN:   -cxx-interoperability-mode=default -I %t/include \
// RUN:   -module-cache-path %t/module-cache %t/main.swift -o %t/deps.json
// RUN: %FileCheck %s --input-file=%t/deps.json --check-prefix=DEPS

// DEPS: "moduleMapPath": "{{.*}}/sdk/include/c++/11/module.modulemap"

// RUN: %{python} %S/../CAS/Inputs/BuildCommandExtractor.py %t/deps.json clang:std > %t/std.cmd
// RUN: %swift_frontend_plain @%t/std.cmd
// RUN: %{python} %S/../CAS/Inputs/BuildCommandExtractor.py %t/deps.json clang:Foo > %t/Foo.cmd
// RUN: %swift_frontend_plain @%t/Foo.cmd

// An implicit build finds the module map through the include path.
// RUN: %swift_frontend_plain -typecheck -module-name Cxx -parse-stdlib \
// RUN:   -target x86_64-unknown-linux-gnu -sdk %t/sdk -resource-dir %t/resources \
// RUN:   -cxx-interoperability-mode=default -I %t/include \
// RUN:   -module-cache-path %t/implicit-module-cache %t/main.swift \
// RUN:   -dump-clang-diagnostics 2>&1 | %FileCheck %s --check-prefix=MODULEMAP

// MODULEMAP: clang importer overriding file '{{.*}}/sdk/include/c++/11/module.modulemap' with the following contents:
// MODULEMAP-NEXT: module std {
// MODULEMAP: header "optional"
// MODULEMAP: /// additional headers.

//--- main.swift
import Foo

//--- include/module.modulemap
module Foo {
  header "Foo.h"
  export *
}

//--- include/Foo.h
#pragma once
#include <vector>

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

//--- sdk/usr/include/c++/11/optional
#pragma once
namespace std { inline int optionalMarker() { return 1; } }

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

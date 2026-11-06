// REQUIRES: CODEGENERATOR=X86
// UNSUPPORTED: OS=windows-msvc

// Verify that when a sysroot symlink (such as 'lib -> usr/lib') causes dot
// removal on the GCC include path to land at '<sysroot>/include/c++/11' instead
// of '<sysroot>/usr/include/c++/11', ClangImporter remaps the injected module
// map directory to the real directory on disk.

// RUN: %empty-directory(%t)
// RUN: split-file %s %t
// RUN: cp -R %S/Inputs/fake-libstdcxx-sdk/sdk %S/Inputs/fake-libstdcxx-sdk/resources %t/
// RUN: ln -s usr/lib %t/sdk/lib
// RUN: mkdir -p %t/module-cache %t/implicit-module-cache

// Use -module-name Cxx to skip implicitly importing the Cxx overlays.
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

// Verify that additional headers on disk (e.g. 'optional') are still injected.
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

//--- sdk/usr/include/c++/11/optional
#pragma once
namespace std { inline int optionalMarker() { return 1; } }

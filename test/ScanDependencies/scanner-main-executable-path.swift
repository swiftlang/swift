// REQUIRES: CODEGENERATOR=X86
// UNSUPPORTED: OS=windows-msvc

// Verify that setting the main executable path on a scan invocation allows the
// scanner to find a GCC installation relative to the compiler binary.

// RUN: %empty-directory(%t)
// RUN: split-file %s %t
// RUN: mkdir -p %t/module-cache-with-path %t/module-cache-without-path

// RUN: %swift-scan-test -action scan_dependency \
// RUN:   -main-executable-path %t/toolchain/bin/swift-frontend -- \
// RUN:   %swift_frontend_plain -module-name Cxx -parse-stdlib \
// RUN:   -target x86_64-unknown-linux-gnu -resource-dir %t/resources \
// RUN:   -cxx-interoperability-mode=default -I %t/include \
// RUN:   -module-cache-path %t/module-cache-with-path %t/main.swift \
// RUN:   > %t/deps-with-path.json
// RUN: %FileCheck %s --input-file=%t/deps-with-path.json --check-prefix=WITH-PATH

// WITH-PATH: "moduleMapPath": "{{.*}}/toolchain/include/c++/11/module.modulemap"

// RUN: %swift-scan-test -action scan_dependency -- \
// RUN:   %swift_frontend_plain -module-name Cxx -parse-stdlib \
// RUN:   -target x86_64-unknown-linux-gnu -resource-dir %t/resources \
// RUN:   -cxx-interoperability-mode=default -I %t/include \
// RUN:   -module-cache-path %t/module-cache-without-path %t/main.swift \
// RUN:   > %t/deps-without-path.json
// RUN: %FileCheck %s --input-file=%t/deps-without-path.json --check-prefix=WITHOUT-PATH

// WITHOUT-PATH-NOT: toolchain/include/c++

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

//--- toolchain/bin/swift-frontend
//--- toolchain/lib/gcc/x86_64-linux-gnu/11/crtbegin.o

//--- toolchain/include/c++/11/cstdlib
#pragma once
namespace std { inline int cstdlibMarker() { return 1; } }

//--- toolchain/include/c++/11/string
#pragma once
namespace std { inline int stringMarker() { return 1; } }

//--- toolchain/include/c++/11/vector
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
  requires cplusplus
  export *
}

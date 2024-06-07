// RUN: %empty-directory(%t)
// RUN: split-file %s %t
// RUN: %target-swift-frontend %t/test.swift -c -enable-experimental-cxx-interop -Xcc -fmodules-cache-path=%t/cache -I %t/Inputs
// RUN: find %t/cache | %FileCheck %s

// REQUIRES: OS=macosx || OS=linux-gnu

//--- Inputs/module.modulemap
module CxxModule {
    header "header.h"
    requires cplusplus
}

//--- Inputs/header.h

#include <string_view>

//--- test.swift

import CxxModule

// CHECK: {{std|std_string_view}}-{{.*}}.pcm

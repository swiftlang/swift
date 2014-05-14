// RUN: not %swift -i -emit-module %s 2>&1 | FileCheck -check-prefix=IMMEDIATE_NO_MODULE %s
// RUN: not %swift -integrated-repl -emit-module 2>&1 | FileCheck -check-prefix=IMMEDIATE_NO_MODULE %s
// REQUIRES: swift_interpreter
// IMMEDIATE_NO_MODULE: this mode does not support emitting modules

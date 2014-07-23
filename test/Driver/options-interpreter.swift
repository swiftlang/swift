// RUN: not %swift_driver -emit-module -i %s 2>&1 | FileCheck -check-prefix=IMMEDIATE_NO_MODULE %s
// RUN: not %swift_driver -integrated-repl -emit-module 2>&1 | FileCheck -check-prefix=IMMEDIATE_NO_MODULE %s
// RUN: not %swifti_driver -emit-module 2>&1 | FileCheck -check-prefix=IMMEDIATE_NO_MODULE %s
// REQUIRES: swift_interpreter
// IMMEDIATE_NO_MODULE: this mode does not support emitting modules

// RUN: not %swift_driver %s -i 2>&1 | FileCheck -check-prefix=NO-FILE %s
// RUN: not %swift_driver -i 2>&1 | FileCheck -check-prefix=NO-FILE %s
// NO-FILE: expected input file after '-i'

// RUN: %swift_driver -### -i %s | FileCheck -check-prefix INTERPRET %s
// INTERPRET: -interpret

// RUN: %swift_driver -### -i %s a b c | FileCheck -check-prefix ARGS %s
// ARGS: -- a b c

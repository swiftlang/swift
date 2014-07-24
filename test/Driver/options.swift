// rdar://13723332 Crash on -emit-sil with no input files
// RUN: not %swift -emit-sil 2>&1 | FileCheck -check-prefix=NO_FILES %s
// RUN: not %swift -emit-sil -parse-as-library 2>&1 | FileCheck -check-prefix=NO_FILES %s
// NO_FILES: this mode requires at least one input file

// RUN: not %swift -parse-sil -emit-sil 2>&1 | FileCheck -check-prefix=SIL_FILES %s
// RUN: not %swift -parse-sil -emit-sil %s %s 2>&1 | FileCheck -check-prefix=SIL_FILES %s
// SIL_FILES: this mode requires a single input file

// RUN: not %swift -emit-silgen -parse-as-library %S/Inputs/invalid-module-name.swift 2>&1 | FileCheck -check-prefix=INVALID_MODULE_NAME %s
// INVALID_MODULE_NAME: error: module name "invalid-module-name" is not a valid identifier; use -module-name flag to specify an alternate name

// RUN: not %swift -emit-silgen -parse-as-library %S/Inputs/invalid.module.name.swift 2>&1 | FileCheck -check-prefix=INVALID_MODULE_NAME2 %s
// INVALID_MODULE_NAME2: error: module name "invalid.module.name" is not a valid identifier; use -module-name flag to specify an alternate name

// RUN: not %swift -emit-silgen -parse-as-library %S/Inputs/invalid-module-name.swift -module-name still-invalid 2>&1 | FileCheck -check-prefix=STILL_INVALID %s
// STILL_INVALID: error: module name "still-invalid" is not a valid identifier{{$}}

// RUN: not %swift_driver -emit-silgen -parse-as-library %s -module-name "Swift" 2>&1 | FileCheck -check-prefix=STDLIB_MODULE %s
// RUN: %swift_driver -emit-silgen -parse-as-library %s -module-name "Swift" -parse-stdlib -###
// STDLIB_MODULE: error: module name "Swift" is reserved for the standard library{{$}}

// RUN: not %swift -parse -emit-module %s 2>&1 | FileCheck -check-prefix=PARSE_NO_MODULE %s
// PARSE_NO_MODULE: error: this mode does not support emitting modules{{$}}

// RUN: not %swift -dump-ast -emit-dependencies %s 2>&1 | FileCheck -check-prefix=DUMP_NO_DEPS %s
// DUMP_NO_DEPS: error: this mode does not support emitting dependency files{{$}}

// Should not fail with non-zero exit code.
// RUN: %swift -emit-silgen %S/Inputs/invalid-module-name.swift > /dev/null
// RUN: %swift -emit-silgen -parse-as-library %S/Inputs/invalid-module-name.swift -module-name foo > /dev/null
// RUN: %swift -parse -parse-as-library %S/Inputs/invalid-module-name.swift -module-name foo

// RUN: not %swift_driver -crazy-option-that-does-not-exist 2>&1 | FileCheck -check-prefix=INVALID_OPTION %s
// INVALID_OPTION: error: unknown argument: '-crazy-option-that-does-not-exist'

// RUN: %swift_driver -assert-config Debug -### | FileCheck -check-prefix=ASSERTCONFIG %s
// ASSERTCONFIG: -assert-config Debug

// RUN: %swift_driver -assert-config Debug -### | FileCheck -check-prefix=ASSERTCONFIG2 %s
// ASSERTCONFIG2: -assert-config Debug

// RUN: %swift_driver -assert-config Release -### | FileCheck -check-prefix=ASSERTCONFIG3 %s
// ASSERTCONFIG3: -assert-config Release

// RUN: %swift_driver -assert-config Release -### | FileCheck -check-prefix=ASSERTCONFIG4 %s
// ASSERTCONFIG4: -assert-config Release

// RUN: %swift_driver -assert-config DisableReplacement -### | FileCheck -check-prefix=ASSERTCONFIG5 %s
// ASSERTCONFIG5: -assert-config DisableReplacement

// RUN: %swift_driver -assert-config DisableReplacement -### | FileCheck -check-prefix=ASSERTCONFIG6 %s
// ASSERTCONFIG6: -assert-config DisableReplacement

// RUN: not %swift_driver -import-objc-header fake.h -import-underlying-module -c %s 2>&1 | FileCheck -check-prefix=FRAMEWORK_BRIDGING_HEADER %s
// FRAMEWORK_BRIDGING_HEADER: error: using bridging headers with framework targets is unsupported

// RUN: %swift_driver -target arm64-apple-ios7 -### | FileCheck -check-prefix=TARGETCPU1 %s
// TARGETCPU1: -target-cpu cyclone

// RUN: %swift_driver -target armv7s-apple-ios7 -### | FileCheck -check-prefix=TARGETCPU2 %s
// TARGETCPU2: -target-cpu swift

// RUN: %swift_driver -target armv7-apple-ios7 -### | FileCheck -check-prefix=TARGETCPU3 %s
// TARGETCPU3: -target-cpu cortex-a8

// RUN: %swift_driver -target i386-apple-ios7 -### | FileCheck -check-prefix=SIMULATOR_CPU %s
// SIMULATOR_CPU: -target-cpu yonah

// RUN: %swift_driver -target x86_64-apple-ios7 -### | FileCheck -check-prefix=SIMULATOR64_CPU %s
// SIMULATOR64_CPU: -target-cpu core2

// RUN: %swifti_driver -### | FileCheck -check-prefix=DEFAULT_REPL %s
// DEFAULT_REPL: -repl

// RUN: %swifti_driver -repl -### 2>&1 | FileCheck -check-prefix=REPL %s
// REPL: warning: unnecessary option '-repl'

// RUN: %swifti_driver -lldb-repl -### 2>&1 | FileCheck -check-prefix=LLDB_REPL %s
// LLDB_REPL-NOT: warning
// LLDB_REPL: lldb
// LLDB_REPL-NOT: warning

// RUN: %swifti_driver -integrated-repl -### 2>&1 | FileCheck -check-prefix=INTEGRATED_REPL %s
// INTEGRATED_REPL-NOT: warning
// INTEGRATED_REPL: -repl
// INTEGRATED_REPL-NOT: warning

// RUN: %swifti_driver -### %s | FileCheck -check-prefix=DEFAULT_I %s
// DEFAULT_I: -interpret

// RUN: %swifti_driver -### -i %s 2>&1 | FileCheck -check-prefix=I_MODE %s
// I_MODE: warning: unnecessary option '-i'
// I_MODE: -interpret

// RUN: not %swifti_driver -### -c %s 2>&1 | FileCheck -check-prefix=C_MODE %s
// C_MODE: error: unsupported option '-c'
// RUN: not %swifti_driver -### -emit-object %s 2>&1 | FileCheck -check-prefix=OBJ_MODE %s
// OBJ_MODE: error: unsupported option '-emit-object'
// RUN: not %swifti_driver -### -emit-executable %s 2>&1 | FileCheck -check-prefix=EXEC_MODE %s
// EXEC_MODE: error: unsupported option '-emit-executable'
// RUN: not %swifti_driver -### -o %t %s 2>&1 | FileCheck -check-prefix=ARG_o %s
// ARG_o: error: unsupported option '-o'

// RUN: not %swiftc_driver -### -i %s 2>&1 | FileCheck -check-prefix=I_MODE_SWIFTC %s
// I_MODE_SWIFTC: error: unsupported option '-i'
// RUN: not %swiftc_driver -### -repl 2>&1 | FileCheck -check-prefix=REPL_MODE_SWIFTC %s
// REPL_MODE_SWIFTC: error: unsupported option '-repl'
// RUN: not %swiftc_driver -### -lldb-repl 2>&1 | FileCheck -check-prefix=LLDB_REPL_MODE_SWIFTC %s
// LLDB_REPL_MODE_SWIFTC: error: unsupported option '-lldb-repl'
// RUN: not %swiftc_driver -### -integrated-repl 2>&1 | FileCheck -check-prefix=INT_REPL_MODE_SWIFTC %s
// INT_REPL_MODE_SWIFTC: error: unsupported option '-integrated-repl'

// RUN: %swifti_driver -g -### %s 2>&1 | FileCheck -check-prefix=OPTIONS_BEFORE_FILE %s
// OPTIONS_BEFORE_FILE: -g

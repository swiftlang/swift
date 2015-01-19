// rdar://13723332 Crash on -emit-sil with no input files
// RUN: not %target-swift-frontend -emit-sil 2>&1 | FileCheck -check-prefix=NO_FILES %s
// RUN: not %target-swift-frontend -emit-sil -parse-as-library 2>&1 | FileCheck -check-prefix=NO_FILES %s
// NO_FILES: this mode requires at least one input file

// RUN: not %target-swift-frontend -parse-sil -emit-sil 2>&1 | FileCheck -check-prefix=SIL_FILES %s
// RUN: not %target-swift-frontend -parse-sil -emit-sil %s %s 2>&1 | FileCheck -check-prefix=SIL_FILES %s
// SIL_FILES: this mode requires a single input file

// RUN: not %target-swift-frontend -emit-silgen -parse-as-library %S/Inputs/invalid-module-name.swift 2>&1 | FileCheck -check-prefix=INVALID_MODULE_NAME %s
// INVALID_MODULE_NAME: error: module name "invalid-module-name" is not a valid identifier; use -module-name flag to specify an alternate name

// RUN: not %target-swift-frontend -emit-silgen -parse-as-library %S/Inputs/invalid.module.name.swift 2>&1 | FileCheck -check-prefix=INVALID_MODULE_NAME2 %s
// INVALID_MODULE_NAME2: error: module name "invalid.module.name" is not a valid identifier; use -module-name flag to specify an alternate name

// RUN: not %target-swift-frontend -emit-silgen -parse-as-library %S/Inputs/invalid-module-name.swift -module-name still-invalid 2>&1 | FileCheck -check-prefix=STILL_INVALID %s
// STILL_INVALID: error: module name "still-invalid" is not a valid identifier{{$}}

// RUN: not %target-swiftc_driver -emit-silgen -parse-as-library %s -module-name "Swift" 2>&1 | FileCheck -check-prefix=STDLIB_MODULE %s
// RUN: %target-swiftc_driver -emit-silgen -parse-as-library %s -module-name "Swift" -parse-stdlib -###
// STDLIB_MODULE: error: module name "Swift" is reserved for the standard library{{$}}

// RUN: not %target-swift-frontend -parse -emit-module %s 2>&1 | FileCheck -check-prefix=PARSE_NO_MODULE %s
// PARSE_NO_MODULE: error: this mode does not support emitting modules{{$}}

// RUN: not %target-swift-frontend -dump-ast -emit-dependencies %s 2>&1 | FileCheck -check-prefix=DUMP_NO_DEPS %s
// DUMP_NO_DEPS: error: this mode does not support emitting dependency files{{$}}

// Should not fail with non-zero exit code.
// RUN: %target-swift-frontend -emit-silgen %S/Inputs/invalid-module-name.swift > /dev/null
// RUN: %target-swift-frontend -emit-silgen -parse-as-library %S/Inputs/invalid-module-name.swift -module-name foo > /dev/null
// RUN: %target-swift-frontend -parse -parse-as-library %S/Inputs/invalid-module-name.swift -module-name foo

// RUN: not %swiftc_driver -crazy-option-that-does-not-exist %s 2>&1 | FileCheck -check-prefix=INVALID_OPTION %s
// RUN: not %swift_driver -crazy-option-that-does-not-exist 2>&1 | FileCheck -check-prefix=INVALID_OPTION %s
// INVALID_OPTION: error: unknown argument: '-crazy-option-that-does-not-exist'

// RUN: %swiftc_driver -assert-config Debug -### %s | FileCheck -check-prefix=ASSERTCONFIG %s
// RUN: %swift_driver -assert-config Debug -### | FileCheck -check-prefix=ASSERTCONFIG %s
// ASSERTCONFIG: -assert-config Debug

// RUN: %swiftc_driver -assert-config Debug -### %s | FileCheck -check-prefix=ASSERTCONFIG2 %s
// ASSERTCONFIG2: -assert-config Debug

// RUN: %swiftc_driver -assert-config Release -### %s | FileCheck -check-prefix=ASSERTCONFIG3 %s
// ASSERTCONFIG3: -assert-config Release

// RUN: %swiftc_driver -assert-config Release -### %s | FileCheck -check-prefix=ASSERTCONFIG4 %s
// ASSERTCONFIG4: -assert-config Release

// RUN: %swiftc_driver -assert-config DisableReplacement -### %s | FileCheck -check-prefix=ASSERTCONFIG5 %s
// ASSERTCONFIG5: -assert-config DisableReplacement

// RUN: %swiftc_driver -assert-config DisableReplacement -### %s | FileCheck -check-prefix=ASSERTCONFIG6 %s
// ASSERTCONFIG6: -assert-config DisableReplacement

// RUN: not %swiftc_driver -import-objc-header fake.h -import-underlying-module -c %s 2>&1 | FileCheck -check-prefix=FRAMEWORK_BRIDGING_HEADER %s
// FRAMEWORK_BRIDGING_HEADER: error: using bridging headers with framework targets is unsupported

// RUN: %swift_driver -### | FileCheck -check-prefix=DEFAULT_REPL %s
// DEFAULT_REPL: -repl
// RUN: not %swiftc_driver 2>&1 | FileCheck -check-prefix=DEFAULT_EXEC_ERR  %s
// DEFAULT_EXEC_ERR: error: no input files
// RUN: %swiftc_driver %s -### 2>&1 | FileCheck -check-prefix=DEFAULT_EXEC  %s
// DEFAULT_EXEC: -c
// DEFAULT_EXEC: ld

// RUN: %swift_driver -repl -### 2>&1 | FileCheck -check-prefix=REPL %s
// REPL: warning: unnecessary option '-repl'

// RUN: %swift_driver -lldb-repl -### 2>&1 | FileCheck -check-prefix=LLDB_REPL %s
// LLDB_REPL-NOT: warning
// LLDB_REPL: lldb
// LLDB_REPL-NOT: warning

// RUN: %swift_driver -deprecated-integrated-repl -### 2>&1 | FileCheck -check-prefix=INTEGRATED_REPL %s
// INTEGRATED_REPL-NOT: warning
// INTEGRATED_REPL: -repl
// INTEGRATED_REPL-NOT: warning

// RUN: %swift_driver -### %s | FileCheck -check-prefix=DEFAULT_I %s
// DEFAULT_I: -interpret

// RUN: not %swift_driver -### -i %s 2>&1 | FileCheck -check-prefix=I_MODE %s
// RUN: not %swift_driver -### -i 2>&1 | FileCheck -check-prefix=I_MODE %s
// RUN: not %swiftc_driver -### -i %s 2>&1 | FileCheck -check-prefix=I_MODE %s
// RUN: not %swiftc_driver -### -i 2>&1 | FileCheck -check-prefix=I_MODE %s
// I_MODE: error: the flag '-i' is no longer required and has been removed; use 'swift input-filename'

// RUN: not %swift_driver -### -c %s 2>&1 | FileCheck -check-prefix=C_MODE %s
// C_MODE: error: unsupported option '-c'
// RUN: not %swift_driver -### -emit-object %s 2>&1 | FileCheck -check-prefix=OBJ_MODE %s
// OBJ_MODE: error: unsupported option '-emit-object'
// RUN: not %swift_driver -### -emit-executable %s 2>&1 | FileCheck -check-prefix=EXEC_MODE %s
// EXEC_MODE: error: unsupported option '-emit-executable'
// RUN: not %swift_driver -### -o %t %s 2>&1 | FileCheck -check-prefix=ARG_o %s
// ARG_o: error: unsupported option '-o'

// RUN: not %swiftc_driver -### -repl 2>&1 | FileCheck -check-prefix=REPL_MODE_SWIFTC %s
// REPL_MODE_SWIFTC: error: unsupported option '-repl'
// RUN: not %swiftc_driver -### -lldb-repl 2>&1 | FileCheck -check-prefix=LLDB_REPL_MODE_SWIFTC %s
// LLDB_REPL_MODE_SWIFTC: error: unsupported option '-lldb-repl'
// RUN: not %swiftc_driver -### -deprecated-integrated-repl 2>&1 | FileCheck -check-prefix=INT_REPL_MODE_SWIFTC %s
// INT_REPL_MODE_SWIFTC: error: unsupported option '-deprecated-integrated-repl'

// RUN: %swift_driver -g -### %s 2>&1 | FileCheck -check-prefix=OPTIONS_BEFORE_FILE %s
// OPTIONS_BEFORE_FILE: -g

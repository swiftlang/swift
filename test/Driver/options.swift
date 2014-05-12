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

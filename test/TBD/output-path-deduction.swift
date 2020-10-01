// REQUIRES: VENDOR=apple 
// RUN: %empty-directory(%t)

// RUN: cd %t; %target-build-swift -emit-module -emit-tbd %s
// RUN: test -e %t/main.tbd
// RUN: %target-build-swift -emit-module -emit-tbd %s -o %t/default
// RUN: test -e %t/default.tbd
// RUN: %target-build-swift -emit-module -emit-tbd %s -o %t/module_name -module-name module_name_different
// RUN: test -e %t/module_name.tbd
// RUN: %target-build-swift -emit-module -emit-tbd-path %t/hard_to_guess_explicit_path.tbd %s -o %t/explicit_path
// RUN: test -e %t/hard_to_guess_explicit_path.tbd
// RUN: %target-build-swift -emit-module -emit-tbd %s -emit-module-path %t/emit_module_path.swiftmodule -module-name emit_module_path_different
// RUN: test -e %t/emit_module_path.tbd

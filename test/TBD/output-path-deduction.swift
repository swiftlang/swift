// REQUIRES: VENDOR=apple 
// RUN: %empty-directory(%t)

// RUN: cd %t; %target-build-swift -emit-module -emit-tbd %s -Xfrontend -tbd-install_name -Xfrontend test
// RUN: test -e %t/main.tbd
// RUN: %target-build-swift -emit-module -emit-tbd %s -o %t/default -Xfrontend -tbd-install_name -Xfrontend test
// RUN: test -e %t/default.tbd
// RUN: %target-build-swift -emit-module -emit-tbd %s -o %t/module_name -module-name module_name_different -Xfrontend -tbd-install_name -Xfrontend test
// RUN: test -e %t/module_name.tbd
// RUN: %target-build-swift -emit-module -emit-tbd-path %t/hard_to_guess_explicit_path.tbd %s -o %t/explicit_path -Xfrontend -tbd-install_name -Xfrontend test
// RUN: test -e %t/hard_to_guess_explicit_path.tbd
// RUN: %target-build-swift -emit-module -emit-tbd %s -emit-module-path %t/emit_module_path.swiftmodule -module-name emit_module_path_different -Xfrontend -tbd-install_name -Xfrontend test
// RUN: test -e %t/emit_module_path.tbd

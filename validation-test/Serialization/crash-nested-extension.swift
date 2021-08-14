// RUN: %empty-directory(%t)
// RUN: cp -r %S/Inputs/crash-nested-extension/* %t
// RUN: cd %t
// RUN: %swiftc_driver -output-file-map %t/output.json -incremental ./f1.swift ./f2.swift -module-name MyMod -j1 -disable-batch-mode -emit-module -emit-module-path MyMod.swiftmodule -v

// RUN: %empty-directory(%t)
// RUN: cp -r %S/Inputs/crash-nested-extension/* %t
// RUN: cd %t
// RUN: %swiftc_driver -output-file-map %t/output.json -incremental -Xfrontend -experimental-skip-all-function-bodies -Xfrontend -experimental-allow-module-with-compiler-errors ./f1.swift ./f2.swift -module-name MyMod -j1 -disable-batch-mode -emit-module -emit-module-path MyMod.swiftmodule -v

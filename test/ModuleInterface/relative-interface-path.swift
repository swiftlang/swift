// RUN: %empty-directory(%t.relative_interface_path)
// RUN: cp -R %S/Inputs/relative_path %t.relative_interface_path/
// RUN: cd %t.relative_interface_path
// RUN: mkdir out

// RUN: %target-swift-frontend -target arm64-apple-macosx15.0 -compile-module-from-interface \
// RUN:   relative_path/B.swiftmodule/arm64-apple-macos.swiftinterface -o out/B.swiftmodule
// RUN: %target-swift-frontend -target arm64-apple-macosx15.0 -compile-module-from-interface \
// RUN:   relative_path/A.swiftmodule/arm64-apple-macos.swiftinterface -o out/A.swiftmodule -I out

// REQUIRES: OS=macosx

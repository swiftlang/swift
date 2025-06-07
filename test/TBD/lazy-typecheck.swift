// RUN: %empty-directory(%t)
// RUN: %empty-directory(%t/baseline)
// RUN: %empty-directory(%t/lazy-skip-all)

// (1) Generate a baseline .tbd file.
// RUN: %target-swift-frontend -swift-version 5 %S/../Inputs/lazy_typecheck.swift -module-name lazy_typecheck -emit-module -emit-module-path /dev/null -emit-tbd-path %t/baseline/lazy_typecheck.tbd -enable-library-evolution -parse-as-library -package-name Package -DFLAG -tbd-install_name @rpath/Package

// (2) Generate a .tbd file passing -experimental-lazy-typecheck, -experimental-skip-all-function-bodies,
//     and -experimental-skip-non-exportable-decls. It should be identical to the baseline and avoid
//     triggering typechecking for any "NoTypecheck" decls.
// RUN: %target-swift-frontend -swift-version 5 %S/../Inputs/lazy_typecheck.swift -module-name lazy_typecheck -emit-module -emit-module-path /dev/null -emit-tbd-path %t/lazy-skip-all/lazy_typecheck.tbd -enable-library-evolution -parse-as-library -package-name Package -DFLAG -debug-forbid-typecheck-prefix NoTypecheck  -experimental-lazy-typecheck -experimental-skip-all-function-bodies -experimental-skip-non-exportable-decls -tbd-install_name @rpath/Package
// RUN: %llvm-readtapi --compare %t/baseline/lazy_typecheck.tbd %t/lazy-skip-all/lazy_typecheck.tbd

// FIXME: Re-run the test with -experimental-skip-non-inlinable-function-bodies

// REQUIRES: VENDOR=apple

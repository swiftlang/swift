// RUN: %empty-directory(%t)
// RUN: %empty-directory(%t/baseline)
// RUN: %empty-directory(%t/lazy)
// RUN: %empty-directory(%t/lazy-skip-all)
// RUN: %empty-directory(%t/lazy-skip-non-inlinable)

// (1) Generate a baseline .swiftinterface and build a client against it.
// RUN: %target-swift-frontend -swift-version 5 %S/../Inputs/lazy_typecheck.swift -module-name lazy_typecheck -typecheck -emit-module-interface-path %t/baseline/lazy_typecheck.swiftinterface -enable-library-evolution -parse-as-library -package-name Package -DFLAG
// RUN: rm -rf %t/baseline/*.swiftmodule
// RUN: %target-swift-frontend -package-name ClientPackage -typecheck -verify %S/../Inputs/lazy_typecheck_client.swift -I %t/baseline/

// (2) Generate a .swiftinterface with -experimental-lazy-typecheck and build the client against it.
//     The .swiftinterface should be identical to the baseline and avoid triggering typechecking
//     for any "NoTypecheck" decls.
// RUN: %target-swift-frontend -swift-version 5 %S/../Inputs/lazy_typecheck.swift -module-name lazy_typecheck -typecheck -emit-module-interface-path %t/lazy/lazy_typecheck.swiftinterface -enable-library-evolution -parse-as-library -package-name Package -DFLAG -debug-forbid-typecheck-prefix NoTypecheck -experimental-lazy-typecheck
// RUN: rm -rf %t/lazy/*.swiftmodule
// RUN: %target-swift-frontend -package-name ClientPackage -typecheck -verify %S/../Inputs/lazy_typecheck_client.swift -I %t/lazy
// RUN: diff -u %t/baseline/lazy_typecheck.swiftinterface %t/lazy/lazy_typecheck.swiftinterface

// (3) Same as (2), but with -experimental-skip-all-function-bodies added.
// RUN: %target-swift-frontend -swift-version 5 %S/../Inputs/lazy_typecheck.swift -module-name lazy_typecheck -typecheck -emit-module-interface-path %t/lazy-skip-all/lazy_typecheck.swiftinterface -enable-library-evolution -parse-as-library -package-name Package -DFLAG -debug-forbid-typecheck-prefix NoTypecheck -experimental-lazy-typecheck -experimental-skip-non-exportable-decls -experimental-skip-all-function-bodies
// RUN: rm -rf %t/lazy-skip-all/*.swiftmodule
// RUN: %target-swift-frontend -package-name ClientPackage -typecheck -verify %S/../Inputs/lazy_typecheck_client.swift -I %t/lazy-skip-all
// RUN: diff -u %t/baseline/lazy_typecheck.swiftinterface %t/lazy-skip-all/lazy_typecheck.swiftinterface

// (4) Same as (2), but with -experimental-skip-non-inlinable-function-bodies added.
// RUN: %target-swift-frontend -swift-version 5 %S/../Inputs/lazy_typecheck.swift -module-name lazy_typecheck -typecheck -emit-module-interface-path %t/lazy-skip-non-inlinable/lazy_typecheck.swiftinterface -enable-library-evolution -parse-as-library -package-name Package -DFLAG -debug-forbid-typecheck-prefix NoTypecheck -experimental-lazy-typecheck -experimental-skip-non-inlinable-function-bodies
// RUN: rm -rf %t/lazy-skip-non-inlinable/*.swiftmodule
// RUN: %target-swift-frontend -package-name ClientPackage -typecheck -verify %S/../Inputs/lazy_typecheck_client.swift -I %t/lazy-skip-non-inlinable
// RUN: diff -u %t/baseline/lazy_typecheck.swiftinterface %t/lazy-skip-non-inlinable/lazy_typecheck.swiftinterface

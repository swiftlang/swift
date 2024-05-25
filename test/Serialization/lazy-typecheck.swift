// RUN: %empty-directory(%t)
// RUN: %empty-directory(%t/baseline)
// RUN: %empty-directory(%t/lazy-skip-all)

// (1) Build the module normally and verify that a client can deserialize it and use its public declarations.
// RUN: %target-swift-frontend -swift-version 5 %S/../Inputs/lazy_typecheck.swift -module-name lazy_typecheck -enable-library-evolution -parse-as-library -package-name Package -DFLAG -emit-module -emit-module-path %t/baseline/lazy_typecheck.swiftmodule
// RUN: %target-swift-frontend -package-name Package -typecheck -verify %S/../Inputs/lazy_typecheck_client.swift -DFLAG -I %t/baseline

// (2) Verify that a module built with -experimental-lazy-typecheck, -experimental-skip-all-function-bodies,
//     and -experimental-skip-non-exportable-decls can be used by the same client as in (1).
// RUN: %target-swift-frontend -swift-version 5 %S/../Inputs/lazy_typecheck.swift -module-name lazy_typecheck -enable-library-evolution -parse-as-library -package-name Package -DFLAG -emit-module -emit-module-path %t/lazy-skip-all/lazy_typecheck.swiftmodule -debug-forbid-typecheck-prefix NoTypecheck -experimental-lazy-typecheck -experimental-skip-all-function-bodies -experimental-skip-non-exportable-decls
// RUN: %target-swift-frontend -package-name Package -typecheck -verify %S/../Inputs/lazy_typecheck_client.swift -DFLAG -I %t/lazy-skip-all

// (2a) Verify that -experimental-lazy-typecheck and -experimental-skip-non-exportable-decls do not require
// -enable-library-evolution if -experimental-skip-all-function-bodies is specified.
// RUN: %target-swift-frontend -swift-version 5 %S/../Inputs/lazy_typecheck.swift -module-name lazy_typecheck -parse-as-library -package-name Package -DFLAG -emit-module -emit-module-path %t/lazy-skip-all-non-resilient/lazy_typecheck.swiftmodule -debug-forbid-typecheck-prefix NoTypecheck -experimental-lazy-typecheck -experimental-skip-all-function-bodies -experimental-skip-non-exportable-decls
// RUN: %target-swift-frontend -package-name Package -typecheck -verify %S/../Inputs/lazy_typecheck_client.swift -DFLAG -I %t/lazy-skip-all-non-resilient

// (3) Verify that a module built with -experimental-lazy-typecheck, -experimental-skip-non-inlinable-function-bodies,
//     and -experimental-skip-non-exportable-decls can be used by the same client as in (1).
// RUN: %target-swift-frontend -swift-version 5 %S/../Inputs/lazy_typecheck.swift -module-name lazy_typecheck -enable-library-evolution -parse-as-library -package-name Package -DFLAG -emit-module -emit-module-path %t/lazy-skip-non-inlinable/lazy_typecheck.swiftmodule -debug-forbid-typecheck-prefix NoTypecheck -experimental-lazy-typecheck -experimental-skip-non-inlinable-function-bodies -experimental-skip-non-exportable-decls
// RUN: %target-swift-frontend -package-name Package -typecheck -verify %S/../Inputs/lazy_typecheck_client.swift -DFLAG -I %t/lazy-skip-non-inlinable

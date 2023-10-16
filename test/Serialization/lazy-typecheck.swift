// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -swift-version 5 %S/../Inputs/lazy_typecheck.swift -enable-library-evolution -parse-as-library -package-name Package -DFLAG -typecheck -verify
// RUN: %target-swift-frontend -swift-version 5 %S/../Inputs/lazy_typecheck.swift -module-name lazy_typecheck -emit-module -emit-module-path %t/lazy_typecheck.swiftmodule -enable-library-evolution -parse-as-library -package-name Package -DFLAG -experimental-lazy-typecheck -experimental-skip-all-function-bodies -experimental-skip-non-exportable-decls

// Verify the module also builds without -experimental-skip-non-exportable-decls
// FIXME: Remove with rdar://117020997
// RUN: %target-swift-frontend -swift-version 5 %S/../Inputs/lazy_typecheck.swift -module-name lazy_typecheck -emit-module -emit-module-path /dev/null -enable-library-evolution -parse-as-library -package-name Package -DFLAG -experimental-lazy-typecheck -experimental-skip-all-function-bodies

// RUN: %target-swift-frontend -package-name Package -typecheck -verify %S/../Inputs/lazy_typecheck_client.swift -DFLAG -I %t

// FIXME: Re-run the test with -experimental-skip-non-inlinable-function-bodies


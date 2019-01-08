// Note that this test should still "pass" when no swiftinterfaces have been
// generated.

// RUN: %empty-directory(%t)
// RUN: for x in %platform-sdk-overlay-dir/*.swiftinterface; do [[ $(basename "$x") = Swift.swiftinterface || $(basename "$x") = simd.swiftinterface || $(basename "$x") = SwiftLang.swiftinterface || $(basename "$x") = '*.swiftinterface' ]] && continue; %target-swift-frontend "$x" -build-module-from-parseable-interface -o %t/$(basename "$x" .swiftinterface).swiftmodule -Fsystem %sdk/System/Library/PrivateFrameworks/ || echo '%target-os:' $(basename "$x") >> %t/failures.txt; done
// RUN: test ! -e %t/failures.txt || diff <(grep '%target-os:' %s) <(sort -f %t/failures.txt)

// REQUIRES: nonexecutable_test

// The following parseable interfaces (in alphabetical order) are known not to
// work with these settings.

// Missing search path for XCTest.framework.
macosx: XCTest.swiftinterface
ios: XCTest.swiftinterface
tvos: XCTest.swiftinterface

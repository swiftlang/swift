// RUN: %empty-directory(%t)
// RUN: for x in %platform-sdk-overlay-dir/*.swiftinterface; do [[ $(basename "$x") = Swift.swiftinterface || $(basename "$x") = simd.swiftinterface || $(basename "$x") = SwiftLang.swiftinterface ]] && continue; %target-swift-frontend "$x" -emit-module -o %t/$(basename "${x/.*}").swiftmodule -disable-objc-attr-requires-foundation-module -enable-resilience -Fsystem %sdk/System/Library/PrivateFrameworks/ -swift-version 4 || echo '%target-os:' $(basename "$x") >> %t/failures.txt; done
// RUN: diff <(grep '%target-os:' %s) <(sort -f %t/failures.txt)

// REQUIRES: nonexecutable_test

// The following parseable interfaces (in alphabetical order) are known not to
// work with these settings.

// Needs to be built as Swift 4.2.
macosx: CloudKit.swiftinterface
ios: CloudKit.swiftinterface
tvos: CloudKit.swiftinterface
watchos: CloudKit.swiftinterface

// Needs to be built as Swift 4.2.
ios: UIKit.swiftinterface
tvos: UIKit.swiftinterface
watchos: UIKit.swiftinterface

// Missing search path for XCTest.framework.
macosx: XCTest.swiftinterface
ios: XCTest.swiftinterface
tvos: XCTest.swiftinterface

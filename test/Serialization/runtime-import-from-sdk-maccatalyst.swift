/// Tests the fallback behavior for runtime library import paths on macCatalyst. These
/// should prefer the resource directory, then the SDK's System/iOSSupport
/// directory, then the SDK's root. Contrariwise, test that iOSSupport is *not*
/// used on any other platform.

// Test format: We try to type-check this file with different combinations of
// mock resource directories and SDKs. If it loads a standard library,
// typechecking will fail with a message that includes the word "success" (from
// the name of the undefined function we try to call). If it tries to load a
// standard library it shouldn't reach, loading will fail with a message that
// includes the word "failure" (from the target triples we insert into the
// bad .swiftmodule directories). If it can't even find a standard library to
// attempt to load, the error message will contain neither "success" nor
// "failure"; this should happen only in the last test case.

// REQUIRES: maccatalyst_support

// This symbol does not exist in the standard library.
Swift.success()

// ***** SDKs AND RESOURCE DIRECTORIES *****

// bad-bad-sdk has bad stdlibs in both iOSSupport and the root.
//
// RUN: %empty-directory(%t/bad-bad-sdk)
// RUN: mkdir -p %t/bad-bad-sdk/System/iOSSupport/usr/lib/swift/Swift.swiftmodule
// RUN: touch %t/bad-bad-sdk/System/iOSSupport/usr/lib/swift/Swift.swiftmodule/failure-failure-failure.swiftmodule
// RUN: mkdir -p %t/bad-bad-sdk/usr/lib/swift/Swift.swiftmodule
// RUN: touch %t/bad-bad-sdk/usr/lib/swift/Swift.swiftmodule/failure-failure-failure.swiftmodule

// good-bad-sdk has a good stdlib in iOSSupport and a bad stdlib in the root.
//
// RUN: %empty-directory(%t/good-bad-sdk)
// RUN: mkdir -p %t/good-bad-sdk/System/iOSSupport/usr/lib/swift
// RUN: cp -r %test-resource-dir/maccatalyst/Swift.swiftmodule %t/good-bad-sdk/System/iOSSupport/usr/lib/swift/Swift.swiftmodule
// RUN: mkdir -p %t/good-bad-sdk/usr/lib/swift/Swift.swiftmodule
// RUN: touch %t/good-bad-sdk/usr/lib/swift/Swift.swiftmodule/failure-failure-failure.swiftmodule

// bad-good-sdk has a bad stdlib in iOSSupport and a good stdlib in the root.
// Note that for the stdlib in this case to be "good", it needs to be universal.
//
// RUN: %empty-directory(%t/bad-good-sdk)
// RUN: mkdir -p %t/bad-good-sdk/System/iOSSupport/usr/lib/swift/Swift.swiftmodule
// RUN: touch %t/bad-good-sdk/System/iOSSupport/usr/lib/swift/Swift.swiftmodule/failure-failure-failure.swiftmodule
// RUN: mkdir -p %t/bad-good-sdk/usr/lib/swift
// RUN: cp -r %test-resource-dir/maccatalyst/Swift.swiftmodule %t/bad-good-sdk/usr/lib/swift/Swift.swiftmodule
// RUN: cp -r %test-resource-dir/macosx/Swift.swiftmodule/* %t/bad-good-sdk/usr/lib/swift/Swift.swiftmodule

// empty-good-sdk has no stdlib in iOSSupport and a good stdlib in the root.
// Note that for the stdlib in this case to be "good", it needs to be universal.
//
// RUN: %empty-directory(%t/empty-good-sdk)
// RUN: mkdir -p %t/empty-good-sdk/System/iOSSupport/usr/lib/swift
// RUN: mkdir -p %t/empty-good-sdk/usr/lib/swift
// RUN: cp -r %test-resource-dir/maccatalyst/Swift.swiftmodule %t/empty-good-sdk/usr/lib/swift/Swift.swiftmodule
// RUN: cp -r %test-resource-dir/macosx/Swift.swiftmodule/* %t/empty-good-sdk/usr/lib/swift/Swift.swiftmodule

// empty-empty-sdk has no stdlib in iOSSupport or the root.
//
// RUN: %empty-directory(%t/empty-empty-sdk)
// RUN: mkdir -p %t/empty-empty-sdk/System/iOSSupport/usr/lib/swift
// RUN: mkdir -p %t/empty-empty-sdk/usr/lib/swift

// We don't create a good-resourcedir; we just use the default one.

// empty-resdir has no stdlib in it.
//
// RUN: %empty-directory(%t/empty-resdir/usr/lib/swift)
// FIXME: Until we have private imports, we need SwiftShims in the toolchain.
// RUN: cp -r %test-resource-dir/shims %t/empty-resdir/usr/lib/swift/shims



// ***** MACCATALYST TESTS *****

// RESDIR: If resource-dir has a standard library, it will be preferred
// over sdk/iOSSupport. (default resource dir + bad-bad-sdk)
//
// RUN: %empty-directory(%t.mcp)
// RUN: not %target-swift-frontend(mock-sdk: -sdk %t/bad-bad-sdk) -target %target-cpu-apple-ios13.1-macabi -module-cache-path %t.mcp -typecheck %s 2>&1 | %FileCheck %s --check-prefix=RESDIR-MACCATALYST
// RESDIR-MACCATALYST: success

// IOSSUP: If resource-dir has no standard library but sdk/iOSSupport does, it
// will be preferred over sdk. (empty-resdir + good-bad-sdk)
//
// RUN: %empty-directory(%t.mcp)
// RUN: not %target-swift-frontend(mock-sdk: -sdk %t/good-bad-sdk) -target %target-cpu-apple-ios13.1-macabi -resource-dir %t/empty-resdir/usr/lib/swift -module-cache-path %t.mcp -typecheck %s 2>&1 | %FileCheck %s --check-prefix=IOSSUP-MACCATALYST
// IOSSUP-MACCATALYST: success

// IOSBAD: Confirms that we don't use sdk/iOSSupport on non-macCatalyst, even if
// present. (empty-resdir + bad-good-sdk)
//
// RUN: %empty-directory(%t.mcp)
// RUN: not %target-swift-frontend(mock-sdk: -sdk %t/bad-good-sdk) -target %target-cpu-apple-ios13.1-macabi -resource-dir %t/empty-resdir/usr/lib/swift -module-cache-path %t.mcp -typecheck %s 2>&1 | %FileCheck %s --check-prefix=IOSBAD-MACCATALYST
// IOSBAD-MACCATALYST: failure

// SDKTOP: If resource-dir and sdk/iOSSupport don't have standard libraries but
// sdk does, it will be used as a last resort. (empty-resdir + empty-good-sdk)
//
// RUN: %empty-directory(%t.mcp)
// RUN: not %target-swift-frontend(mock-sdk: -sdk %t/empty-good-sdk) -target %target-cpu-apple-ios13.1-macabi -resource-dir %t/empty-resdir/usr/lib/swift -module-cache-path %t.mcp -typecheck %s 2>&1 | %FileCheck %s --check-prefix=SDKTOP-MACCATALYST
// SDKTOP-MACCATALYST: success

// NILLIB: If no standard libraries are available, stdlib loading fails.
// (empty-resdir + empty-empty-sdk) This one has a different error message from
// the others because there are no failure-failure-failure triples to find.
//
// RUN: %empty-directory(%t.mcp)
// RUN: not %target-swift-frontend(mock-sdk: -sdk %t/empty-empty-sdk) -target %target-cpu-apple-ios13.1-macabi -resource-dir %t/empty-resdir/usr/lib/swift -module-cache-path %t.mcp -typecheck %s 2>&1 | %FileCheck %s --check-prefix=NILLIB-MACCATALYST
// NILLIB-MACCATALYST: unable to load standard library



// ***** MACOSX TESTS *****

// FIXME: Right now we're only building the ios-macabi swiftmodule when we
// build for macCatalyst, so we need to accept "module 'Swfit' was created for
// incompatible target" as successful.

// RESDIR: If resource-dir has a standard library, it will be preferred
// over sdk/iOSSupport. (default resource dir + bad-bad-sdk)
//
// RUN: %empty-directory(%t.mcp)
// RUN: not %target-swift-frontend(mock-sdk: -sdk %t/bad-bad-sdk) -target %target-cpu-apple-macosx10.15 -module-cache-path %t.mcp -typecheck %s 2>&1 | %FileCheck %s --check-prefix=RESDIR-MACOSX
// RESDIR-MACOSX: {{success|module 'Swift' was created for incompatible target}}

// IOSSUP: If resource-dir has no standard library but sdk/iOSSupport does, it
// will be preferred over sdk. (empty-resdir + good-bad-sdk)
//
// RUN: %empty-directory(%t.mcp)
// RUN: not %target-swift-frontend(mock-sdk: -sdk %t/good-bad-sdk) -target %target-cpu-apple-macosx10.15 -resource-dir %t/empty-resdir/usr/lib/swift -module-cache-path %t.mcp -typecheck %s 2>&1 | %FileCheck %s --check-prefix=IOSSUP-MACOSX
// IOSSUP-MACOSX: failure

// IOSBAD: Confirms that we don't use sdk/iOSSupport on non-macCatalyst, even if
// present. (empty-resdir + bad-good-sdk)
//
// RUN: %empty-directory(%t.mcp)
// RUN: not %target-swift-frontend(mock-sdk: -sdk %t/bad-good-sdk) -target %target-cpu-apple-macosx10.15 -resource-dir %t/empty-resdir/usr/lib/swift -module-cache-path %t.mcp -typecheck %s 2>&1 | %FileCheck %s --check-prefix=IOSBAD-MACOSX
// IOSBAD-MACOSX: {{success|module 'Swift' was created for incompatible target}}

// SDKTOP: If resource-dir and sdk/iOSSupport don't have standard libraries but
// sdk does, it will be used as a last resort. (empty-resdir + empty-good-sdk)
//
// RUN: %empty-directory(%t.mcp)
// RUN: not %target-swift-frontend(mock-sdk: -sdk %t/empty-good-sdk) -target %target-cpu-apple-macosx10.15 -resource-dir %t/empty-resdir/usr/lib/swift -module-cache-path %t.mcp -typecheck %s 2>&1 | %FileCheck %s --check-prefix=SDKTOP-MACOSX
// SDKTOP-MACOSX: {{success|module 'Swift' was created for incompatible target}}

// NILLIB: If no standard libraries are available, stdlib loading fails.
// (empty-resdir + empty-empty-sdk) This one has a different error message from
// the others because there are no failure-failure-failure triples to find.
//
// RUN: %empty-directory(%t.mcp)
// RUN: not %target-swift-frontend(mock-sdk: -sdk %t/empty-empty-sdk) -target %target-cpu-apple-macosx10.15 -resource-dir %t/empty-resdir/usr/lib/swift -module-cache-path %t.mcp -typecheck %s 2>&1 | %FileCheck %s --check-prefix=NILLIB-MACOSX
// NILLIB-MACOSX: unable to load standard library

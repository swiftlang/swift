// REQUIRES: OS=maccatalyst || OS=macosx
// REQUIRES: maccatalyst_support
// REQUIRES: CPU=x86_64

// REQUIRES: rdar136779081

// Zippered libraries

// RUN: %otool-classic -l %test-resource-dir/macosx/libswiftCore.dylib | %FileCheck %s --check-prefix=CHECK-ZIPPERED
// RUN: %otool-classic -l %test-resource-dir/macosx/libswiftDarwin.dylib | %FileCheck %s --check-prefix=CHECK-ZIPPERED
// RUN: %otool-classic -l %test-resource-dir/macosx/libswiftSwiftOnoneSupport.dylib | %FileCheck %s --check-prefix=CHECK-ZIPPERED
// RUN: %otool-classic -l %test-resource-dir/macosx/libswiftCompatibility51.a | %FileCheck %s --check-prefix=CHECK-ZIPPERED


// macCatalyst-only libraries
//   (None)

// Unzippered twins (separate iosMac-only and macOS-only libraries)
//   (None)

// For zippered dylibs we expect two load commands:
// one for macos and one for maccatalyst.
//
// Note: For dylibs with deployment targets earlier than 10.14 we
// should see an LC_VERSION_MIN_MACOSX load command followed by a
// LC_BUILD_VERSION command. This enables the dylib to be back
// deployed to versions of the OS don't support macCatalyst.
//
// For dylibs targetting 10.14 and later, we should see two
// LC_BUILD_VERSION commands.
//
// CHECK-ZIPPERED:       cmd {{LC_BUILD_VERSION|LC_VERSION_MIN_MACOSX}}
// CHECK-ZIPPERED-NEXT:  cmdsize
// CHECK-ZIPPERED-NEXT:  {{platform 1|version}}
// CHECK-ZIPPERED-NEXT:  {{minos|sdk}}

// CHECK-ZIPPERED:       cmd LC_BUILD_VERSION
// CHECK-ZIPPERED-NEXT:  cmdsize
// CHECK-ZIPPERED-NEXT:  platform 6
// CHECK-ZIPPERED-NEXT:  minos

// For macCatalyst-only dylibs we expect a maccatalyst load command and no macos load command
// CHECK-MACCATALYST-NOT:   platform macos
// CHECK-MACCATALYST:       cmd LC_BUILD_VERSION
// CHECK-MACCATALYST-NEXT:  cmdsize
// CHECK-MACCATALYST-NEXT:  platform 6
// CHECK-MACCATALYST-NEXT:  minos
// CHECK-MACCATALYST-NOT:   platform macos


// For Mac-only dylibs we expect a macos load command and no maccatalyst load command
// Similar to the zippered case, when the deployment target is 10.14 and later
// we should expect a *single* LC_BUILD_VERSION command and when it is earlier
// we should expect only a LC_VERSION_MIN_MACOSX command.
// CHECK-MAC-NOT:   platform 6
// CHECK-MAC:       cmd LC_VERSION_MIN_MACOSX
// CHECK-MAC-NEXT:  cmdsize
// CHECK-MAC-NEXT:  version
// CHECK-MAC-NOT:   platform 6

// Check to make sure that when passing -target-variant the .o file is zippered.
func foo() { }

// RUN: %swiftc_driver -target x86_64-apple-macosx10.15 -target-variant x86_64-apple-ios13.1-macabi -emit-object %s -o %t.o
// RUN: %otool-classic -l %t.o | %FileCheck %s --check-prefix=CHECK-ZIPPERED

// With -target and -target variant reversed
// RUN: %swiftc_driver -target x86_64-apple-ios13.1-macabi -target-variant x86_64-apple-macosx10.15 -emit-object %s -o %t.reversed.o
// RUN: %otool-classic -l %t.reversed.o | %FileCheck %s --check-prefix=CHECK-ZIPPERED

// RUN: %swiftc_driver -target x86_64-apple-macosx10.15 -target-variant x86_64-apple-ios13.1-macabi -emit-library -module-name foo %s -o %t.dylib
// RUN: %otool-classic -l %t.dylib | %FileCheck %s --check-prefix=CHECK-ZIPPERED

// RUN: %swiftc_driver -target x86_64-apple-ios13.1-macabi -target-variant x86_64-apple-macosx10.15 -emit-library -module-name foo %s -o %t.reversed.dylib
// RUN: %otool-classic -l %t.reversed.dylib | %FileCheck %s --check-prefix=CHECK-ZIPPERED

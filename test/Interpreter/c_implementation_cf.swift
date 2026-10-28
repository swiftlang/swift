// RUN: %empty-directory(%t)
// RUN: split-file %s %t --leading-lines

/// Build Swift dylib and compatibility header.
// RUN: %target-build-swift-dylib(%t/%target-library-name(Lib)) %t/Lib.swift \
// RUN:   -emit-module-path %t/Lib.swiftmodule \
// RUN:   -emit-clang-header-path %t/cdecl.h \
// RUN:   -import-objc-header %t/BridgingHeader.h %if !legacy_swift_driver %{ -explicit-module-build %}
// RUN: %target-codesign %t/%target-library-name(Lib)

/// Build a C client against cdecl.h.
// RUN: %clang-no-modules %t/Client.c -o %t/a.out -target %target-triple \
// RUN:   -I %clang-include-dir -Werror -isysroot %sdk \
// RUN:   -I %t -l Lib -L %t %target-rpath(%t) -framework CoreFoundation
// RUN: %target-codesign %t/a.out
// RUN: %target-run %t/a.out %t/%target-library-name(Lib) > %t/run.log
// RUN: %FileCheck %t/Client.c --check-prefix=PRINTS --input-file %t/run.log

// REQUIRES: executable_test
// REQUIRES: objc_interop

//--- BridgingHeader.h
#include <CoreFoundation/CoreFoundation.h>

CF_ASSUME_NONNULL_BEGIN

/// 'CFTypeRef' is imported as 'AnyObject' and uses the same reference
/// counting, so a CF object can travel through it.
extern CFTypeRef makeType(void) CF_RETURNS_RETAINED;
extern CFTypeID typeID(CFTypeRef obj);

/// A 'CFTypeRef' holding a CF class type keeps its identity across the
/// round trip through Swift.
extern CFStringRef copyAsString(CFTypeRef obj) CF_RETURNS_RETAINED;

CF_ASSUME_NONNULL_END

//--- Lib.swift
import Foundation

@c @implementation
public func makeType() -> CFTypeRef {
  return CFStringCreateWithCString(nil, "made in Swift",
                                   CFStringBuiltInEncodings.UTF8.rawValue)!
}

@c @implementation
public func typeID(_ obj: CFTypeRef) -> CFTypeID {
  return CFGetTypeID(obj)
}

@c @implementation
public func copyAsString(_ obj: CFTypeRef) -> CFString {
  return obj as! CFString
}

//--- Client.c

#include "cdecl.h"
#include "BridgingHeader.h"
#include <stdio.h>

int main() {
    CFTypeRef obj = makeType();

    printf("is a string: %d\n", typeID(obj) == CFStringGetTypeID());
    // PRINTS: is a string: 1

    CFStringRef string = copyAsString(obj);
    char buffer[64];
    CFStringGetCString(string, buffer, sizeof(buffer), kCFStringEncodingUTF8);
    printf("contents: %s\n", buffer);
    // PRINTS: contents: made in Swift

    CFRelease(string);
    CFRelease(obj);
    printf("done\n");
    // PRINTS: done
}

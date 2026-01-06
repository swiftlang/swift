// RUN: rm -rf %t
// RUN: split-file %s %t
// RUN: mkdir -p %t/pch

// RUN: %target-swift-frontend %t/SwiftMod.swift -module-name SwiftMod -emit-module -o %t/SwiftMod.swiftmodule -I %t -enable-experimental-cxx-interop -Xcc -DFIRSTPASS

// RUN: %target-swift-ide-test -print-module -module-to-print=SwiftMod -module-to-print=CxxModule -I %t -I %t/Inputs -I %swift_src_root/lib/ClangImporter/SwiftBridging -source-filename=x -enable-experimental-cxx-interop | %FileCheck %s

// RUN: %target-swift-ide-test -print-module -module-to-print=SwiftMod -module-to-print=CxxModule -I %t -I %t/Inputs -I %swift_src_root/lib/ClangImporter/SwiftBridging -source-filename=x -enable-experimental-cxx-interop -Xcc -DINCMOD | %FileCheck %s

// RUN: %target-swift-ide-test -print-module -module-to-print=SwiftMod -module-to-print=CxxModule -I %t -I %t/Inputs -I %swift_src_root/lib/ClangImporter/SwiftBridging -source-filename=x -cxx-interoperability-mode=swift-6 -Xcc -DINCMOD | %FileCheck --check-prefixes=CHECK,CHECKLATEST %s
// RUN: %target-swift-ide-test -print-module -module-to-print=SwiftMod -module-to-print=CxxModule -I %t -I %t/Inputs -I %swift_src_root/lib/ClangImporter/SwiftBridging -source-filename=x -cxx-interoperability-mode=upcoming-swift -Xcc -DINCMOD | %FileCheck --check-prefixes=CHECK,CHECKLATEST %s

// Test through the use of the bridging header
// RUN: %target-swift-frontend -emit-ir -I %t -import-objc-header %t/Inputs/header.h -I %swift_src_root/lib/ClangImporter/SwiftBridging -enable-experimental-cxx-interop -DBRIDGING_HEADER_TEST -disable-availability-checking %t/SwiftMod.swift

// Precompile the bridging header and test the use of that.
// RUN: %target-swift-frontend -emit-pch -I %t -pch-output-dir %t/pch %t/Inputs/header.h -I %swift_src_root/lib/ClangImporter/SwiftBridging -enable-experimental-cxx-interop
// RUN: %target-swift-frontend -emit-ir -I %t -pch-output-dir %t/pch -import-objc-header %t/Inputs/header.h -I %swift_src_root/lib/ClangImporter/SwiftBridging -enable-experimental-cxx-interop -DBRIDGING_HEADER_TEST -disable-availability-checking %t/SwiftMod.swift


//--- SwiftMod.swift

public protocol Proto {
}

#if BRIDGING_HEADER_TEST
func f() -> SharedObject { return SharedObject.create() }

func g() {
  var logger: LoggerSingleton?
  var loggerPtr: UnsafeMutablePointer<LoggerSingleton?>?
  var loggerPtrPtr: UnsafeMutablePointer<UnsafeMutablePointer<LoggerSingleton?>?>?

  takeLoggersByPointer(logger, &logger, &loggerPtr)
  takeLoggersByPointer(logger, loggerPtr, loggerPtrPtr)
  takeLoggersByPointer(nil, nil, nil)
  
  takeLoggersByReference(logger!, &logger, &loggerPtr)
  takeLoggersByReference(logger!, &loggerPtr!.pointee, &loggerPtrPtr!.pointee)
}

func releaseSharedObject(_: SharedObject) { }
#endif

//--- Inputs/module.modulemap
module CxxModule {
    header "header.h"
    requires cplusplus
}

//--- Inputs/header.h

// Note: in actuality, this will be included
// as <swift/bridging>, but in this test we include
// it directly.
#ifndef INCMOD
#include "swift/bridging"
#else
#pragma clang module import SwiftBridging
#endif

class SWIFT_SELF_CONTAINED SelfContained {
public:
    int *pointer;

    SelfContained();

    const int *returnsIndependent() const SWIFT_RETURNS_INDEPENDENT_VALUE;
};

class SWIFT_SHARED_REFERENCE(retainSharedObject, releaseSharedObject)
SharedObject {
public:
    static SharedObject *create();
};

void retainSharedObject(SharedObject *);
void releaseSharedObject(SharedObject *);

class SWIFT_IMMORTAL_REFERENCE LoggerSingleton {
public:
    LoggerSingleton(const LoggerSingleton &) = delete;
    static LoggerSingleton *getInstance();
};

void takeLoggersByPointer(LoggerSingleton *ptr, LoggerSingleton **ptr_ptr, LoggerSingleton ***ptr_ptr_ptr);
void takeLoggersByReference(LoggerSingleton &ref, LoggerSingleton *&ref_ptr, LoggerSingleton **&ref_ptr_ptr);

void takeLoggersByConstPointer(const LoggerSingleton **pointee0, LoggerSingleton const **pointee1, LoggerSingleton *const *pointer);
void takeLoggersByConstReference(const LoggerSingleton *&pointee0, LoggerSingleton const *&pointee1, LoggerSingleton *const &pointer);

class SWIFT_UNSAFE_REFERENCE UnsafeNonCopyable {
public:
    UnsafeNonCopyable(UnsafeNonCopyable &) = delete;
};

UnsafeNonCopyable *returnsPointerToUnsafeReference();
void takesPointerToUnsafeNonCopyable(UnsafeNonCopyable *);

class SWIFT_CONFORMS_TO_PROTOCOL(SwiftMod.Proto) ConformsTo {
public:
};

class SWIFT_UNCHECKED_SENDABLE UnsafeSendable {
public:
};

class SWIFT_NONCOPYABLE NonCopyableCopyable {
public:
    NonCopyableCopyable(const NonCopyableCopyable &other) = default;
    NonCopyableCopyable(NonCopyableCopyable &&other);
    ~NonCopyableCopyable();
private:
    int x;
};


// CHECK: struct SelfContained {

// CHECK:   func returnsIndependent() -> UnsafePointer<Int32>!

// CHECK: class SharedObject {
// CHECK:   class func create() -> SharedObject!
// CHECK: func retainSharedObject(_: SharedObject!)
// CHECK: func releaseSharedObject(_: SharedObject!)

// CHECK: class LoggerSingleton {
// CHECK:   class func getInstance() -> LoggerSingleton!
// CHECK: }

// CHECK-LABEL: func takeLoggersByPointer(
// CHECK-SAME: _ ptr: LoggerSingleton!,
// CHECK-SAME: _ ptr_ptr: UnsafeMutablePointer<LoggerSingleton?>!,
// CHECK-SAME: _ ptr_ptr_ptr: UnsafeMutablePointer<UnsafeMutablePointer<LoggerSingleton?>?>!)

// CHECK-LABEL: func takeLoggersByReference(
// CHECK-SAME: _ ref: LoggerSingleton,
// CHECK-SAME: _ ref_ptr: inout LoggerSingleton!,
// CHECK-SAME: _ ref_ptr_ptr: inout UnsafeMutablePointer<LoggerSingleton?>!)

// CHECK-LABEL: func takeLoggersByConstPointer(
// CHECK-SAME: _ pointee0: UnsafeMutablePointer<LoggerSingleton?>!,
// CHECK-SAME: _ pointee1: UnsafeMutablePointer<LoggerSingleton?>!,
// CHECK-SAME: _ pointer: UnsafePointer<LoggerSingleton?>!)

// CHECK-LABEL: func takeLoggersByConstReference(
// CHECK-SAME: _ pointee0: inout LoggerSingleton!,
// CHECK-SAME: _ pointee1: inout LoggerSingleton!,
// CHECK-SAME: _ pointer: LoggerSingleton!)

// CHECK: class UnsafeNonCopyable {
// CHECK: }
// CHECK: func returnsPointerToUnsafeReference() -> UnsafeNonCopyable!
// CHECK: func takesPointerToUnsafeNonCopyable(_: UnsafeNonCopyable!)

// CHECK: struct ConformsTo : Proto {

// CHECK: struct UnsafeSendable : @unchecked Sendable {

// CHECKLATEST: struct NonCopyableCopyable

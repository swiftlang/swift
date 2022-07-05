// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend %s -target %target-cpu-apple-macosx12.0 -module-name main -emit-ir -o %t/new.ir
// RUN: %FileCheck %s --check-prefix=NEW < %t/new.ir
// RUN: %target-swift-frontend %s -target %target-cpu-apple-macosx10.15 -module-name main -emit-ir -o %t/old.ir -disable-availability-checking
// RUN: %FileCheck %s --check-prefix=OLD < %t/old.ir
// RUN: %target-swift-frontend %s -target %target-cpu-apple-macosx10.15 -O -module-name main -emit-ir -o %t/optimized.ir -disable-availability-checking
// RUN: %FileCheck %s --check-prefix=OPTIMIZED < %t/optimized.ir


// REQUIRES: OS=macosx

// NEW-NOT: extern_weak
// OLD: @"$sScPMn" = extern_weak global
// OLD: declare extern_weak swiftcc i8* @swift_task_alloc
// OLD: declare extern_weak swiftcc %swift.metadata_response @"$sScPMa"
// OLD: declare extern_weak swiftcc i8 @"$sScP8rawValues5UInt8Vvg"

// OPTIMIZED: @swift_async_extendedFramePointerFlags = extern_weak global i8*
// OPTIMIZED: @_swift_async_extendedFramePointerFlagsUser = linkonce_odr hidden global i8** @swift_async_extendedFramePointerFlags
// OPTIMIZED: @llvm.used =
// OPTIMIZED-SAME: (i8*** @_swift_async_extendedFramePointerFlagsUser to i8*)

@available(macOS 12.0, *)
public func g() async -> String { "hello" }

@available(macOS 12.0, *)
public func f() async {
  Task {
    print(await g())
  }
}

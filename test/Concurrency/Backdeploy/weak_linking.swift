// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend %s -target x86_64-apple-macosx12.0 -module-name main -emit-ir -o %t/new.ir
// RUN: %FileCheck %s --check-prefix=NEW < %t/new.ir
// RUN: %target-swift-frontend %s -target x86_64-apple-macosx10.15 -module-name main -emit-ir -o %t/old.ir -disable-availability-checking
// RUN: %FileCheck %s --check-prefix=OLD < %t/old.ir

// REQUIRES: OS=macosx

// NEW-NOT: extern_weak
// OLD: @"$sScPMn" = extern_weak global
// OLD: declare extern_weak swiftcc i8* @swift_task_alloc
// OLD: declare extern_weak swiftcc %swift.metadata_response @"$sScPMa"
// OLD: declare extern_weak swiftcc i8 @"$sScP8rawValues5UInt8Vvg"

@available(macOS 12.0, *)
public func g() async -> String { "hello" }

@available(macOS 12.0, *)
public func f() async {
  Task {
    print(await g())
  }
}

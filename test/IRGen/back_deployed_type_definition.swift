// RUN: %target-swift-frontend -emit-ir -parse-as-library -module-name M -enable-library-evolution -validate-tbd-against-ir=all %s > %t.ll
// RUN: %FileCheck %s < %t.ll
// RUN: %FileCheck %s --check-prefix=NEGATIVE < %t.ll
// RUN: %FileCheck %s --check-prefix=ACCESSORS < %t.ll
// RUN: %target-swift-frontend -emit-silgen -parse-as-library -module-name M -enable-library-evolution %s | %FileCheck %s --check-prefix=SILGEN

// REQUIRES: OS=macosx
// REQUIRES: CPU=arm64 || CPU=x86_64

// The module defining a back-deployed type emits ordinary, unique metadata
// for it. Its metadata is canonical whenever it is present at runtime; only
// clients that emit their own copies of the type have to defer to it.

@backDeployed(before: macOS 99)
@frozen public struct Box<T> {
  public var value: T
}

extension Box: Equatable where T: Equatable {}

@backDeployed(before: macOS 99)
@frozen public struct Point: Hashable {
  public var x: Int

  // Back-deployed along with the type.
  public func older() -> Int { x }

  // Only available where this module provides the type.
  @available(macOS 99, *)
  public func newer() -> Int { x }

  // Always emitted into clients, under either spelling, so not back-deployed.
  @export(implementation)
  public func exported() -> Int { x }

  @_alwaysEmitIntoClient
  public func historical() -> Int { x }
}

// Members are back-deployed with the type unless they are only available
// where this module provides it.
// SILGEN-DAG: sil non_abi [serialized] {{.*}}@$s1M5PointV5olderSiyFTwb :
// SILGEN-DAG: sil non_abi [serialized] {{.*}}@$s1M5PointV5olderSiyFTwB :
// SILGEN-DAG: sil [available 99] {{.*}}@$s1M5PointV5newerSiyF :
// SILGEN-NOT: $s1M5PointV5newerSiyFTw
// SILGEN-NOT: $s1M5PointV8exportedSiyFTw
// SILGEN-NOT: $s1M5PointV10historicalSiyFTw

// The type context descriptors are unique: flags are the kind (struct) plus
// the unique bit (0x40), and the generic bit (0x80) for Box.
// CHECK: @"$s1M3BoxVMn" = constant {{.*}} <{ i32 209,
// CHECK: @"$s1M5PointVMn" = constant {{.*}} <{ i32 81,

// Clients with their own copies of the type find this module's metadata
// through its ordinary access functions.
// ACCESSORS-DAG: define swiftcc %swift.metadata_response @"$s1M3BoxVMa"(i64 %0, ptr %1)
// ACCESSORS-DAG: define swiftcc %swift.metadata_response @"$s1M5PointVMa"(i64 %0)

// There is no runtime uniquing of the metadata or witness tables, and
// nothing is emitted for the sake of back deployment.
// NEGATIVE-NOT: Twa
// NEGATIVE-NOT: __swift_back_deployment_
// NEGATIVE-NOT: swift_conformsToProtocol

// Metadata and witness tables are referenced directly.
// CHECK-LABEL: define {{.*}}swiftcc void @"$s1M10usePointEqyyF"()
// CHECK: [[WTABLE:%.*]] = call ptr @"$s1M5PointVACSQAAWl"()
// CHECK: call swiftcc void @"$s1M7needsEqyyxmSQRzlF"(ptr getelementptr inbounds ({{.*}}, ptr @"$s1M5PointVMf", i32 0, i32 2), ptr getelementptr inbounds ({{.*}}, ptr @"$s1M5PointVMf", i32 0, i32 2), ptr [[WTABLE]])
public func needsEq<T: Equatable>(_: T.Type) {}
public func usePointEq() { needsEq(Point.self) }

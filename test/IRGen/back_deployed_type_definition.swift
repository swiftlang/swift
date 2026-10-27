// RUN: %target-swift-frontend -emit-ir -parse-as-library -module-name M -enable-library-evolution -validate-tbd-against-ir=all %s > %t.ll
// RUN: %FileCheck %s < %t.ll
// RUN: %FileCheck %s --check-prefix=NEGATIVE < %t.ll
// RUN: %FileCheck %s --check-prefix=ACCESSORS < %t.ll
// RUN: %target-swift-frontend -emit-silgen -parse-as-library -module-name M -enable-library-evolution %s | %FileCheck %s --check-prefix=SILGEN
// RUN: %target-swift-frontend -emit-silgen -target %target-cpu-apple-macosx14.0 -parse-as-library -module-name M -enable-library-evolution %s | %FileCheck %s --check-prefix=WITNESS

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

  // The same goes for accessors that are always emitted into clients, even
  // though their storage is back-deployed.
  public var doubled: Int {
    @export(implementation)
    get { x * 2 }
  }

  public var tripled: Int {
    @_transparent
    get { x * 3 }
  }

  // Back-deployed, because clients can reference it.
  @usableFromInline
  internal func usable() -> Int { x }

  // Internal to this module, so not part of the interface that is
  // back-deployed: there are no thunks, and callers use it directly.
  internal func hidden() -> Int { x }
  internal var hiddenProperty: Int { x }

  @frozen public struct Nested {
    public var y: Int

    internal func hiddenNested() -> Int { y }
  }
}

func callHidden(_ p: Point, _ n: Point.Nested) -> Int {
  p.hidden() + p.hiddenProperty + n.hiddenNested()
}

// Members are back-deployed with the type unless they are only available
// where this module provides it.
// SILGEN-DAG: sil non_abi [serialized] {{.*}}@$s1M5PointV5olderSiyFTwb :
// SILGEN-DAG: sil non_abi [serialized] {{.*}}@$s1M5PointV5olderSiyFTwB :
// SILGEN-DAG: sil [available 99] {{.*}}@$s1M5PointV5newerSiyF :
// SILGEN-NOT: $s1M5PointV5newerSiyFTw
// SILGEN-NOT: $s1M5PointV8exportedSiyFTw
// SILGEN-NOT: $s1M5PointV10historicalSiyFTw
// SILGEN-NOT: $s1M5PointV7doubledSivgTw
// SILGEN-NOT: $s1M5PointV7tripledSivgTw
// SILGEN-DAG: sil non_abi [serialized] {{.*}}@$s1M5PointV6usableSiyFTwb :
// SILGEN-DAG: sil non_abi [serialized] {{.*}}@$s1M5PointV6usableSiyFTwB :
// SILGEN-NOT: $s1M5PointV6hiddenSiyFTw
// SILGEN-NOT: $s1M5PointV14hiddenPropertySivgTw
// SILGEN-NOT: $s1M5PointV6NestedV06hiddenB0SiyFTw
// SILGEN-LABEL: sil hidden {{.*}}@$s1M10callHiddenySiAA5PointV_AD6NestedVtF :
// SILGEN: function_ref @$s1M5PointV6hiddenSiyF :
// SILGEN: function_ref @$s1M5PointV14hiddenPropertySivg :
// SILGEN: function_ref @$s1M5PointV6NestedV06hiddenB0SiyF :

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

// The serialized witness thunks of a back-deployed type call the back
// deployment thunks of the witnesses even when the deployment target is at
// least the version the type is back-deployed before. Clients that emit their
// own copy of the conformance run where the original witnesses might not
// exist, and the optimizer devirtualizes calls in the serialized fallbacks to
// these thunks.
@backDeployed(before: macOS 12)
@frozen public struct Old: Hashable {
  public var y: Int
}

// WITNESS-LABEL: sil shared [transparent] [serialized] [thunk] [ossa] @$s1M3OldVSHAASH9hashValueSivgTW :
// WITNESS: function_ref @$s1M3OldV9hashValueSivgTwb :
// WITNESS-LABEL: sil shared [transparent] [serialized] [thunk] [ossa] @$s1M3OldVSHAASH4hash4intoys6HasherVz_tFTW :
// WITNESS: function_ref @$s1M3OldV4hash4intoys6HasherVz_tFTwb :

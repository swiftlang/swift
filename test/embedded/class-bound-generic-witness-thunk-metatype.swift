// RUN: %target-swift-emit-ir %s -enable-experimental-feature Embedded -parse-as-library -module-name main -wmo | %FileCheck %s
// RUN: %target-swift-emit-ir %s -enable-experimental-feature Embedded -parse-as-library -module-name main -wmo -O | %FileCheck %s

// REQUIRES: swift_feature_Embedded

// The witness thunk for a class-bound generic class is emitted unspecialized.
// A metatype of the class in the code reachable from it must not produce a
// "specialized" vtable for the unbound class type, which would be rejected
// because its deinit is still generic.

public protocol Chan { func close() }

public final class Engine<L: AnyObject>: Chan {
  public static var tag: UInt8 { 0x42 }
  public func close() { _ = Self.tag }
}

public final class Other<L: AnyObject>: Chan {
  public static var tag: UInt8 { 0x42 }
  public func close() { _ = Other.tag }
}

public class K {}

public func useConcrete() {
  Engine<K>().close()
}

// CHECK: define {{.*}}@"$e4main11useConcreteyyF"()

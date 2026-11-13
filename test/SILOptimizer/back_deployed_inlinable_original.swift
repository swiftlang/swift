// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -emit-module -parse-as-library -enable-library-evolution -module-name MyKit -emit-module-path %t/MyKit.swiftmodule -target %target-cpu-apple-macosx10.15 -DLIBRARY %s
// RUN: %target-swift-frontend -O -emit-sil -parse-as-library -module-name Client -I %t -target %target-cpu-apple-macosx99 -DCLIENT %s | %FileCheck %s --check-prefix=NEW
// RUN: %target-swift-frontend -O -emit-sil -parse-as-library -module-name Client -I %t -target %target-cpu-apple-macosx10.15 -DCLIENT %s | %FileCheck %s --check-prefix=OLD

// REQUIRES: OS=macosx

// An @inlinable function that is back-deployed has a serialized original, so
// a client whose deployment target guarantees the original's availability
// (and so calls it directly) can inline and specialize it. A client that goes
// through the back deployment thunk can too.

#if LIBRARY

@backDeployed(before: macOS 50.3)
@inlinable
public func widen<T: BinaryInteger>(_ x: T) -> Int {
  Int(truncatingIfNeeded: x)
}

public struct Box {
  public init() {}

  // The accessor inherits @inlinable from the property.
  @backDeployed(before: macOS 50.3)
  @inlinable
  public var value: Int { 42 }
}

#else

import MyKit

// NEW-LABEL: sil {{.*}}@$s6Client4testyS2iF
// NEW-NOT: function_ref @$s5MyKit5widenySixSzRzlF
// NEW: return

// OLD-LABEL: sil {{.*}}@$s6Client4testyS2iF
// OLD-NOT: function_ref @$s5MyKit5widen
// OLD: return
public func test(_ x: Int) -> Int {
  widen(x)
}

// NEW-LABEL: sil {{.*}}@$s6Client12testPropertySiyF
// NEW-NOT: function_ref @$s5MyKit3BoxV5valueSivg
// NEW: integer_literal $Builtin.Int64, 42
// NEW: return

// OLD-LABEL: sil {{.*}}@$s6Client12testPropertySiyF
// OLD-NOT: function_ref @$s5MyKit3BoxV5valueSivg
// OLD: integer_literal $Builtin.Int64, 42
// OLD: return
public func testProperty() -> Int {
  Box().value
}

#endif

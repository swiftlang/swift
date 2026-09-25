// RUN: %target-swift-frontend -emit-ir -parse-stdlib -enable-objc-interop -disable-objc-attr-requires-foundation-module -validate-tbd-against-ir=none %s | %FileCheck --check-prefix=KEEP %s
// RUN: %target-swift-frontend -emit-ir -parse-stdlib -enable-objc-interop -disable-objc-attr-requires-foundation-module -validate-tbd-against-ir=none -disable-objc-ivar-metadata %s | %FileCheck --check-prefix=OMIT --implicit-check-not='@_IVARS__TtC{{.*}}10FixedSwift' --implicit-check-not='@_IVARS__TtC{{.*}}13PureSwiftBase' --implicit-check-not='@_IVARS__TtC{{.*}}13PureSwiftLeaf' %s

// REQUIRES: objc_codegen

precedencegroup AssignmentPrecedence {
  assignment: true
}

// Under -disable-objc-ivar-metadata, Swift-only hierarchies with a fixed
// metadata layout get a null ivar list.
// OMIT-DAG: @_DATA__TtC{{.*}}10FixedSwift = internal constant {{.*}}ptr null, ptr @.str.{{.*}}._TtC{{.*}}10FixedSwift, ptr null, ptr null, ptr null, ptr null, ptr null
// OMIT-DAG: @_DATA__TtC{{.*}}13PureSwiftBase = internal constant {{.*}}ptr null, ptr @.str.{{.*}}._TtC{{.*}}13PureSwiftBase, ptr null, ptr null, ptr null, ptr null, ptr null
// OMIT-DAG: @_DATA__TtC{{.*}}13PureSwiftLeaf = internal constant {{.*}}ptr null, ptr @.str.{{.*}}._TtC{{.*}}13PureSwiftLeaf, ptr null, ptr null, ptr null, ptr null, ptr null

// Classes that are themselves @objc, or that inherit from one, keep their
// ivar metadata even under the flag.
// OMIT-DAG: @_IVARS__TtC{{.*}}16ObjCExposedSwift = internal constant
// OMIT-DAG: @_IVARS__TtC{{.*}}21SwiftObjCAncestryLeaf = internal constant

// Without the flag, every class keeps its ivar metadata.
// KEEP-DAG: @_IVARS__TtC{{.*}}10FixedSwift = internal constant
// KEEP-DAG: @_IVARS__TtC{{.*}}13PureSwiftBase = internal constant
// KEEP-DAG: @_IVARS__TtC{{.*}}13PureSwiftLeaf = internal constant
// KEEP-DAG: @_IVARS__TtC{{.*}}16ObjCExposedSwift = internal constant
// KEEP-DAG: @_IVARS__TtC{{.*}}21SwiftObjCAncestryLeaf = internal constant
// KEEP-DAG: @_DATA__TtC{{.*}}10FixedSwift = internal constant {{.*}}@_IVARS__TtC{{.*}}10FixedSwift

class FixedSwift {
  var value: Builtin.Int64

  init(value: Builtin.Int64) {
    self.value = value
  }
}

class PureSwiftBase {
  var baseValue: Builtin.Int64 = Builtin.zeroInitializer()
}

class PureSwiftLeaf : PureSwiftBase {
  var leafValue: Builtin.Int64 = Builtin.zeroInitializer()
}

@objc public class ObjCExposedSwift {
  var objcValue: Builtin.Int64 = Builtin.zeroInitializer()
}

@objc public class SwiftObjCAncestryRoot {
  var rootValue: Builtin.Int64 = Builtin.zeroInitializer()
}

public class SwiftObjCAncestryLeaf : SwiftObjCAncestryRoot {
  var leafValue: Builtin.Int64 = Builtin.zeroInitializer()
}

public func useSwiftObjCAncestryLeaf(_ value: SwiftObjCAncestryLeaf) {}

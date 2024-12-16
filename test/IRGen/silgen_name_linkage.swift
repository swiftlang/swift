// RUN: %target-swift-frontend -parse-as-library -emit-ir %s | %FileCheck %s

// Since this test depends on weak linking based on availability, it only
// applies to Apple platforms.
// REQUIRES: OS=macosx || OS=ios || OS=tvos || OS=watchos || OS=visionos

@available(SwiftStdlib 9999, *)
@_silgen_name("privateForwardDecl")
private func privateForwardDecl()

@available(SwiftStdlib 9999, *)
@_silgen_name("internalForwardDecl")
internal func internalForwardDecl()

@available(SwiftStdlib 9999, *)
@_silgen_name("publicForwardDecl")
public func publicForwardDecl()

@available(SwiftStdlib 9999, *)
@_silgen_name("privateDefined")
private func privateDefined() {}

// CHECK: define internal swiftcc void @privateDefined()

@available(SwiftStdlib 9999, *)
@_silgen_name("internalDefined")
internal func internalDefined() {}

// CHECK: define hidden swiftcc void @internalDefined()

@available(SwiftStdlib 9999, *)
@_silgen_name("publicDefined")
public func publicDefined() {}

// CHECK: define swiftcc void @publicDefined()

public func test() {
  guard #available(SwiftStdlib 9999, *) else { return }
  privateForwardDecl()
  internalForwardDecl()
  publicForwardDecl()
  privateDefined()
  internalDefined()
  publicDefined()
}

// CHECK: declare extern_weak swiftcc void @privateForwardDecl()
// CHECK: declare extern_weak swiftcc void @internalForwardDecl()
// CHECK: declare extern_weak swiftcc void @publicForwardDecl()

// RUN: %empty-directory(%t)
// RUN: %target-build-swift %s -o %t/a.out4 -swift-version 4 && %target-codesign %t/a.out4 &&  %target-run %t/a.out4
// REQUIRES: objc_interop
// UNSUPPORTED: OS=watchos

// REQUIRES: executable_test

import StdlibUnittest

import Metal
import MetalKit

var MetalKitTests = TestSuite("MetalKit")

// Call each overlay to ensure nothing explodes

if #available(macOS 10.12, iOS 10.0, tvOS 10.0, *) {
  MetalKitTests.test("Globals") {

    do {
      let _ = try MTKModelIOVertexDescriptorFromMetalWithError(MTLVertexDescriptor())
    } catch _ {
      expectUnreachable("MTKModelIOVertexDescriptorFromMetalWithError has thrown an unexpected error")
    }

    do {
      let _ = try MTKMetalVertexDescriptorFromModelIOWithError(MDLVertexDescriptor())
    } catch _ {
      expectUnreachable("MTKMetalVertexDescriptorFromModelIOWithError has thrown an unexpected error")
    }
  }
}

if #available(macOS 10.11, iOS 9.0, tvOS 9.0, *) {
  MetalKitTests.test("MTKMesh") {
    func apiAvailabilityTest(device: MTLDevice) {
      do {
        let _ = try MTKMesh.newMeshes(asset: MDLAsset(), device: device)
      } catch _ {
        expectUnreachable("MTKMesh.newMeshes has thrown an unexpected error")
      }
    }
  }
}

runAllTests()

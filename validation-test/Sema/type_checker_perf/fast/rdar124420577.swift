// RUN: %target-typecheck-verify-swift -target %target-cpu-apple-macosx15 -swift-version 5 -solver-scope-threshold=28000
// REQUIRES: objc_interop
// REQUIRES: OS=macosx

import RealityKit
import Foundation

func f(_: () async throws -> Entity, load: (URL) -> Void) async throws {}

func test() async throws {
    try await f {
        var mesh = MeshDescriptor()
        let vertCount: Int = 10_000
        mesh.positions = .init((0..<vertCount).map { _ in .random(in: -1...1) })
        mesh.primitives = .triangles((0..<vertCount).map({ UInt32($0) }).flatMap({
            return [
                $0,
                $0 + 1,
                $0 + 2
            ]
        }))
        return ModelEntity(  // expected-warning {{actor-isolated}}
            mesh: try await .generate(from: [mesh]),
            materials: [SimpleMaterial()]
        )
    } load: { url in

    }
}


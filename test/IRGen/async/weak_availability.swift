// RUN: %target-swift-frontend -enable-implicit-dynamic -target %target-cpu-apple-macosx10.14 -Onone -emit-ir %s | %FileCheck --check-prefix=MAYBE-AVAILABLE %s
// REQUIRES: OS=macosx && CPU=x86_64

@available(macOS 12.0, *)
public func f<S: AsyncSequence>(_ s: S) async throws -> Any.Type {
  for try await _ in s { }

  typealias Fn = @MainActor () -> S.Element
  return Fn.self
}

// MAYBE-AVAILABLE: @"$sScI{{.*next.*}}YaKFTjTu" = extern_weak global
// MAYBE-AVAILABLE: declare{{.*}} extern_weak{{.*}} @swift_getFunctionTypeMetadataGlobalActor

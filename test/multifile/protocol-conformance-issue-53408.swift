// RUN: not %target-swift-frontend -emit-sil -module-name main -primary-file %s %S/Inputs/protocol-conformance-issue-53408-other.swift -disable-experimental-associated-type-inference
// RUN: %target-swift-frontend -emit-sil -module-name main %s -primary-file %S/Inputs/protocol-conformance-issue-53408-other.swift -disable-experimental-associated-type-inference
// RUN: not %target-swift-frontend -emit-sil -module-name main %s %S/Inputs/protocol-conformance-issue-53408-other.swift -disable-experimental-associated-type-inference
// RUN: %target-swift-frontend -emit-sil -module-name main %S/Inputs/protocol-conformance-issue-53408-other.swift %s -disable-experimental-associated-type-inference

// RUN: %target-swift-frontend -emit-sil -module-name main -primary-file %s %S/Inputs/protocol-conformance-issue-53408-other.swift -enable-experimental-associated-type-inference
// RUN: %target-swift-frontend -emit-sil -module-name main %s -primary-file %S/Inputs/protocol-conformance-issue-53408-other.swift -enable-experimental-associated-type-inference
// RUN: %target-swift-frontend -emit-sil -module-name main %s %S/Inputs/protocol-conformance-issue-53408-other.swift -enable-experimental-associated-type-inference
// RUN: %target-swift-frontend -emit-sil -module-name main %S/Inputs/protocol-conformance-issue-53408-other.swift %s -enable-experimental-associated-type-inference

// https://github.com/apple/swift/issues/53408

func reproducer() -> Float { return Struct().func1(1.0) }


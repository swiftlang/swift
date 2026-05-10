// RUN: %target-swift-frontend -emit-sil -module-name main -primary-file %s %S/Inputs/protocol-conformance-issue-53408-other.swift
// RUN: %target-swift-frontend -emit-sil -module-name main %s -primary-file %S/Inputs/protocol-conformance-issue-53408-other.swift
// RUN: %target-swift-frontend -emit-sil -module-name main %s %S/Inputs/protocol-conformance-issue-53408-other.swift
// RUN: %target-swift-frontend -emit-sil -module-name main %S/Inputs/protocol-conformance-issue-53408-other.swift %s

// https://github.com/apple/swift/issues/53408

func reproducer() -> Float { return Struct().func1(1.0) }


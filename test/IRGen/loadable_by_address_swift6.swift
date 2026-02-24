// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -emit-module -enable-library-evolution -emit-module-path=%t/resilient_struct.swiftmodule -module-name=resilient_struct %S/../Inputs/resilient_struct.swift
// RUN: %target-swift-frontend -I %t -c %s -swift-version 6 -sil-verify-all

public struct G<T> {
    public let a: String
    public let b: String
    public let c: String
    public let d: String
}

func f<T>(body: sending ((G<T>) -> Void)?) {}

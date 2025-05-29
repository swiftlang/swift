// RUN: %empty-directory(%t)
// RUN: split-file %s %t
// RUN: %target-swift-frontend -emit-module %t/Library.swift -emit-module-path %t/Library.swiftmodule -module-name Library
// RUN: %target-swift-frontend -typecheck -verify -strict-concurrency=complete -swift-version 6 %s -I %t

//--- Library.swift

@frozen public enum Numquam {}

@_fixed_layout public struct Nunca {} // expected-warning {{}}

//--- Client.swift

public protocol WithSendable {
  associatedtype AssocSendable : Sendable
}

extension Numquam : WithSendable {
  public typealias AssocSendable = Numquam
}

extension Nunca : WithSendable {
  public typealias AssocSendable = Nunca
}

public protocol WithBitwiseCopyable {
  associatedtype AssocBitwiseCopyable : BitwiseCopyable
}

extension Numquam : WithBitwiseCopyable {
  public typealias AssocBitwiseCopyable = Numquam
}

extension Nunca : WithBitwiseCopyable {
  public typealias AssocBitwiseCopyable = Nunca
}

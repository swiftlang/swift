// RUN: not %target-swift-frontend -typecheck %s

public protocol SomeProtocol {}

public struct Impl<Param>: SomeProtocol where Param: SomeProtocol {}

public struct Wrapper<Content> where Content: SomeProtocol {}

public extension Wrapper where Content == Impl<WrapperParam> {
  typealias WrapperParam = SomeProtocol
}


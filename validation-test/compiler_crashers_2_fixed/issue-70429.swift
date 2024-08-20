// RUN: %target-swift-frontend -emit-ir %s -disable-availability-checking

// https://github.com/swiftlang/swift/issues/70429

public protocol Node: Sendable {}
public protocol BlockNode: Node {}
public protocol InlineNode: Node {}

// MARK: - Block nodes

public struct Paragraph: BlockNode {
  public init() {}
}

// MARK: - NodeGroup

/// Intermediate aggregate for nodes.
///
/// Its intended usage is for building node groups through result builders.
public struct NodeGroup<each N: Node>: Sendable {
  @usableFromInline let nodes: (repeat each N)

  @usableFromInline init(nodes: repeat each N) {
    self.nodes = (repeat each nodes)
  }

  @usableFromInline func merging<each M>(_ group: NodeGroup<repeat each M>) -> NodeGroup<repeat each N, repeat each M> {
    NodeGroup<repeat each N, repeat each M>(nodes: repeat each self.nodes, repeat each group.nodes)
  }
}

@resultBuilder public enum GroupBuilder {
  public static func buildExpression<N: Node>(_ expression: N) -> NodeGroup<N> {
    NodeGroup(nodes: expression)
  }

  public static func buildPartialBlock<each N: Node>(first: consuming NodeGroup<repeat each N>) -> NodeGroup<repeat each N> {
    first
  }

  public static func buildPartialBlock<each A, each N>(accumulated: consuming NodeGroup<repeat each A>, next: consuming NodeGroup<repeat each N>) -> NodeGroup<repeat each A, repeat each N> {
    let tmp = accumulated.merging(next)
    return tmp
  }
}

// MARK: - Zero or More

/// Group defining a bunch of children nodes, which can occur zero or more times in a non-sequential order.
public struct ZeroOrMore<each N: Node>: Node {
  /// All nodes hold by this group.
  public let nodes: (repeat each N)

  public init(nodes: repeat each N) where repeat each N: InlineNode {
    self.nodes = (repeat each nodes)
  }

  public init(nodes: repeat each N) where repeat each N: BlockNode {
    self.nodes = (repeat each nodes)
  }

  public init(@GroupBuilder group: () -> NodeGroup<repeat each N>) where repeat each N: InlineNode {
    self.nodes = group().nodes
  }

  public init(@GroupBuilder group: () -> NodeGroup<repeat each N>) where repeat each N: BlockNode {
    self.nodes = group().nodes
  }
}

// When all children blocks are block nodes, the group is a block node itself.
extension ZeroOrMore: BlockNode where repeat each N: BlockNode {}
// When all children blocks are inline nodes, the group is an inline node itself.
extension ZeroOrMore: InlineNode where repeat each N: InlineNode {}

// MARK: - Schema

/// A ``Schema`` defines what a text document can contain/display.
public protocol Schema: Sendable {
  associatedtype Document: BlockNode

  var document: Document { get }
}

public struct MarkdownSchema: Schema {
  public init() {}

  public var document: some BlockNode {
    ZeroOrMore {
      Paragraph()
    }
  }
}

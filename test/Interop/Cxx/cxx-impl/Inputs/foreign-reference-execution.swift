import ForeignReference

// int takesNode(Node *_Nonnull n);
@cxx @implementation
public func takesNode(_ n: Node) -> Int32 { return n.value }

// int takesNullableNode(Node *_Nullable n);
@cxx @implementation
public func takesNullableNode(_ n: Node?) -> Int32 { return n?.value ?? -1 }

// Node *_Nonnull returnsRetainedNode(Node *_Nonnull n)
//     __attribute__((swift_attr("returns_retained")));
@cxx @implementation
public func returnsRetainedNode(_ n: Node) -> Node { return n }

// Node *_Nullable returnsNullableRetainedNode(Node *_Nonnull n, int null)
//     __attribute__((swift_attr("returns_retained")));
@cxx @implementation
public func returnsNullableRetainedNode(_ n: Node, _ null: Int32) -> Node? {
  return null != 0 ? nil : n
}

extension Node {
  // static Node *_Nonnull Node::passThrough(Node *_Nonnull n)
  //     __attribute__((swift_attr("returns_retained")));
  @cxx @implementation
  public static func passThrough(_ n: Node) -> Node { return n }
}

// Leaf *_Nonnull returnsRetainedLeaf(Leaf *_Nonnull l)
//     __attribute__((swift_attr("returns_retained")));
@cxx @implementation
public func returnsRetainedLeaf(_ l: Leaf) -> Leaf { return l }

// Singleton *_Nonnull returnsSingleton(Singleton *_Nonnull s);
@cxx @implementation
public func returnsSingleton(_ s: Singleton) -> Singleton { return s }

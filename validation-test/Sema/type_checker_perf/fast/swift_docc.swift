// RUN: %target-typecheck-verify-swift -solver-scope-threshold=2000

// We don't use anything from Foundation below, but Foundation adds a
// large number of == overloads. Import it if we can, because it makes
// the problem instance harder.
#if canImport(Foundation)
import Foundation
#endif

struct Graph {
  func nodeWithReference(_: Int) -> Node? { fatalError() }
}

struct DocumentationContext {
  func parents(of: Int) -> [Int] { fatalError() }
  var topicGraph: Graph
}

struct Node {
  var reference: Int
  var kind: Kind

  enum Kind {
    case tutorialTableOfContents
    case chapter
    case volume
  }
}

func analyze(_ node: Node, context: DocumentationContext) {
  _ = context.parents(of: node.reference)
    .compactMap({ context.topicGraph.nodeWithReference($0) })
    .first(where: { $0.kind == .tutorialTableOfContents || $0.kind == .chapter || $0.kind == .volume })
}


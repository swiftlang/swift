// RUN: %target-swift-frontend -emit-ir %s

public protocol DefaultInitializable {
  init()
}

public protocol AdjacencyEdge_ {
  associatedtype VertexID: Equatable
  associatedtype Properties

  var destination: VertexID { get set }
  var properties: Properties { get set }
}

public struct AdjacencyEdge<VertexID: Equatable, Properties>: AdjacencyEdge_ {
  public var destination: VertexID
  public var properties: Properties
}

public protocol AdjacencyVertex_ {
  associatedtype Edges: Collection where Edges.Element: AdjacencyEdge_
  associatedtype Properties

  var edges: Edges { get set }
  var properties: Properties { get set }
}

public struct AdjacencyVertex<Edges: Collection, Properties> : AdjacencyVertex_
  where Edges.Element: AdjacencyEdge_
{
  public var edges: Edges
  public var properties: Properties
}

public protocol BinaryFunction {
  associatedtype Parameter0
  associatedtype Parameter1
  associatedtype Result

  func callAsFunction(_: Parameter0, _: Parameter1) -> Result
}

public struct GeneralAdjacencyList<
  Spine: Collection, VertexIDToIndex: BinaryFunction
>
  where Spine.Element : AdjacencyVertex_,
        VertexIDToIndex.Parameter0 == Spine,
        VertexIDToIndex.Parameter1 == Spine.Element.Edges.Element.VertexID,
        VertexIDToIndex.Result == Spine.Index
{
  public let vertexIDToIndex: VertexIDToIndex
  public var spine: Spine
}

public struct IdentityVertexIDToIndex<Spine: Collection>: BinaryFunction
  where Spine.Element : AdjacencyVertex_,
        Spine.Element.Edges.Element.VertexID == Spine.Index
{
  public func callAsFunction(_: Spine, _ id: Spine.Index) -> Spine.Index {
    return id
  }
}

public extension GeneralAdjacencyList {
  typealias VertexID = VertexIDToIndex.Parameter1
  typealias VertexProperties = Spine.Element.Properties
  typealias EdgeProperties = Spine.Element.Edges.Element.Properties

  struct EdgeID: Equatable {
    /// The source vertex.
    let source: VertexIDToIndex.Parameter1
    /// The position of the edge in `source`'s list of edges.
    let targetIndex: Spine.Index
  }
}

public extension GeneralAdjacencyList
  where VertexIDToIndex == IdentityVertexIDToIndex<Spine>,
        Spine: RangeReplaceableCollection,
        Spine.Element.Edges: RangeReplaceableCollection
          & BidirectionalCollection // Because https://bugs.swift.org/browse/SR-12810
{
  func addVertex(storing properties: VertexProperties) -> VertexID {
    return spine.indices.first!
  }
}


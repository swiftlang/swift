public protocol Node: Source { }

public protocol Source {
    associatedtype Output
}

public final class GraphNode<U>: Node {
    public typealias Output = U
}

// RUN: %target-swift-frontend -typecheck %s -solver-scope-threshold=700
// REQUIRES: tools-release,no_asan

public class Cookie {
    public var url: String!
    public var port: Int16?
    public var name: String!
    public var path: String!
    public var value: String!
    public var comment: String?
    public var commentURL: String?

    private let fixedByteSize: Int32 = 56

    var totalByteCount: Int32 {
        return fixedByteSize + 
               (port != nil ? 2 : 0) +
               Int32(comment?.utf8.count ?? 0) +
               Int32(commentURL?.utf8.count ?? 0) +
               Int32(url.utf8.count) +
               Int32(name.utf8.count) +
               Int32(path.utf8.count) +
               Int32(value.utf8.count)
    }
}

// RUN: not %target-swift-frontend -typecheck %s

// https://github.com/apple/swift/issues/57752

public extension Array {
    func chunked(into size: Int) -> [[Element]] {
        return stride(from: 0, to: count, by: size).map { elt in
            self[elt


public extension Array where Element == Item {
    mutating func toggle(item: Item) -> Bool {
        _ = contains(item)
    }
}

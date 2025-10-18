// RUN: %target-swift-frontend %s -typecheck

extension Collection {
    func f() -> [Iterator.Element] {
        return Array(self.prefix(0))
    }
}

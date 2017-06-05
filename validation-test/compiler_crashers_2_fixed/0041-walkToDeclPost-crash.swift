// RUN: %target-swift-frontend %s -typecheck

extension Collection {
    func f() -> [Generator.Element] {
        return Array(self.prefix(0))
    }
}

// RUN: not %target-swift-frontend %s -parse

extension Collection {
    func f() -> [Generator.Element] {
        return Array(self.prefix(0))
    }
}

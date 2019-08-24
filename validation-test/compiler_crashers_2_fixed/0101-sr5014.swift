// RUN: not %target-swift-frontend -emit-ir -primary-file %s

struct Version {
}

extension CountableRange where Bound == Version {
    func contains(_ element: Version) -> Bool {
        fatalError()
    }
}

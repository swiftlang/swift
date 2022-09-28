// RUN: %target-swift-frontend -emit-ir -module-name M -primary-file %S/Inputs/issue-53550-other.swift -primary-file %s
// RUN: %target-swift-frontend -emit-ir -module-name M %S/Inputs/issue-53550-other.swift %s

// https://github.com/apple/swift/issues/53550

class WatchRegistry {
    func single<S: Snapshotting>(objectType: S.Type) throws -> Watch<S>
    {
        return try Watch<S>.singleObject(objectType: S.self, properties: S.ChangeType.PropertiesType.all)
    }
}

class Watch<SnapshotType : Snapshotting> {
    static func singleObject(objectType: SnapshotType.Type, properties: SnapshotType.ChangeType.PropertiesType) throws -> Watch<SnapshotType> {
        fatalError()
    }
}

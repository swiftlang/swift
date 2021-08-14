// RUN: %target-swift-frontend -emit-ir -primary-file %S/Inputs/sr11153_2_other.swift -primary-file %s
// RUN: %target-swift-frontend -emit-ir %S/Inputs/sr11153_2_other.swift %s

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

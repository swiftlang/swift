protocol Snapshotting {
    associatedtype NativeType: NativeInserting where NativeType.SnapshotType == Self
    associatedtype ChangeType: SnapshotChange where ChangeType.SnapshotType == Self
}

protocol NativeInserting {
    associatedtype SnapshotType : Snapshotting where SnapshotType.NativeType == Self
}

protocol SnapshotProperties : OptionSet where RawValue == Int {
    static var all: Self { get }
}

protocol SnapshotChange {
    associatedtype SnapshotType : Snapshotting where SnapshotType.ChangeType == Self
    associatedtype PropertiesType : SnapshotProperties
}

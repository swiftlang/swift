// RUN: not --crash %target-swift-frontend -emit-ir %s

// REQUIRES: asserts

public class OFMAttachment : NativeInserting {
    public typealias SnapshotType = Message.Attachment
}

public protocol Snapshotting {
    associatedtype NativeType: NativeInserting where NativeType.SnapshotType == Self
    associatedtype RecordType: SnapshotRecord where RecordType.SnapshotType == Self
    associatedtype ChangeType: SnapshotChange where ChangeType.SnapshotType == Self

    static var baseMessageName: String { get }
}

public protocol NativeInserting {
    associatedtype SnapshotType : Snapshotting where SnapshotType.NativeType == Self
}

public protocol SnapshotRecord {
    associatedtype SnapshotType : Snapshotting where SnapshotType.RecordType == Self

}

public protocol SnapshotChange {
    associatedtype SnapshotType : Snapshotting where SnapshotType.ChangeType == Self
}

public struct Message {
    public enum Attachment : Snapshotting {

        public static var baseMessageName: String = "attachment"

        public typealias NativeType = OFMAttachment
        public typealias RecordType = Record
        public typealias ChangeType = Change
        public struct Record : SnapshotRecord {
            public typealias SnapshotType = Message.Attachment
        }

        public struct Change : SnapshotChange {
            public typealias SnapshotType = Message.Attachment
        }

    }
}


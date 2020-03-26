// RUN: %target-swift-frontend -typecheck %s

class OFMAttachment : NativeInserting {
    typealias SnapshotType = Message.Attachment
}

protocol Snapshotting {
    associatedtype NativeType: NativeInserting where NativeType.SnapshotType == Self
    associatedtype RecordType: SnapshotRecord where RecordType.SnapshotType == Self
    associatedtype ChangeType: SnapshotChange where ChangeType.SnapshotType == Self

    static var baseMessageName: String { get }
}

protocol NativeInserting {
    associatedtype SnapshotType : Snapshotting where SnapshotType.NativeType == Self
}

protocol SnapshotRecord {
    associatedtype SnapshotType : Snapshotting where SnapshotType.RecordType == Self

}

protocol SnapshotChange {
    associatedtype SnapshotType : Snapshotting where SnapshotType.ChangeType == Self
}

struct Message {
    enum Attachment : Snapshotting {

        static var baseMessageName: String = "attachment"

        typealias NativeType = OFMAttachment
        typealias RecordType = Record
        typealias ChangeType = Change
        struct Record : SnapshotRecord {
            typealias SnapshotType = Message.Attachment
        }

        struct Change : SnapshotChange {
            typealias SnapshotType = Message.Attachment
        }

    }
}


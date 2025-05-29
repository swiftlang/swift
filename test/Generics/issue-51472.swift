// RUN: %target-typecheck-verify-swift
// RUN: %target-swift-frontend -typecheck %s -debug-generic-signatures 2>&1 | %FileCheck %s

// https://github.com/apple/swift/issues/51472

public class OFMAttachment : NativeInserting {
// expected-warning@-1 {{non-final class 'OFMAttachment' cannot safely conform to protocol 'NativeInserting', which requires that 'Self.SnapshotType.NativeType' is exactly equal to 'Self'; this is an error in the Swift 6 language mode}}
    public typealias SnapshotType = Message.Attachment
}

// CHECK-LABEL: .Snapshotting@
// CHECK-NEXT: Requirement signature: <Self where Self == Self.[Snapshotting]ChangeType.[SnapshotChange]SnapshotType, Self.[Snapshotting]ChangeType : SnapshotChange, Self.[Snapshotting]NativeType : NativeInserting, Self.[Snapshotting]RecordType : SnapshotRecord, Self.[Snapshotting]ChangeType.[SnapshotChange]SnapshotType == Self.[Snapshotting]NativeType.[NativeInserting]SnapshotType, Self.[Snapshotting]NativeType.[NativeInserting]SnapshotType == Self.[Snapshotting]RecordType.[SnapshotRecord]SnapshotType>
public protocol Snapshotting {
    associatedtype NativeType: NativeInserting where NativeType.SnapshotType == Self
    associatedtype RecordType: SnapshotRecord where RecordType.SnapshotType == Self
    associatedtype ChangeType: SnapshotChange where ChangeType.SnapshotType == Self

    static var baseMessageName: String { get }
}

// CHECK-LABEL: .NativeInserting@
// CHECK-NEXT: Requirement signature: <Self where Self == Self.[NativeInserting]SnapshotType.[Snapshotting]NativeType, Self.[NativeInserting]SnapshotType : Snapshotting>
public protocol NativeInserting {
    associatedtype SnapshotType : Snapshotting where SnapshotType.NativeType == Self
}

// CHECK-LABEL: .SnapshotRecord@
// CHECK-NEXT: Requirement signature: <Self where Self == Self.[SnapshotRecord]SnapshotType.[Snapshotting]RecordType, Self.[SnapshotRecord]SnapshotType : Snapshotting>
public protocol SnapshotRecord {
    associatedtype SnapshotType : Snapshotting where SnapshotType.RecordType == Self

}

// CHECK-LABEL: .SnapshotChange@
// CHECK-NEXT: Requirement signature: <Self where Self == Self.[SnapshotChange]SnapshotType.[Snapshotting]ChangeType, Self.[SnapshotChange]SnapshotType : Snapshotting>
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


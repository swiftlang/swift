// RUN: %target-swift-frontend -typecheck %s

// Reduced from Realm open source project: https://github.com/realm/realm-swift.git

internal protocol RealmCollectionImpl: RealmCollection where Index == Int, SubSequence == Slice<Self> {}

public protocol RealmCollection: RealmCollectionBase, Equatable {}

public protocol RealmCollectionBase: RandomAccessCollection, LazyCollectionProtocol, CustomStringConvertible, ThreadConfined where Element: RealmCollectionValue {
  typealias ElementType = Element
}

public protocol RealmCollectionValue: Hashable, _HasPersistedType where PersistedType: RealmCollectionValue {}

public protocol _HasPersistedType: _ObjcBridgeable {
  associatedtype PersistedType: _ObjcBridgeable
}

public protocol _ObjcBridgeable {}

public protocol ThreadConfined {}

public protocol PersistableEnum: _PersistableInsideOptional, RawRepresentable, CaseIterable, RealmEnum, _RealmCollectionValueInsideOptional, MinMaxType, Comparable where RawValue: Comparable {}

public protocol _PersistableInsideOptional: _Persistable where PersistedType: _PersistableInsideOptional {} 

public protocol _Persistable: _RealmSchemaDiscoverable, _HasPersistedType where PersistedType: _Persistable, PersistedType.PersistedType.PersistedType == PersistedType.PersistedType {}

public protocol RealmEnum: RealmOptionalType, _RealmSchemaDiscoverable {}

public protocol _RealmCollectionValueInsideOptional: RealmCollectionValue where PersistedType: _RealmCollectionValueInsideOptional {}

public protocol MinMaxType {}

public protocol _RealmSchemaDiscoverable {}

public protocol RealmOptionalType: _ObjcBridgeable {}

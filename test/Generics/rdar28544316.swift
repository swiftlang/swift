// RUN: %target-swift-frontend %s -emit-ir -debug-generic-signatures 2>&1 | %FileCheck %s
// REQUIRES: asserts

class PropertyDataSource<O: PropertyHosting> {
}

protocol TableViewCellFactoryType {
    associatedtype Item
}

public protocol PropertyHosting {
    associatedtype PType: Hashable, EntityOwned
}

public protocol EntityOwned: class {
    associatedtype Owner
}

public protocol PropertyType: class {
}

func useType<T>(cellType: T.Type) {
}

// The GSB would reject this declaration because it was "too minimal",
// missing the Factory.Item : EntityOwned requirement that is
// required to get a conformance-valid rewrite system.
//
// The Requirement Machine correctly infers this requirement and adds
// it during minimization.

// CHECK-LABEL: rdar28544316.(file).PropertyTableViewAdapter@
// CHECK-NEXT: Generic signature: <Factory where Factory : TableViewCellFactoryType, Factory.[TableViewCellFactoryType]Item : EntityOwned, Factory.[TableViewCellFactoryType]Item : PropertyType, Factory.[TableViewCellFactoryType]Item == Factory.[TableViewCellFactoryType]Item.[EntityOwned]Owner.[PropertyHosting]PType, Factory.[TableViewCellFactoryType]Item.[EntityOwned]Owner : PropertyHosting>

final class PropertyTableViewAdapter<Factory: TableViewCellFactoryType>
    where
    Factory.Item: PropertyType,
    Factory.Item.Owner: PropertyHosting,
    Factory.Item.Owner.PType == Factory.Item
{
    typealias Item = Factory.Item
    
    let dataManager: PropertyDataSource<Factory.Item.Owner>
    init(dataManager: PropertyDataSource<Factory.Item.Owner>) {
      useType(cellType: Factory.Item.Owner.self)
      self.dataManager = dataManager
    }
}

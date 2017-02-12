// RUN: %target-swift-frontend %s -emit-ir

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


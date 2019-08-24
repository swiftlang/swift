// RUN: %target-swift-frontend %s -emit-ir

public class Entity{
}
public class DataCollection<T>{
}
public protocol IEntityCollection : class{
	func AddEntity(_ entity:Entity)
}
public class EntityCollection<T:Entity> : DataCollection<T>, IEntityCollection{
	public func AddEntity(_ entity: Entity) {}
}
public class EntityReference2<TParentEntityCollection:EntityCollection<TParentEntity>, TChildEntityCollection:EntityCollection<TChildEntity>, TChildEntity:Entity, TParentEntity:Entity>
{
	public let i = 0
}

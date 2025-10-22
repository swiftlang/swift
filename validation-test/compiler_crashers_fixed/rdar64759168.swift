// RUN: %target-swift-frontend -emit-ir %s

public class Clazz {}

public protocol SelectableFieldValueProtocol {}

public protocol FieldProtocol {
    associatedtype SelectableValue : SelectableFieldValueProtocol
}

public protocol SelectFieldValueCoordinatorDelegate  {
    associatedtype Field : Clazz, FieldProtocol
}

public class SelectFieldValueCoordinator<Field, Delegate : SelectFieldValueCoordinatorDelegate> where Field == Delegate.Field {}

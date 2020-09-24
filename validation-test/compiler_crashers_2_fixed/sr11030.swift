// RUN: %target-swift-frontend -O -emit-ir %s

public class Coordinator: ConcreteCoordinator {
    public func coordinate() {

    }
}

public typealias ConcreteCoordinator = InternalBaseCoordinator & CoordinatorProtocol

public protocol CoordinatorProtocol {
    func coordinate()
}

public protocol ViewBridgeProtocol {
    func createCoordinator() -> ConcreteCoordinator
}

public class InternalBaseViewBridge {

    public func startCoordinator() {
        abstract().createCoordinator()
            .with(success: nil)
            .coordinate()
    }

    private func abstract() -> ViewBridgeProtocol {
        return self as! ViewBridgeProtocol
    }

}

public class InternalBaseCoordinator: InternalCoordinatorProtocol {
    
}

public protocol InternalCoordinatorProtocol: AnyObject {
    
}

extension InternalCoordinatorProtocol {
    public func with(success: ((ConcreteCoordinator) -> Void)?) -> Self {
        return self
    }
}

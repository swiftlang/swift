// RUN: %target-swift-frontend -emit-ir %s

public protocol P1 {}

public protocol P2 {
    associatedtype A1
    var id: String { get }
}

public protocol P3<A2> {
    associatedtype A2
    func execute()  -> A2
}

public protocol P4<A3>: P3 {
    associatedtype A3: P1
}

public protocol P5<A3>: P4 {}

public protocol P6 {
    func insert<A3: P1>(id: String, _ value: A3, replace: Bool) -> any P5<A3>
    func remove<A4: P2>(with key: A4) where A4.A1: P1
}

extension P6 {
    public func persist<A4: P2>(_ value: A4.A1?, for key: A4) where A4.A1: P1 {
        guard let value else {
            self.remove(with: key)
            return
        }

        _ = self.insert(id: key.id, value).execute()
    }
    
    public func insert<A3: P1>(id: String, _ value: A3)  -> any P5<A3> {
        self.insert(id: id, value, replace: true)
    }
}


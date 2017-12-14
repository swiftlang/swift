// RUN: %target-swift-frontend %s -emit-ir -o -

public protocol NumProt: Prot {
    func value() -> Double
}

extension Float: NumProt {
    public func value() -> Double {
        return Double(self)
    }
}

public protocol Prot: CustomStringConvertible {
    func getOp() -> Op
}

extension Prot {
    public func getOp() -> Op {
        return Op("\(self) ")
    }
}

public protocol CompProt: Prot {}

open class Op: CompProt {
    fileprivate var valueText = ""
    
    open var description: String {
        return "42"
    }
    
    open func getOp() -> Op {
        return self
    }

    public init(_ value: Double) {
        self.valueText = "\(value)"
    }

    public init(_ operationString: String) {
        self.valueText = operationString
    }
}

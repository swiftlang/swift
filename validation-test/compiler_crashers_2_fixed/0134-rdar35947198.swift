// RUN: %target-swift-frontend %s -O -emit-sil | %FileCheck %s

// CHECK-LABEL: sil shared [transparent] [thunk] @$sSf4main7NumProtA2aBP5valueSdyFTW : $@convention(witness_method: NumProt) (@in_guaranteed Float) -> Double
// CHECK-NOT: %1 = load %0 : $*Float
// CHECK-NOT: function_ref @$sSf4mainE5valueSdyF : $@convention(method) (Float) -> Double
// CHECK: %1 = struct_element_addr %0 : $*Float, #Float._value // user: %2
// CHECK: %2 = load %1 : $*Builtin.FPIEEE32               // user: %3
// CHECK: %3 = builtin "fpext_FPIEEE32_FPIEEE64"(%2 : $Builtin.FPIEEE32) : $Builtin.FPIEEE64 // user: %4
// CHECK: %4 = struct $Double (%3 : $Builtin.FPIEEE64)    // user: %5
// CHECK: return %4 : $Double

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

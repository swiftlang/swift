/// second_decl_class_1 Aaa.
public class second_decl_class_1 {
}

public struct second_decl_struct_1 {
    public var instanceVar: Int {
        get { return 1 }
        set {}
    }
    public enum NestedEnum {}
    public typealias NestedTypealias = Int
}

public enum second_decl_enum_1 {
    case Case1
    case Case2(Int)
}

public class second_decl_class_2 {
    public init() {}
}

public protocol second_decl_protocol_1 {
    associatedtype NestedTypealias
    subscript(i: Int) -> Double { get set }
}

public var (decl_var_2, decl_var_3): (Int, Float) = (1, 1.0)

/// Comment on package function
package func second_package_function() {}

/// Comment on SPI function
@_spi(DocSPI) public func second_spi_function() {}

#sourceLocation(file:"NonExistingSource.swift", line:100000)
public func functionAfterPoundSourceLoc() {}

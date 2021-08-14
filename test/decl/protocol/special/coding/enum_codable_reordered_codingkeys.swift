// RUN: %target-typecheck-verify-swift -verify-ignore-unknown

// The order of cases in the case specific CodingKeys enum should not matter
enum SimpleEnum : Codable {
    case a(x: Int, y: Double, z: Bool)
    
    enum ACodingKeys: CodingKey {
    	case z
    	case x
    	case y
    }
}
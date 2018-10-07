// RUN: %target-swift-frontend %s -emit-ir


public protocol E {
	associatedtype F
	
	static func g(_: F) -> Self
}

internal enum CF {
	case f
}

internal enum CE: E {
	case f(CF)
	
	static func g(_ f: CF) -> CE {
		return CE.f(f)
	}
	
	static let cf = CE.g(.f)
}

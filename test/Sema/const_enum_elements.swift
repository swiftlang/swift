// RUN: %target-typecheck-verify-swift

enum CarKind {
	static let wagon = "Wagon"
	case coupe
	case sedan
	case other(String)
}

extension CarKind {
	static var myCoupe: CarKind {
		return .coupe
	}
}

func getCarKind() -> CarKind { return .sedan }

func drive(_ k1: _const CarKind, k2: _const CarKind) {}

func main() {
	drive(.coupe, k2: .sedan)
	drive(.sedan, k2: .coupe)
	drive(CarKind.coupe, k2: CarKind.sedan)
	drive(CarKind.sedan, k2: CarKind.coupe)
	drive(.other(""), k2: .sedan) // expected-error {{expect a compile-time constant literal}}
	drive(.other(CarKind.wagon), k2: .sedan) // expected-error {{expect a compile-time constant literal}}

	drive(.myCoupe, k2: .sedan) // expected-error {{expect a compile-time constant literal}}
	drive(.coupe, k2: .myCoupe) // expected-error {{expect a compile-time constant literal}}
	drive(.coupe, k2: getCarKind()) // expected-error {{expect a compile-time constant literal}}
	drive(getCarKind(), k2: .coupe) // expected-error {{expect a compile-time constant literal}}
}

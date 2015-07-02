// RUN: not %target-swift-frontend %s -parse

protocol FixpointType {
	typealias Algebra : AlgebraicType
	init(_ : Algebra)
	var out: Algebra { get }}
protocol AlgebraicType {typealias Recur}
enum Expr<T>: AlgebraicType {
	typealias Recur = T
	case Null
	func map<U>(transform: T -> U) -> Expr<U> {return .Null}
}
func out<Fix: FixpointType>(v: Fix) -> Fix.Recur {return v.out}
func cata<T, Fix: FixpointType where Fix.Recur == Expr<Fix>>(f: Expr<T> -> T)(_ term: Fix) -> T {
	return f({ $0.map(cata(f)) }(out(term)))
}
struct Term: FixpointType, CustomDebugStringConvertible {
	typealias Algebra = Expr<Term>
	var debugDescription: String {return cata(Term.toDebugString)(self)}
	static func toDebugString(expression: Expr<String>) -> String {return ""}
}

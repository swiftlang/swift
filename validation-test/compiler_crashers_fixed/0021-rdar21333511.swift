// RUN: not %target-swift-frontend %s -typecheck

protocol Fixpoint {
	typealias Algebra : Algebraic
	init(_ : Algebra)
	var out: Algebra { get }}
protocol Algebraic {typealias Recur}
enum Expr<T>: Algebraic {
	typealias Recur = T
	case Null
	func map<U>(transform: T -> U) -> Expr<U> {return .Null}
}
func out<Fix: Fixpoint>(v: Fix) -> Fix.Recur {return v.out}
func cata<T, Fix: Fixpoint where Fix.Recur == Expr<Fix>>(f: Expr<T> -> T)(_ term: Fix) -> T {
	return f({ $0.map(cata(f)) }(out(term)))
}
struct Term: Fixpoint, CustomDebugStringConvertible {
	typealias Algebra = Expr<Term>
	var debugDescription: String {return cata(Term.toDebugString)(self)}
	static func toDebugString(expression: Expr<String>) -> String {return ""}
}

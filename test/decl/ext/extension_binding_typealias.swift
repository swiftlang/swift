// RUN: %target-typecheck-verify-swift

struct S: A.P { typealias T = Int }
extension S.T {}

enum A {}
extension A { protocol P {} }

func takesP(_: any A.P) {}
func testConformance() { takesP(S()) }

protocol Q {}
extension Q { typealias U = Int }

struct S2: Q { typealias V = U }
extension S2.V { func viaConformance() {} }

class Base: Q {}
class Derived: Base { typealias V = U }
extension Derived.V { func viaSuperclass() {} }

func testLookup() {
  0.viaConformance()
  0.viaSuperclass()
}

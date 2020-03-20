// RUN: %target-typecheck-verify-swift

enum Uninhabited {}
func returnsUninhabited() -> Uninhabited {}

func acceptsInt(_ x: Int) {}
protocol Proto {}
func acceptsProto(_ x: Proto) {}
func acceptsGenericParamConstrainedToProto<T: Proto>(_ x: T) {}

let x: Int = returnsUninhabited()
let y: (Int, String) = (1, returnsUninhabited())
let z: Int? = nil
let zz = z ?? returnsUninhabited()

acceptsInt(returnsUninhabited())
let a: Int = returnsUninhabited() + returnsUninhabited()

acceptsProto(returnsUninhabited())
acceptsGenericParamConstrainedToProto(returnsUninhabited())

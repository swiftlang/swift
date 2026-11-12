import _Differentiation
import ADCSLibrary

// Compile this as a second primary file in the same client module. Both files
// specialize importedBranch<Int>; their synthesized tape metadata must link.
@differentiable(reverse, wrt: x)
public func clientOther(_ x: Float, _ flag: Bool) -> Float {
  importedBranch(x, Int(1), flag)
}

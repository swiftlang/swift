import _Differentiation
import ADCSLibrary

// A second primary file specializes the same imported VJP.
@differentiable(reverse, wrt: x)
public func clientOther(_ x: Float, _ flag: Bool) -> Float {
  importedBranch(x, Int(1), flag)
}

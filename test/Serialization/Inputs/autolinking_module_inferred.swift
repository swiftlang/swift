@_exported import autolinking_public
import autolinking_other // inferred as @_usableFromInline
import autolinking_other2
import autolinking_private

public func bfunc(x: Int = afunc(), y: Int = afunc2()) {
  cfunc()
}

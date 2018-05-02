@_exported import autolinking_public
import autolinking_other // inferred as @_usableFromInline
import autolinking_private

public func bfunc(x: Int = afunc()) {
  cfunc()
}

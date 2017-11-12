
import Swift

@_silgen_name("closure_test_target")
@inline(never)
public func foo3() {
}

public func closure_invoker() {
  let f = foo3
  f()
}

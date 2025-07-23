import Swift

@_silgen_name("closure_test_target")
@inline(never)
public func foo3() {
  print("123")
}

public func closure_invoker() {
  foo3()
}

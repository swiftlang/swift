import Swift

@_silgen_name("test_target")
@inline(never)
public func foo3() {
}

public func foo1() {
  print("1")
}

public func foo2() {
  print("3")
  foo1()
  foo3()
}

struct SpecialInt {
  var value = 0
}

operator prefix +++ {}
operator postfix +++ {}

@prefix func +++(base: @inout SpecialInt) {
  base.value += 2
}

@postfix func +++(base: @inout SpecialInt) {
  println("use the prefix form instead")
}


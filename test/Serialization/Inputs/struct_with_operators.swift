struct SpecialInt {
  var value = 0
}

operator prefix +++ {}
operator postfix +++ {}

@prefix func +++(inout base: SpecialInt) {
  base.value += 2
}

@postfix func +++(inout base: SpecialInt) {
  println("use the prefix form instead")
}


// watchMe will get more and more specific
// By running the program at various stages, one can see if the incrementality
// is correct.
func whatToPrint() -> String {
  _ = Struct1InB() // only Struct1 will be parsed
  return watchMe(3)
}

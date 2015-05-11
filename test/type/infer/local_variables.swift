// RUN: %target-parse-verify-swift

func dict_to_array(_: Dictionary<String, Int>) -> [(String, Int)] {
  return Array<(String, Int)>()
}

func infer_type(i: Int, f: Float) {
  // Simple types
  var i2 = i
  i2 = i

  // Tuples
  var (i3, f2) = (i, f)
  var i_and_f = (i, f)
  i3 = i2
  f2 = f
  i_and_f = (i3, f2)
  _ = i_and_f
}

func infer_generic_args() {
  // Simple types
  var x : Dictionary = ["Hello" : 1]
  var i : Int = x["Hello"]!

  // Tuples
  var (d, s) : (Dictionary, Array) = ( ["Hello" : 1], [1, 2, 3] )
  i = d["Hello"]!
  i = s[i]

  // Function types
  let f : (Dictionary) -> Array = dict_to_array
  var _ : [(String, Int)] = f(d)
}


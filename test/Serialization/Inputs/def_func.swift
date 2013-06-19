import Builtin

func getZero() -> Builtin.Int64 {
  // Can't just return 0 because of <rdar://problem/14209859>
  var result : Builtin.Int64
  return result
}

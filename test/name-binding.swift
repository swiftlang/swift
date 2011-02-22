//===----------------------------------------------------------------------===//
// Name binding stress test
//===----------------------------------------------------------------------===//

// void is just a type alias for the empty tuple.
typealias void : ()
// int is just a type alias for the 32-bit integer type.
typealias int : __builtin_int32_type


func func1() {
  // Shadow int.
  oneof int { xyz, abc }
  // We get the shadowed version of int.
  var x : int = :abc;
}



//var x : x_ty;
//typealias x_ty : int


// typealias x : x
//var x : typeof(x)


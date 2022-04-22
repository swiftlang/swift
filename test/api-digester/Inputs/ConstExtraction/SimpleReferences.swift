_const let global_str = "abc"
_const let global_int = 3
_const let global_float = 3.2

class C {
  static _const let class_bool = false
  static _const let class_arr = [2, 2, 3]
  static _const let class_dict = [2:1, 2:1, 3:1]
}

func foo() {
  _ = global_str
  _ = global_int
  _ = global_float
  _ = C.class_bool
  _ = C.class_arr
  _ = C.class_dict
}

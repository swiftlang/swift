// RUN: %target-typecheck-verify-swift

// Infer bound generic types in closures
func dict_int_string_indexer(_ getElement: (Dictionary<Int, String>) -> String) -> String {
  let dict = [0 : "zero", 1 : "one", 2 : "two"];
  return getElement(dict)
}

func test_dict_int_string_indexer() {
  _ = dict_int_string_indexer({ (x : Dictionary) in x[1]! })
}

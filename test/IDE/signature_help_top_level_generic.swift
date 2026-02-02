// RUN: %target-swift-ide-test -signature-help -code-completion-token=TOP_LEVEL_GENERIC -source-filename=%s | %FileCheck %s --check-prefix=TOP_LEVEL_GENERIC

func add<T>(x: T, y: T, with adder: (T, T) -> T) -> T where T: AdditiveArithmetic {
  return adder(x, y)
}

add(x: "A", y: "B", with: #^TOP_LEVEL_GENERIC^#)
// TOP_LEVEL_GENERIC:     Begin signatures, 1 items
// TOP_LEVEL_GENERIC-DAG: Signature[Active]: add(<param name="x">x: String</param>, <param name="y">y: String</param>, <param name="adder" active>with: (String, String) -> String</param>) -> String

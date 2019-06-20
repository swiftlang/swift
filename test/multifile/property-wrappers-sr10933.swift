// RUN: %target-swift-frontend -typecheck -primary-file %s %S/Inputs/sr10933a.swift %S/Inputs/sr10933b.swift

// SR-10933: crash involving multiple files
class Holder {
  @IntWrapper(defaultValue: 100) var int: Int
}

func main() {
  let h = Holder()
}

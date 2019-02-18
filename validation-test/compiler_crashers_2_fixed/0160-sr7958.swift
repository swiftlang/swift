// RUN: %target-swift-frontend %s -emit-ir

func foo<U>(_ x: U?) {
  _ = "\(anyLabelHere: x)"
}

// This one also crashes in a slightly different place
func bar(_ x: Int?) {
  _ = "\(anyLabelHere: x)"
}

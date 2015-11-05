enum GraphicType: Int {
  case RectangleType = 1
  case CircleType = 2
}

func foo_t2() -> Bool {
  // Don't return anything on purpose, we want a SIL diagnostic here.
}

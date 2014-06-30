// Do not put any classes in this file. It's part of the test that no classes
// get serialized here.

@public enum TheEnum {
  case A, B, C(MyClass)
}

@public enum EquatableEnum {
  case A
}

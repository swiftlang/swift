// CHECK-LABEL: public enum FutureproofEnum : Swift.Int
public enum FutureproofEnum: Int {
  // CHECK-NEXT: case a{{$}}
  case a = 1
  // CHECK-NEXT: case b{{$}}
  case b = 10
  // CHECK-NEXT: case c{{$}}
  case c = 100
  // CHECK-NEXT: case d{{$}}
  case d
}

// CHECK-LABEL: public enum FrozenEnum : Swift.Int
@_frozen public enum FrozenEnum: Int {
  // CHECK-NEXT: case a{{$}}
  case a = 1
  // CHECK-NEXT: case b{{$}}
  case b = 10
  // CHECK-NEXT: case c{{$}}
  case c = 100
  // CHECK-NEXT: case d{{$}}
  case d
}

// CHECK-LABEL: public enum FutureproofObjCEnum : Swift.Int
@objc public enum FutureproofObjCEnum: Int {
  // CHECK-NEXT: case a = 1{{$}}
  case a = 1
  // CHECK-NEXT: case b = 10{{$}}
  case b = 10
  // CHECK-NEXT: case c = 100{{$}}
  case c = 100
  // CHECK-NEXT: case d{{$}}
  case d
}

// CHECK-LABEL: public enum FrozenObjCEnum : Swift.Int
@_frozen @objc public enum FrozenObjCEnum: Int {
  // CHECK-NEXT: case a = 1{{$}}
  case a = 1
  // CHECK-NEXT: case b = 10{{$}}
  case b = 10
  // CHECK-NEXT: case c = 100{{$}}
  case c = 100
  // CHECK-NEXT: case d{{$}}
  case d
}

// CHECK-LABEL: indirect public enum FutureproofIndirectEnum
public indirect enum FutureproofIndirectEnum {
  // CHECK-NEXT: case a{{$}}
  case a
  // CHECK-NEXT: case b(Swift.Int){{$}}
  case b(Int)
  // CHECK-NEXT: case c{{$}}
  case c
}

// CHECK-LABEL: indirect public enum FrozenIndirectEnum
@_frozen public indirect enum FrozenIndirectEnum {
  // CHECK-NEXT: case a{{$}}
  case a
  // CHECK-NEXT: case b(Swift.Int){{$}}
  case b(Int)
  // CHECK-NEXT: case c{{$}}
  case c
}

// CHECK-LABEL: public enum FutureproofIndirectCaseEnum
public enum FutureproofIndirectCaseEnum {
  // CHECK-NEXT: {{^}} case a{{$}}
  case a
  // CHECK-NEXT: indirect case b(Swift.Int){{$}}
  indirect case b(Int)
  // CHECK-NEXT: {{^}} case c{{$}}
  case c
}

// CHECK-LABEL: public enum FrozenIndirectCaseEnum
@_frozen public enum FrozenIndirectCaseEnum {
  // CHECK-NEXT: {{^}} case a{{$}}
  case a
  // CHECK-NEXT: indirect case b(Swift.Int){{$}}
  indirect case b(Int)
  // CHECK-NEXT: {{^}} case c{{$}}
  case c
}

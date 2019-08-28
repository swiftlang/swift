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

// CHECK-LABEL: public enum FutureproofObjCEnum : Swift.Int32
@objc public enum FutureproofObjCEnum: Int32 {
  // CHECK-NEXT: case a = 1{{$}}
  case a = 1
  // CHECK-NEXT: case b = 10{{$}}
  case b = 10
  // CHECK-NEXT: case c = 100{{$}}
  case c = 100
  // CHECK-NEXT: case d{{$}}
  case d
}

// CHECK-LABEL: public enum FrozenObjCEnum : Swift.Int32
@_frozen @objc public enum FrozenObjCEnum: Int32 {
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

// CHECK-LABEL: public enum FutureproofIndirectMultiCaseEnum
public enum FutureproofIndirectMultiCaseEnum {
  // CHECK-SINGLE-FRONTEND-NEXT: {{^}} case a1, a2{{$}}
  // CHECK-MULTI-FILE-NEXT: {{^}} case a1{{$}}
  // CHECK-MULTI-FILE-NEXT: {{^}} case a2{{$}}
  case a1, a2
  // CHECK-SINGLE-FRONTEND-NEXT: indirect case b1(Swift.Int), b2(Swift.Int){{$}}
  // CHECK-MULTI-FILE-NEXT: indirect case b1(Swift.Int){{$}}
  // CHECK-MULTI-FILE-NEXT: indirect case b2(Swift.Int){{$}}
  indirect case b1(Int), b2(Int)
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

// CHECK-LABEL: public enum FrozenIndirectMultiCaseEnum
@_frozen public enum FrozenIndirectMultiCaseEnum {
  // CHECK-SINGLE-FRONTEND-NEXT: {{^}} case a1, a2{{$}}
  // CHECK-MULTI-FILE-NEXT: {{^}} case a1{{$}}
  // CHECK-MULTI-FILE-NEXT: {{^}} case a2{{$}}
  case a1, a2
  // CHECK-SINGLE-FRONTEND-NEXT: indirect case b1(Swift.Int), b2(Swift.Int){{$}}
  // CHECK-MULTI-FILE-NEXT: indirect case b1(Swift.Int){{$}}
  // CHECK-MULTI-FILE-NEXT: indirect case b2(Swift.Int){{$}}
  indirect case b1(Int), b2(Int)
  // CHECK-NEXT: {{^}} case c{{$}}
  case c
}

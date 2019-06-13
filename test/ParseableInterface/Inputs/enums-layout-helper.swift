// CHECK-LABEL: public enum FutureproofEnum : Swift.Int
public enum FutureproofEnum: Int {
  // CHECK-NEXT: case a{{$}}
  case a = 1
  // CHECK-NEXT: case b{{$}}
  case b = 10
  // CHECK-NEXT: case c{{$}}
  case c = 100
}

// CHECK-LABEL: public enum FrozenEnum : Swift.Int
@_frozen public enum FrozenEnum: Int {
  // CHECK-NEXT: case a{{$}}
  case a = 1
  // CHECK-NEXT: case b{{$}}
  case b = 10
  // CHECK-NEXT: case c{{$}}
  case c = 100
}

// CHECK-LABEL: public enum FutureproofObjCEnum : Swift.Int
@objc public enum FutureproofObjCEnum: Int {
  // CHECK-NEXT: case a = 1{{$}}
  case a = 1
  // CHECK-NEXT: case b = 10{{$}}
  case b = 10
  // CHECK-NEXT: case c = 100{{$}}
  case c = 100
}

// CHECK-LABEL: public enum FrozenObjCEnum : Swift.Int
@_frozen @objc public enum FrozenObjCEnum: Int {
  // CHECK-NEXT: case a = 1{{$}}
  case a = 1
  // CHECK-NEXT: case b = 10{{$}}
  case b = 10
  // CHECK-NEXT: case c = 100{{$}}
  case c = 100
}

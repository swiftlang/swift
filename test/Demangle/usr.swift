// RUN: swift-demangle s:So19UIGestureRecognizerC9LambdaKitE7handlerAByAB_So0aB5StateVtc_tcfc | %FileCheck %s
// CHECK: $sSo19UIGestureRecognizerC9LambdaKitE7handlerAByAB_So0aB5StateVtc_tcfc ---> (extension in LambdaKit):__C.UIGestureRecognizer.init(handler: (__C.UIGestureRecognizer, __C.UIGestureRecognizerState) -> ()) -> __C.UIGestureRecognizer

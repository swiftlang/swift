// RUN: swift-demangle sSo19UIGestureRecognizerC9LambdaKitE7handlerAByAB_So0aB5StateVtc_tcfc | %FileCheck %s --check-prefix=ARG
// ARG: $sSo19UIGestureRecognizerC9LambdaKitE7handlerAByAB_So0aB5StateVtc_tcfc ---> (extension in LambdaKit):__C.UIGestureRecognizer.init(handler: (__C.UIGestureRecognizer, __C.UIGestureRecognizerState) -> ()) -> __C.UIGestureRecognizer

// RUN: echo sSo19UIGestureRecognizerC9LambdaKitE7handlerAByAB_So0aB5StateVtc_tcfc | swift-demangle | %FileCheck %s --check-prefix=STDIN
// STDIN: (extension in LambdaKit):__C.UIGestureRecognizer.init(handler: (__C.UIGestureRecognizer, __C.UIGestureRecognizerState) -> ()) -> __C.UIGestureRecognizer

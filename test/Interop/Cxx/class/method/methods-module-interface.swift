// RUN: %target-swift-ide-test -print-module -module-to-print=Methods -I %S/Inputs -source-filename=x -enable-experimental-cxx-interop | %FileCheck %s

// CHECK: mutating func nonConstMethod()
// CHECK: func constMethod()

// CHECK: mutating func nonConstPassThrough(_ a: CInt) -> CInt
// CHECK: func constPassThrough(_ a: CInt) -> CInt

// CHECK: mutating func nonConstSum(_ a: CInt, _ b: CInt) -> CInt
// CHECK: func constSum(_ a: CInt, _ b: CInt) -> CInt

// CHECK: mutating func nonConstSum(_ a: NonTrivialInWrapper, _ b: NonTrivialInWrapper) -> CInt
// CHECK: func constSum(_ a: NonTrivialInWrapper, _ b: NonTrivialInWrapper) -> CInt

// CHECK: mutating func nonConstSumAsWrapper(_ a: NonTrivialInWrapper, _ b: NonTrivialInWrapper) -> NonTrivialInWrapper
// CHECK: func constSumAsWrapper(_ a: NonTrivialInWrapper, _ b: NonTrivialInWrapper) -> NonTrivialInWrapper

// CHECK: mutating func nonConstPassThroughAsWrapper(_ a: CInt) -> NonTrivialInWrapper
// CHECK: func constPassThroughAsWrapper(_ a: CInt) -> NonTrivialInWrapper

// CHECK: struct HasInitMethods {
// CHECK:   init(field: CInt)
// CHECK:   init()
// CHECK:   func `init`() -> CInt
// CHECK:   mutating func initMutating(_ n: CInt) -> CInt
// CHECK: }

// CHECK: struct HasInitWithBackticks {
// CHECK:   init(x: CInt)
// CHECK:   func `init`() -> CInt
// CHECK: }

// CHECK: struct HasRenamedInitMethods {
// CHECK:   func start() -> CInt
// CHECK:   func startWith(_ a: CInt) -> CInt
// CHECK: }

// CHECK: struct HasStaticInitFactoryAndInitMethod {
// CHECK:   func `init`() -> CInt
// CHECK:   init(value: CInt)
// CHECK: }

// CHECK: struct HasNonInitializerStaticInitMethod {
// CHECK:   init()
// CHECK:   static func nonInitializer(_ value: CInt) -> CInt
// CHECK: }

// CHECK: struct ConstructorWithRenamedLabel {
// CHECK:   init(renamed v: CInt)
// CHECK: }

// CHECK: struct WithFactory {
// CHECK:   init(x: CInt)
// CHECK: }

// CHECK: extension WithFactory {
// CHECK:   init(n: CInt)
// CHECK: }

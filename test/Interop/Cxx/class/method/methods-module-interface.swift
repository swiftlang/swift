// RUN: %target-swift-ide-test -print-module -module-to-print=Methods -I %S/Inputs -source-filename=x -enable-experimental-cxx-interop | %FileCheck %s

// CHECK: mutating func nonConstMethod()
// CHECK: func constMethod()

// CHECK: mutating func nonConstPassThrough(_ a: Int32) -> Int32
// CHECK: func constPassThrough(_ a: Int32) -> Int32

// CHECK: mutating func nonConstSum(_ a: Int32, _ b: Int32) -> Int32
// CHECK: func constSum(_ a: Int32, _ b: Int32) -> Int32

// CHECK: mutating func nonConstSum(_ a: NonTrivialInWrapper, _ b: NonTrivialInWrapper) -> Int32
// CHECK: func constSum(_ a: NonTrivialInWrapper, _ b: NonTrivialInWrapper) -> Int32

// CHECK: mutating func nonConstSumAsWrapper(_ a: NonTrivialInWrapper, _ b: NonTrivialInWrapper) -> NonTrivialInWrapper
// CHECK: func constSumAsWrapper(_ a: NonTrivialInWrapper, _ b: NonTrivialInWrapper) -> NonTrivialInWrapper

// CHECK: mutating func nonConstPassThroughAsWrapper(_ a: Int32) -> NonTrivialInWrapper
// CHECK: func constPassThroughAsWrapper(_ a: Int32) -> NonTrivialInWrapper

// CHECK: struct HasInitMethods {
// CHECK:   init(field: Int32)
// CHECK:   init()
// CHECK:   func `init`() -> Int32
// CHECK:   mutating func initMutating(_ n: Int32) -> Int32
// CHECK: }

// CHECK: struct HasInitWithBackticks {
// CHECK:   init(x: Int32)
// CHECK:   func `init`() -> Int32
// CHECK: }

// CHECK: struct HasRenamedInitMethods {
// CHECK:   func start() -> Int32
// CHECK:   func startWith(_ a: Int32) -> Int32
// CHECK: }

// CHECK: struct HasStaticInitFactoryAndInitMethod {
// CHECK:   func `init`() -> Int32
// CHECK:   init(value: Int32)
// CHECK: }

// CHECK: struct HasNonInitializerStaticInitMethod {
// CHECK:   init()
// CHECK:   static func nonInitializer(_ value: Int32) -> Int32
// CHECK: }

// CHECK: struct ConstructorWithRenamedLabel {
// CHECK:   init(renamed v: Int32)
// CHECK: }

// CHECK: struct WithFactory {
// CHECK:   init(x: Int32)
// CHECK: }

// CHECK: extension WithFactory {
// CHECK:   init(n: Int32)
// CHECK: }

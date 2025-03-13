// REQUIRES: swift_swift_parser, swift_feature_ConcurrencySyntaxSugar, swift_feature_ClosureBodyMacro

// RUN: %target-swift-frontend -typecheck -plugin-path %swift-plugin-dir -enable-experimental-feature ConcurrencySyntaxSugar -enable-experimental-feature ClosureBodyMacro -language-mode 6 %s -dump-macro-expansions 2>&1 | %FileCheck %s

func f() async {}

// CHECK-LABEL: @__swiftmacro_10start_task4sync9StartTaskfMb_.swift
// CHECK: Task {
// CHECK:   await f()
// CHECK: }

@StartTask
func sync() {
  await f()
}

func takeClosure(
  _ closure: @escaping @Sendable () -> Void,
  v: Int = 42
) {
  closure()
}

func multipleClosures(
  a: @escaping @Sendable () -> Void,
  b: @escaping @Sendable () -> Void) {
}

func onClosure() {
  // CHECK-LABEL: @__swiftmacro_10start_task0021start_taskswift_IfFDefMX35_16_33_EEC79532ED9A2723128F952F754D3F84Ll9StartTaskfMb_.swift
  // CHECK: {
  // CHECK:     Task {
  // CHECK:         await f()
  // CHECK:     }
  // CHECK: }
  takeClosure { @StartTask in
    await f()
  }

  // CHECK-LABEL: @__swiftmacro_10start_task0021start_taskswift_IfFDefMX45_16_33_EEC79532ED9A2723128F952F754D3F84Ll9StartTaskfMb_.swift
  // CHECK: {
  // CHECK:     Task {
  // CHECK:         await f()
  // CHECK:     }
  // CHECK: }
  takeClosure({ @StartTask in
    await f()
  }, v: 0)

  // CHECK-LABEL: @__swiftmacro_10start_task0021start_taskswift_IfFDefMX55_21_33_EEC79532ED9A2723128F952F754D3F84Ll9StartTaskfMb_.swift
  // CHECK: {
  // CHECK:     Task {
  // CHECK:         await f()
  // CHECK:     }
  // CHECK: }
  multipleClosures { @StartTask in
    await f()
  } b: {
  }

  // CHECK-LABEL: @__swiftmacro_10start_task0021start_taskswift_IfFDefMX68_9_33_EEC79532ED9A2723128F952F754D3F84Ll9StartTaskfMb_.swift
  // CHECK: {
  // CHECK:     Task {
  // CHECK:         await f()
  // CHECK:     }
  // CHECK: }
  multipleClosures {
    _ = 42
  } b: { @StartTask in
    await f()
  }

  // CHECK-LABEL: @__swiftmacro_10start_task0021start_taskswift_IfFDefMX86_4_33_EEC79532ED9A2723128F952F754D3F84Ll9StartTaskfMb_.swift
  // CHECK: {
  // CHECK:     Task {
  // CHECK:         _ = 42
  // CHECK:     }
  // CHECK: }
  
  // CHECK-LABEL: @__swiftmacro_10start_task0021start_taskswift_IfFDefMX88_9_33_EEC79532ED9A2723128F952F754D3F84Ll9StartTaskfMb_.swift
  // CHECK: {
  // CHECK:     Task {
  // CHECK:         await f()
  // CHECK:     }
  // CHECK: }
  multipleClosures {
    @StartTask in
    _ = 42
  } b: { @StartTask in
    await f()
  }

  // CHECK-LABEL: @__swiftmacro_10start_task0021start_taskswift_IfFDefMX100_12_33_EEC79532ED9A2723128F952F754D3F84Ll9StartTaskfMb_.swift
  // CHECK: {
  // CHECK:     Task {
  // CHECK:         await f()
  // CHECK:     }
  // CHECK: }
  let _ = {
    func test() {
      _ = { @StartTask in await f() }
    }
  }

  // CHECK-LABEL: @__swiftmacro_10start_task0021start_taskswift_IfFDefMX114_54_33_EEC79532ED9A2723128F952F754D3F84Ll9StartTaskfMb_.swift
  // CHECK: {
  // CHECK:     Task {
  // CHECK:         await test()
  // CHECK:     }
  // CHECK: }
  let _ = {
    let y = 42
    func test() async { print(y) }
    
    if case let (_, closure) = (otherValue: 42, fn: { @StartTask in
         await test()
      }) {
      let _: Task<(), Never> = closure()
    }
  }
}

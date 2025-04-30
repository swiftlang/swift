// REQUIRES: swift_swift_parser, swift_feature_ConcurrencySyntaxSugar, swift_feature_ClosureBodyMacro

// RUN: %target-swift-frontend -typecheck -plugin-path %swift-plugin-dir -enable-experimental-feature ConcurrencySyntaxSugar -enable-experimental-feature ClosureBodyMacro -language-mode 6 %s -dump-macro-expansions -disable-availability-checking 2>&1 | %FileCheck %s

func f() async {}

// CHECK-LABEL: @__swiftmacro_10task_macro4sync4TaskfMb_.swift
// CHECK: Task {
// CHECK:   await f()
// CHECK: }

@Task
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
  // CHECK-LABEL: @__swiftmacro_10task_macro0021task_macroswift_IfFDefMX35_16_33_39E2B7F186C0B70273105736DD2F3721Ll4TaskfMb_.swift
  // CHECK: {
  // CHECK:     Task {
  // CHECK:         await f()
  // CHECK:     }
  // CHECK: }
  takeClosure { @Task in
    await f()
  }

  // CHECK-LABEL: @__swiftmacro_10task_macro0021task_macroswift_IfFDefMX45_16_33_39E2B7F186C0B70273105736DD2F3721Ll4TaskfMb_.swift
  // CHECK: {
  // CHECK:     Task {
  // CHECK:         await f()
  // CHECK:     }
  // CHECK: }
  takeClosure({ @Task in
    await f()
  }, v: 0)

  // CHECK-LABEL: @__swiftmacro_10task_macro0021task_macroswift_IfFDefMX55_21_33_39E2B7F186C0B70273105736DD2F3721Ll4TaskfMb_.swift
  // CHECK: {
  // CHECK:     Task {
  // CHECK:         await f()
  // CHECK:     }
  // CHECK: }
  multipleClosures { @Task in
    await f()
  } b: {
  }

  // CHECK-LABEL: @__swiftmacro_10task_macro0021task_macroswift_IfFDefMX68_9_33_39E2B7F186C0B70273105736DD2F3721Ll4TaskfMb_.swift
  // CHECK: {
  // CHECK:     Task {
  // CHECK:         await f()
  // CHECK:     }
  // CHECK: }
  multipleClosures {
    _ = 42
  } b: { @Task in
    await f()
  }

  // CHECK-LABEL: @__swiftmacro_10task_macro0021task_macroswift_IfFDefMX86_4_33_39E2B7F186C0B70273105736DD2F3721Ll4TaskfMb_.swift
  // CHECK: {
  // CHECK:     Task {
  // CHECK:         _ = 42
  // CHECK:     }
  // CHECK: }
  
  // CHECK-LABEL: @__swiftmacro_10task_macro0021task_macroswift_IfFDefMX88_9_33_39E2B7F186C0B70273105736DD2F3721Ll4TaskfMb_.swift
  // CHECK: {
  // CHECK:     Task {
  // CHECK:         await f()
  // CHECK:     }
  // CHECK: }
  multipleClosures {
    @Task in
    _ = 42
  } b: { @Task in
    await f()
  }

  // CHECK-LABEL: @__swiftmacro_10task_macro0021task_macroswift_IfFDefMX100_12_33_39E2B7F186C0B70273105736DD2F3721Ll4TaskfMb_.swift
  // CHECK: {
  // CHECK:     Task {
  // CHECK:         await f()
  // CHECK:     }
  // CHECK: }
  let _ = {
    func test() {
      _ = { @Task in await f() }
    }
  }

  // CHECK-LABEL: @__swiftmacro_10task_macro0021task_macroswift_IfFDefMX114_54_33_39E2B7F186C0B70273105736DD2F3721Ll4TaskfMb_.swift
  // CHECK: {
  // CHECK:     Task {
  // CHECK:         await test()
  // CHECK:     }
  // CHECK: }
  let _ = {
    let y = 42
    func test() async { print(y) }
    
    if case let (_, closure) = (otherValue: 42, fn: { @Task in
         await test()
      }) {
      let _: Task<(), Never> = closure()
    }
  }
}

func onClosureWithArguments() {
  // CHECK-LABEL: @__swiftmacro_10task_macro0021task_macroswift_IfFDefMX129_16_33_39E2B7F186C0B70273105736DD2F3721Ll4TaskfMb_.swift
  // CHECK: {
  // CHECK:     Task { @MainActor in
  // CHECK:         await f()
  // CHECK:     }
  // CHECK: }
  takeClosure { @Task(on: MainActor.shared) in
    await f()
  }

  // CHECK-LABEL: @__swiftmacro_10task_macro0021task_macroswift_IfFDefMX139_16_33_39E2B7F186C0B70273105736DD2F3721Ll4TaskfMb_.swift
  // CHECK: {
  // CHECK:     Task(name: "MyTask") { @MainActor in
  // CHECK:         await f()
  // CHECK:     }
  // CHECK: }
  takeClosure { @Task(on: MainActor.shared, name: "MyTask") in
    await f()
  }

  // CHECK-LABEL: @__swiftmacro_10task_macro0021task_macroswift_IfFDefMX149_16_33_39E2B7F186C0B70273105736DD2F3721Ll4TaskfMb_.swift
  // CHECK: {
  // CHECK:     Task(name: "MyTask", priority: .high) {
  // CHECK:         await f()
  // CHECK:     }
  // CHECK: }
  takeClosure { @Task(name: "MyTask", priority: .high) in
    await f()
  }
}

// FIXME: Remove `-disable-availability-checking` from the run line after
// SE-0469 review wraps up. This comment is at the end of the file because
// closure body macro mangling includes source locations.

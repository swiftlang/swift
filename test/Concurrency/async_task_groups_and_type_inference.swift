// RUN: %target-swift-frontend  -disable-availability-checking %s -emit-sil -o /dev/null -verify -strict-concurrency=complete

// REQUIRES: concurrency
// REQUIRES: asserts

@available(SwiftStdlib 5.1, *)
func test_TaskGroupTypeInference() async {
  // Specify both `ChildTaskResult` and `GroupResult`.

  let str1: String = await withTaskGroup(of: Int.self, returning: String.self) { group in
    group.addTask { 1 }
    group.addTask { 2 }

    return "Hello, world!"
  }

  let int1: Int = await withTaskGroup(of: Void.self, returning: Int.self) { group in
    return 1
  }

  let void1: Void = await withTaskGroup(of: Void.self, returning: Void.self) { group in
    group.addTask { }
    group.addTask { }
  }

  // Infer `GroupResult`; specify `ChildTaskResult`.

  let str2: String = await withTaskGroup(of: Int.self) { group in
    group.addTask { 1 }
    group.addTask { 2 }

    return "Hello, world!"
  }

  let int2: Int = await withTaskGroup(of: Void.self) { group in
    return 1
  }

  let void2: Void = await withTaskGroup(of: Void.self) { group in
    group.addTask { }
    group.addTask { }
  }

  // Infer both `ChildTaskResult` and `GroupResult`.

  let str3: String = await withTaskGroup { group in
    group.addTask { 1 }
    group.addTask { 2 }

    return "Hello, world!"
  }

  let int3: Int = await withTaskGroup { group in
    return 1
  }

  let void3: Void = await withTaskGroup { group in
    group.addTask { }
    group.addTask { }
  }

  // Minimum type information.

  let str4 = await withTaskGroup { group in
    group.addTask { 1 }
    group.addTask { 2 }
    // CHECK: str4 TaskGroup type=TaskGroup<Int>
    print("str4 TaskGroup type=\(type(of: group))")
  }
  // CHECK str4 type=String
  print("str4 type=\(type(of: str4))")

  let int4 = await withTaskGroup { group in
    // CHECK: int4 TaskGroup type=TaskGroup<Void>
    print("int4 TaskGroup type=\(type(of: group))")
    return 1
  }
  // CHECK int4 type=Int
  print("int4 type=\(type(of: int4))")

  let void4 = await withTaskGroup { group in
    group.addTask { }
    group.addTask { }
    // CHECK: void4 TaskGroup type=TaskGroup<Void>
    print("void4 TaskGroup type=\(type(of: group))")
  }
  // CHECK void4 type=Void
  print("void4 type=\(type(of: void4))")
}

@available(SwiftStdlib 5.1, *)
func test_ThrowingTaskGroupTypeInference() async {
    // Specify both `ChildTaskResult` and `GroupResult`.

  let str1: String = await withThrowingTaskGroup(of: Int.self, returning: String.self) { group in
    group.addTask { 1 }
    group.addTask { 2 }

    return "Hello, world!"
  }

  let int1: Int = await withThrowingTaskGroup(of: Void.self, returning: Int.self) { group in
    return 1
  }

  let void1: Void = await withThrowingTaskGroup(of: Void.self, returning: Void.self) { group in
    group.addTask { }
    group.addTask { }
  }

  // Infer `GroupResult`; specify `ChildTaskResult`.

  let str2: String = await withThrowingTaskGroup(of: Int.self) { group in
    group.addTask { 1 }
    group.addTask { 2 }

    return "Hello, world!"
  }

  let int2: Int = await withThrowingTaskGroup(of: Void.self) { group in
    return 1
  }

  let void2: Void = await withThrowingTaskGroup(of: Void.self) { group in
    group.addTask { }
    group.addTask { }
  }

  // Infer both `ChildTaskResult` and `GroupResult`.

  let str3: String = await withThrowingTaskGroup { group in
    group.addTask { 1 }
    group.addTask { 2 }

    return "Hello, world!"
  }

  let int3: Int = await withThrowingTaskGroup { group in
    return 1
  }

  let void3: Void = await withThrowingTaskGroup { group in
    group.addTask { }
    group.addTask { }
  }

  // Minimum type information.

  let str4 = await withThrowingTaskGroup { group in
    group.addTask { 1 }
    group.addTask { 2 }
    // CHECK: str4 TaskGroup type=ThrowingTaskGroup<Int, Never>
    print("str4 TaskGroup type=\(type(of: group))")
  }
  // CHECK str4 type=String
  print("str4 type=\(type(of: str4))")

  let int4 = await withThrowingTaskGroup { group in
    // CHECK: int4 TaskGroup type=ThrowingTaskGroup<Void, Never>
    print("int4 TaskGroup type=\(type(of: group))")
    return 1
  }
  // CHECK int4 type=Int
  print("int4 type=\(type(of: int4))")

  let void4 = await withThrowingTaskGroup { group in
    group.addTask { }
    group.addTask { }
    // CHECK: void4 TaskGroup type=ThrowingTaskGroup<Void, Never>
    print("void4 TaskGroup type=\(type(of: group))")
  }
  // CHECK void4 type=Void
  print("void4 type=\(type(of: void4))")
}
// RUN: %empty-directory(%t)
// RUN: %{python} %utils/split_file.py -o %t %s

// RUN: %target-swift-frontend %t/Main.swift -parse-as-library -import-bridging-header %t/BridgingHeader.h -enable-experimental-feature Embedded -c -o %t/main.o
// RUN: %target-clang %t/main.o -o %t/a.out -dead_strip -pthreads
// RUN: %target-run %t/a.out | %FileCheck %s

// REQUIRES: swift_in_compiler
// REQUIRES: executable_test
// REQUIRES: optimized_stdlib
// REQUIRES: OS=macosx || OS=linux-gnu
// REQUIRES: swift_feature_Embedded

// BEGIN BridgingHeader.h

typedef void *pthread_t;
int pthread_create(pthread_t *thread, const void *attr, void *(*start_routine)(void *), void *arg);
int pthread_join(pthread_t thread, void **value_ptr);
unsigned int sleep(unsigned int);
int usleep(unsigned int);

// BEGIN Main.swift

public struct MyStructWithAnExpensiveInitializer {
  static var singleton = MyStructWithAnExpensiveInitializer()
  static var n = 0

  init() {
    print("MyStructWithAnExpensiveInitializer.init")
    usleep(100_000)
    MyStructWithAnExpensiveInitializer.n += 1
    print("MyStructWithAnExpensiveInitializer.init done")
  }

  func foo() {
    precondition(MyStructWithAnExpensiveInitializer.n == 1)
  }
}

@main
struct Main {
  static func main() {
    print("Start")

    var t1 = pthread_t(bitPattern: 0)
    pthread_create(&t1, nil, { _ in
      MyStructWithAnExpensiveInitializer.singleton.foo()
      return nil
    }, nil)

    var t2 = pthread_t(bitPattern: 0)
    pthread_create(&t2, nil, { _ in
      MyStructWithAnExpensiveInitializer.singleton.foo()
      return nil
    }, nil)

    pthread_join(t1, nil)
    pthread_join(t2, nil)

    print("All done")
    
    // CHECK: Start
    // CHECK-NEXT: MyStructWithAnExpensiveInitializer.init
    // CHECK-NEXT: MyStructWithAnExpensiveInitializer.init done
    // CHECK-NEXT: All done
  }
}

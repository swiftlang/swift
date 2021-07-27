// RUN: %target-swift-ide-test -batch-code-completion -source-filename %s -filecheck %raw-FileCheck -completion-output-dir %t 

// REQUIRES: concurrency

func funcThrows() throws {
    fatalError()
}
func asyncThrows() async throws {
    fatalError()
}
func asyncRethrows(fn : () async throws -> Int) async rethrows -> Int {
    fatalError()
}
func asyncRethrows(fn : () async throws -> String) async rethrows -> String {
    fatalError()
}
func invoke<T>(fn : () async throws -> T) async rethrows -> T {
    fatalError()
}
func invokeAuto<T>(_ val : @autoclosure () async throws -> T) async rethrows -> T {
    fatalError()
}
func normalTask() async -> Int {
    fatalError()
}
func throwingTask() async throws -> String {
    fatalError()
}

// CHECK_syncContext: Begin completions
// CHECK_syncContext-DAG: Decl[FreeFunction]/CurrModule: funcThrows()[' throws'][#Void#];
// CHECK_syncContext-DAG: Decl[FreeFunction]/CurrModule/NotRecommended: asyncRethrows({#fn: () async throws -> Int##() async throws -> Int#})[' async'][' rethrows'][#Int#];
// CHECK_syncContext-DAG: Decl[FreeFunction]/CurrModule/NotRecommended: asyncRethrows({#fn: () async throws -> String##() async throws -> String#})[' async'][' rethrows'][#String#];
// CHECK_syncContext-DAG: Decl[FreeFunction]/CurrModule/NotRecommended: invokeAuto({#(val): T#})[' async'][' rethrows'][#T#];
// CHECK_syncContext-DAG: Decl[FreeFunction]/CurrModule/NotRecommended: throwingTask()[' async'][' throws'][#String#];
// CHECK_syncContext-DAG: Decl[FreeFunction]/CurrModule/NotRecommended: invoke({#fn: () async throws -> T##() async throws -> T#})[' async'][' rethrows'][#T#];
// CHECK_syncContext-DAG: Decl[FreeFunction]/CurrModule/NotRecommended: normalTask()[' async'][#Int#];
// CHECK_syncContext: End completions

// CHECK_asyncContext: Begin completions
// CHECK_asyncContext-DAG: Decl[FreeFunction]/CurrModule: funcThrows()[' throws'][#Void#];
// CHECK_asyncContext-DAG: Decl[FreeFunction]/CurrModule: asyncRethrows({#fn: () async throws -> Int##() async throws -> Int#})[' async'][' rethrows'][#Int#];
// CHECK_asyncContext-DAG: Decl[FreeFunction]/CurrModule: asyncRethrows({#fn: () async throws -> String##() async throws -> String#})[' async'][' rethrows'][#String#];
// CHECK_asyncContext-DAG: Decl[FreeFunction]/CurrModule: invokeAuto({#(val): T#})[' async'][' rethrows'][#T#];
// CHECK_asyncContext-DAG: Decl[FreeFunction]/CurrModule: throwingTask()[' async'][' throws'][#String#];
// CHECK_asyncContext-DAG: Decl[FreeFunction]/CurrModule: invoke({#fn: () async throws -> T##() async throws -> T#})[' async'][' rethrows'][#T#];
// CHECK_asyncContext-DAG: Decl[FreeFunction]/CurrModule: normalTask()[' async'][#Int#];
// CHECK_asyncContext: End completions

func syncFunc() {
    #^CHECK_syncFunc?check=CHECK_syncContext^#
}
func syncClosure() async {
    func handleSyncClosure<T>(_: () -> T) {}
    handleSyncClosure {
        #^CHECK_syncClosure?check=CHECK_syncContext^#
    }
}
func syncClosure() {
  func handleAsyncClosure<T>(_: () async -> T) async {}
  handleAsyncClosure {
    #^CHECK_asyncClosure?check=CHECK_asyncContext^#
  }
}
func asyncFunc() async {
  #^CHECK_asyncFunc?check=CHECK_asyncContext^#
}

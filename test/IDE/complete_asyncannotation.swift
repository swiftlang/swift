// RUN: %target-swift-ide-test -batch-code-completion -source-filename %s -filecheck %raw-FileCheck -completion-output-dir %t -enable-experimental-concurrency

func globalFuncAsync() async {}
func globalFuncAsyncThrows() async throws {}
func globalFuncAsyncRethrows(_ x: () async throws -> ()) async rethrows {}
struct HasAsyncMembers {
  func memberAsync() async {}
  func memberAsyncThrows() async throws {}
  func memberAsyncRethrows(_ x: () async throws -> ()) async rethrows {}

  init() async {}
  init(withAsync: Int) async {}
  init(withAsyncThrows: Int) async throws {}
  init(withAsyncRethrows: () async throws -> Void) async rethrows {}
}

func testGlobalFuncAsync() {
  globalFuncAsync#^CHECK_globalFuncAsync^#
// CHECK_globalFuncAsync: Begin completions
// CHECK_globalFuncAsync-DAG: Decl[FreeFunction]/CurrModule: ()[' async'][#Void#]; name=() async
// CHECK_globalFuncAsync: End completions
}
func testGlobalFuncAsyncThrows() {
  globalFuncAsyncThrows#^CHECK_globalFuncAsyncThrows^#
// CHECK_globalFuncAsyncThrows: Begin completions
// CHECK_globalFuncAsyncThrows-DAG: Decl[FreeFunction]/CurrModule: ()[' async'][' throws'][#Void#]; name=() async throws
// CHECK_globalFuncAsyncThrows: End completions
}
func testGlobalFuncAsyncRethrows() {
  globalFuncAsyncRethrows#^CHECK_globalFuncAsyncRethrows^#
// CHECK_globalFuncAsyncRethrows: Begin completions
// CHECK_globalFuncAsyncRethrows-DAG: Decl[FreeFunction]/CurrModule: ({#(x): () async throws -> ()##() async throws -> ()#})[' async'][' rethrows'][#Void#]; name=(x: () async throws -> ()) async rethrows
// CHECK_globalFuncAsyncRethrows: End completions
}
func testAsyncMembers(_ x: HasAsyncMembers) {
  x.#^CHECK_members^#
// CHECK_members: Begin completions
// CHECK_members-DAG: Decl[InstanceMethod]/CurrNominal:   memberAsync()[' async'][#Void#]; name=memberAsync() async
// CHECK_members-DAG: Decl[InstanceMethod]/CurrNominal:   memberAsyncThrows()[' async'][' throws'][#Void#]; name=memberAsyncThrows() async throws
// CHECK_members-DAG: Decl[InstanceMethod]/CurrNominal:   memberAsyncRethrows {|}[' async'][' rethrows'][#Void#]; name=memberAsyncRethrows { code } async rethrows
// CHECK_members-DAG: Decl[InstanceMethod]/CurrNominal:   memberAsyncRethrows({#(x): () async throws -> ()##() async throws -> ()#})[' async'][' rethrows'][#Void#]; name=memberAsyncRethrows(x: () async throws -> ()) async rethrows
// CHECK_members: End completions
}
func testMemberAsync(_ x: HasAsyncMembers) {
  x.memberAsync#^CHECK_memberAsync^#
// CHECK_memberAsync: Begin completions
// CHECK_memberAsync-DAG: Decl[InstanceMethod]/CurrNominal: ()[' async'][#Void#]; name=() async
// CHECK_memberAsync: End completions
}
func testMemberAsyncThrows(_ x: HasAsyncMembers) {
  x.memberAsyncThrows#^CHECK_memberAsyncThrows^#
// CHECK_memberAsyncThrows: Begin completions
// CHECK_memberAsyncThrows-DAG: Decl[InstanceMethod]/CurrNominal: ()[' async'][' throws'][#Void#]; name=() async throws
// CHECK_memberAsyncThrows: End completions
}
func testMemberAsyncRethrows(_ x: HasAsyncMembers) {
  x.memberAsyncRethrows#^CHECK_memberAsyncRethrows^#
// CHECK_memberAsyncRethrows: Begin completions
// CHECK_memberAsyncRethrows-DAG: Decl[InstanceMethod]/CurrNominal: ({#(x): () async throws -> ()##() async throws -> ()#})[' async'][' rethrows'][#Void#]; name=(x: () async throws -> ()) async rethrows
// CHECK_memberAsyncRethrows: End completions
}

func testAsyncIntiializers() {
  HasAsyncMembers(#^CHECK_initializers^#
// CHECK_initializers: Begin completions
// CHECK_initializers-DAG: Decl[Constructor]/CurrNominal: ['('][')'][#HasAsyncMembers#]; name=
// CHECK_initializers-DAG: Decl[Constructor]/CurrNominal: ['(']{#withAsync: Int#}[')'][#HasAsyncMembers#]; name=withAsync: Int
// CHECK_initializers-DAG: Decl[Constructor]/CurrNominal: ['(']{#withAsyncThrows: Int#}[')'][' throws'][#HasAsyncMembers#]; name=withAsyncThrows: Int throws
// CHECK_initializers-DAG: Decl[Constructor]/CurrNominal: ['(']{#withAsyncRethrows: () async throws -> Void##() async throws -> Void#}[')'][' rethrows'][#HasAsyncMembers#]; name=withAsyncRethrows: () async throws -> Void rethrows
// CHECK_initializers: End completions
}

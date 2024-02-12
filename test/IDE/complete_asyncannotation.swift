// RUN: %batch-code-completion

// REQUIRES: concurrency

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

func testGlobalFuncAsync() async {
  globalFuncAsync#^CHECK_globalFuncAsync^#
// CHECK_globalFuncAsync-DAG: Decl[FreeFunction]/CurrModule/Flair[ArgLabels]: ()[' async'][#Void#]; name=(){{$}}
}
func testGlobalFuncAsyncThrows() async {
  globalFuncAsyncThrows#^CHECK_globalFuncAsyncThrows^#
// CHECK_globalFuncAsyncThrows-DAG: Decl[FreeFunction]/CurrModule/Flair[ArgLabels]: ()[' async'][' throws'][#Void#]; name=(){{$}}
}
func testGlobalFuncAsyncRethrows() async {
  globalFuncAsyncRethrows#^CHECK_globalFuncAsyncRethrows^#
// CHECK_globalFuncAsyncRethrows-DAG: Decl[FreeFunction]/CurrModule/Flair[ArgLabels]: ({#(x): () async throws -> ()##() async throws -> ()#})[' async'][' rethrows'][#Void#]; name=(:){{$}}
}
func testAsyncMembers(_ x: HasAsyncMembers) async {
  x.#^CHECK_members^#
// CHECK_members-DAG: Decl[InstanceMethod]/CurrNominal:   memberAsync()[' async'][#Void#]; name=memberAsync(){{$}}
// CHECK_members-DAG: Decl[InstanceMethod]/CurrNominal:   memberAsyncThrows()[' async'][' throws'][#Void#]; name=memberAsyncThrows(){{$}}
// CHECK_members-DAG: Decl[InstanceMethod]/CurrNominal:   memberAsyncRethrows {|}[' async'][' rethrows'][#Void#]; name=memberAsyncRethrows{{$}}
// CHECK_members-DAG: Decl[InstanceMethod]/CurrNominal:   memberAsyncRethrows({#(x): () async throws -> ()##() async throws -> ()#})[' async'][' rethrows'][#Void#]; name=memberAsyncRethrows(:){{$}}
}
func testMemberAsync(_ x: HasAsyncMembers) async {
  x.memberAsync#^CHECK_memberAsync^#
// CHECK_memberAsync-DAG: Decl[InstanceMethod]/CurrNominal/Flair[ArgLabels]: ()[' async'][#Void#]; name=(){{$}}
}
func testMemberAsyncThrows(_ x: HasAsyncMembers) async {
  x.memberAsyncThrows#^CHECK_memberAsyncThrows^#
// CHECK_memberAsyncThrows-DAG: Decl[InstanceMethod]/CurrNominal/Flair[ArgLabels]: ()[' async'][' throws'][#Void#]; name=(){{$}}
}
func testMemberAsyncRethrows(_ x: HasAsyncMembers) async {
  x.memberAsyncRethrows#^CHECK_memberAsyncRethrows^#
// CHECK_memberAsyncRethrows-DAG: Decl[InstanceMethod]/CurrNominal/Flair[ArgLabels]: ({#(x): () async throws -> ()##() async throws -> ()#})[' async'][' rethrows'][#Void#]; name=(:){{$}}
}

func testAsyncIntiializers() async {
  HasAsyncMembers(#^CHECK_initializers^#
// CHECK_initializers-DAG: Decl[Constructor]/CurrNominal/Flair[ArgLabels]: ['('][')'][' async'][#HasAsyncMembers#]; name={{$}}
// CHECK_initializers-DAG: Decl[Constructor]/CurrNominal/Flair[ArgLabels]: ['(']{#withAsync: Int#}[')'][' async'][#HasAsyncMembers#]; name=withAsync:{{$}}
// CHECK_initializers-DAG: Decl[Constructor]/CurrNominal/Flair[ArgLabels]: ['(']{#withAsyncThrows: Int#}[')'][' async'][' throws'][#HasAsyncMembers#]; name=withAsyncThrows:{{$}}
// CHECK_initializers-DAG: Decl[Constructor]/CurrNominal/Flair[ArgLabels]: ['(']{#withAsyncRethrows: () async throws -> Void##() async throws -> Void#}[')'][' async'][' rethrows'][#HasAsyncMembers#]; name=withAsyncRethrows:{{$}}
}

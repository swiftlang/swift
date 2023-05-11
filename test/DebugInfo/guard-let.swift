// RUN: %target-swift-frontend %use_no_opaque_pointers %s -c -emit-ir -g -o - | \
// RUN:   %FileCheck %s --check-prefix=CHECK1
// RUN: %target-swift-frontend %use_no_opaque_pointers %s -c -emit-ir -g -o - | \
// RUN:   %FileCheck %s --check-prefix=CHECK2
// RUN: %target-swift-frontend %use_no_opaque_pointers %s -c -emit-ir -g -o - | \
// RUN:   %FileCheck %s --check-prefix=CHECK3
// RUN: %target-swift-frontend %use_no_opaque_pointers %s -c -emit-ir -g -o -

// UNSUPPORTED: OS=watchos

// With large type optimizations the string is passed indirectly on the
// following architectures so there is no shadow copy happening. As this
// tests that we're emitting the DI correctly, we can skip running on them.
// UNSUPPORTED: CPU=i386
// UNSUPPORTED: CPU=armv7
// UNSUPPORTED: CPU=armv7s
// UNSUPPORTED: CPU=armv7k

func use<T>(_ t: T) {}

public func f(_ i : Int?)
{
  // CHECK1-LABEL: define {{.*}}@"$s4main1fyySiSgF"
  // CHECK1: %i.debug = alloca %TSiSg
  // CHECK1: @llvm.dbg.declare(metadata %TSiSg* %i.debug
  // CHECK1: %[[BITCAST:.*]] = bitcast %TSiSg* %i.debug to i8*
  // CHECK1: call void @llvm.memset{{.*}}(i8* align {{(4|8)}} %[[BITCAST]],
  // CHECK1-SAME:                         i8 0, i64 {{(5|9)}}, i1 false){{$}}
  // CHECK1: @llvm.dbg.declare(metadata {{(i32|i64)}}* %val.debug,
  // CHECK1-SAME:              !dbg ![[DBG0:.*]]
  // CHECK1-LABEL: define {{.*}}@"$s4main1gyySSSgF"
  // CHECK1: ![[F:.*]] = distinct !DISubprogram(name: "f",
  // CHECK1: ![[BLK:.*]] = distinct !DILexicalBlock(scope: ![[F]],
  // CHECK1: ![[DBG0]] = !DILocation(line: [[@LINE+1]],
  guard let val = i else { return }
  use(val)
}

public func g(_ s : String?)
{
  // CHECK2: define {{.*}}@"$s4main1gyySSSgF"
  // CHECK2: %s.debug = alloca %TSSSg
  // CHECK2: @llvm.dbg.declare(metadata %TSSSg*
  // CHECK2: %val.debug = alloca %TSS
  // CHECK2: @llvm.dbg.declare(metadata %TSS*
  // CHECK2: %[[BITCAST:.*]] = bitcast %TSS* %val.debug to i8*{{$}}
  // CHECK2: call void @llvm.memset.{{.*}}(i8* align {{(4|8)}} %[[BITCAST]], i8 0
  // CHECK2: ![[G:.*]] = distinct !DISubprogram(name: "g"
  guard let val = s else { return }
  use(val)
}

public func h(_ s : String?)
{
  // CHECK3: define {{.*}}@"$s4main1hyySSSgF"
  // CHECK3: %s.debug = alloca %TSSSg
  // CHECK3: @llvm.dbg.declare(metadata %TSSSg*
  // CHECK3: %s.debug1 = alloca %TSS
  // CHECK3: @llvm.dbg.declare(metadata %TSS*
  // CHECK3: %[[BITCAST:.*]] = bitcast %TSS* %s.debug1 to i8*{{$}}
  // CHECK3: call void @llvm.memset.{{.*}}(i8* align {{(4|8)}} %[[BITCAST]], i8 0
  // CHECK3: ![[G:.*]] = distinct !DISubprogram(name: "h"
  guard let s = s else { return }
  use(s)
}

enum MyError : Error {
  case bad
}

enum Stuff {
  case array([Stuff])
  case any(Any)
  case nothing

  func toArray() throws -> [Stuff] {
    guard case .array(let array) = self else { throw MyError.bad }
    return array
  }
}

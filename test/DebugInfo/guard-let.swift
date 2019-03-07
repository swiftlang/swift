// RUN: %target-swift-frontend %s -c -emit-ir -g -o - | \
// RUN:   %FileCheck %s --check-prefix=CHECK1
// RUN: %target-swift-frontend %s -c -emit-ir -g -o - | \
// RUN:   %FileCheck %s --check-prefix=CHECK2

// UNSUPPORTED: OS=watchos

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

// With large type optimizations the string is passed indirectly on the
// following architectures so there is no shadow copy happening. As this
// tests that we're emitting the DI correctly, we can skip running on them.
// UNSUPPORTED: CPU=i386
// UNSUPPORTED: CPU=armv7
// UNSUPPORTED: CPU=armv7s
// UNSUPPORTED: CPU=armv7k

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

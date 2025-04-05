// RUN: %target-swift-frontend %s -emit-ir -g -o -
import StdlibUnittest
protocol P1 {}
protocol P2 {}
protocol P3 {}
protocol P4 {}
protocol P5 {}

struct I: P1, P2, P4 {}
struct I2: P3 {}
struct I3: P5 {}

struct S<T, U: P3> where T: P1, T: P2 {
    let t: T
    let u: U

    func foo() {
    }
//CHECK: ![[foo:[0-9]+]] = distinct !DISubprogram(name: "foo", linkageName: "$s13witness_table1SV3fooyyF",
//CHECK: !DILocalVariable(name: "$WT_T_$s13witness_table2P1_pmD", scope: ![[foo]], {{.*}}, flags: DIFlagArtificial)
//CHECK: !DILocalVariable(name: "$WT_T_$s13witness_table2P2_pmD", scope: ![[foo]], {{.*}}, flags: DIFlagArtificial)
//CHECK: !DILocalVariable(name: "$\CF\84_0_0", scope: ![[foo]], {{.*}}, flags: DIFlagArtificial)
//CHECK: !DILocalVariable(name: "$WT_U_$s13witness_table2P3_pmD", scope: ![[foo]], {{.*}}, flags: DIFlagArtificial)
//CHECK: !DILocalVariable(name: "$\CF\84_0_1", scope: ![[foo]], {{.*}}, flags: DIFlagArtificial)

    func bar<V: P4>(v: V) {
    }
//CHECK: ![[bar:[0-9]+]] = distinct !DISubprogram(name: "bar", linkageName: "$s13witness_table1SV3bar1vyqd___tAA2P4Rd__lF",
//CHECK: !DILocalVariable(name: "$\CF\84_1_0", scope: ![[bar]], {{.*}}, flags: DIFlagArtificial)
//CHECK: !DILocalVariable(name: "$WT_V_$s13witness_table2P4_pmD", scope: ![[bar]], {{.*}}, flags: DIFlagArtificial)
//CHECK: !DILocalVariable(name: "$WT_T_$s13witness_table2P1_pmD", scope: ![[bar]], {{.*}}, flags: DIFlagArtificial)
//CHECK: !DILocalVariable(name: "$WT_T_$s13witness_table2P2_pmD", scope: ![[bar]], {{.*}}, flags: DIFlagArtificial)
//CHECK: !DILocalVariable(name: "$\CF\84_0_0", scope: ![[bar]], {{.*}}, flags: DIFlagArtificial)
//CHECK: !DILocalVariable(name: "$WT_U_$s13witness_table2P3_pmD", scope: ![[bar]], {{.*}}, flags: DIFlagArtificial)
//CHECK: !DILocalVariable(name: "$\CF\84_0_1", scope: ![[bar]], {{.*}}, flags: DIFlagArtificial)
}

extension S where T: P5 {
    func baz() {
    }

//CHECK: ![[baz:[0-9]+]] = distinct !DISubprogram(name: "baz", linkageName: "$s13witness_table1SVA2A2P5RzrlE3bazyyF", 
//CHECK: !DILocalVariable(name: "$WT_T_$s13witness_table2P5_pmD", scope: ![[baz]], {{.*}}, flags: DIFlagArtificial)
//CHECK: !DILocalVariable(name: "$WT_T_$s13witness_table2P1_pmD", scope: ![[baz]], {{.*}}, flags: DIFlagArtificial)
//CHECK: !DILocalVariable(name: "$WT_T_$s13witness_table2P2_pmD", scope: ![[baz]], {{.*}}, flags: DIFlagArtificial)
//CHECK: !DILocalVariable(name: "$\CF\84_0_0", scope: ![[baz]], {{.*}}, flags: DIFlagArtificial)
//CHECK: !DILocalVariable(name: "$WT_U_$s13witness_table2P3_pmD", scope: ![[baz]], {{.*}}, flags: DIFlagArtificial)
//CHECK: !DILocalVariable(name: "$\CF\84_0_1", scope: ![[baz]], {{.*}}, flags: DIFlagArtificial)
}

S(t: I(), u: I2())

func freeFunc<T1: P1, T2>(t1: T1, t2: T2) where T2: P3,  T2: P4 {
}
//CHECK: ![[freeFunc:[0-9]+]] = distinct !DISubprogram(name: "freeFunc", linkageName: "$s13witness_table8freeFunc2t12t2yx_q_tAA2P1RzAA2P3R_AA2P4R_r0_lF", 
//CHECK: !DILocalVariable(name: "$\CF\84_0_0", scope: ![[freeFunc]], {{.*}}, flags: DIFlagArtificial)
//CHECK: !DILocalVariable(name: "$\CF\84_0_1", scope: ![[freeFunc]], {{.*}}, flags: DIFlagArtificial)
//CHECK: !DILocalVariable(name: "$WT_T1_$s13witness_table2P1_pmD", scope: ![[freeFunc]], {{.*}}, flags: DIFlagArtificial)
//CHECK: !DILocalVariable(name: "$WT_T2_$s13witness_table2P3_pmD", scope: ![[freeFunc]], {{.*}}, flags: DIFlagArtificial)
//CHECK: !DILocalVariable(name: "$WT_T2_$s13witness_table2P4_pmD", scope: ![[freeFunc]], {{.*}}, flags: DIFlagArtificial)

protocol A {
    associatedtype Element
}

func withAssociatedType<T: A>(_: T) where T.Element: A {
}

//CHECK: ![[withAssociatedType:[0-9]+]] = distinct !DISubprogram(name: "withAssociatedType", linkageName: "$s13witness_table18withAssociatedTypeyyxAA1ARzAaC7ElementRpzlF"
//CHECK: !DILocalVariable(name: "$\CF\84_0_0", scope: ![[withAssociatedType]], {{.*}}, flags: DIFlagArtificial)
//CHECK: !DILocalVariable(name: "$WT_T_$s13witness_table1A_pmD", scope: ![[withAssociatedType]], {{.*}}, flags: DIFlagArtificial)
//CHECK: !DILocalVariable(name: "$WT_T.Element_$s13witness_table1A_pmD", scope: ![[withAssociatedType]], {{.*}}, flags: DIFlagArtificial)


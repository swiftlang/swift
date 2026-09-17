// RUN: %target-swift-frontend -emit-ir -g -module-name main %s | %FileCheck %s
// RUN: %target-swift-frontend -emit-ir -g -O -module-name main %s

// A protocol composition typealias that is itself built from a nested
// composition typealias canonicalizes to the same type -- and therefore the
// same debug-info UID -- as the nested composition. `PQP` below has sugared
// members [PQ, P] but canonicalizes to `P & Q`, exactly like `PQ`. Emitting
// debug info from the sugared members produced two different DICompositeTypes
// sharing one UID.

protocol P {}
protocol Q {}

typealias PQ = P & Q
typealias PQP = PQ & P

struct S: PQP {}

let g: PQP = S()

// The composition is emitted once, by its canonical mangled name, with the
// underlying protocols P and Q as direct members (no nested PQ composition).
// CHECK-DAG: ![[COMP:[0-9]+]] = !DICompositeType(tag: DW_TAG_structure_type, name: "$s4main1P_AA1QpD", {{.*}}elements: ![[ELTS:[0-9]+]], {{.*}}identifier: "$s4main1P_AA1QpD")
// CHECK-DAG: ![[ELTS]] = !{![[I1:[0-9]+]], ![[I2:[0-9]+]]}
// CHECK-DAG: ![[I1]] = !DIDerivedType(tag: DW_TAG_inheritance, scope: ![[COMP]], baseType: ![[PTY:[0-9]+]]
// CHECK-DAG: ![[PTY]] = !DICompositeType(tag: DW_TAG_structure_type, name: "P", {{.*}}identifier: "$s4main1P_pD")
// CHECK-DAG: ![[I2]] = !DIDerivedType(tag: DW_TAG_inheritance, scope: ![[COMP]], baseType: ![[QTY:[0-9]+]]
// CHECK-DAG: ![[QTY]] = !DICompositeType(tag: DW_TAG_structure_type, name: "Q", {{.*}}identifier: "$s4main1Q_pD")

// The redundant member appears first in the composition (member order does not
// matter -- the redundant member is the whole composition either way).
protocol R1 {}
protocol R2 {}
typealias R12 = R1 & R2
typealias RR = R1 & R12
struct SR: RR {}
func useR(_ x: RR) {}

// RR is redundant, so it is emitted as the canonical `R1 & R2` with R1 and R2
// as direct members (no nested R12 composition sharing the UID).
// CHECK-DAG: ![[RR:[0-9]+]] = !DICompositeType(tag: DW_TAG_structure_type, name: "$s4main2R1_AA2R2pD", {{.*}}elements: ![[RRE:[0-9]+]], {{.*}}identifier: "$s4main2R1_AA2R2pD")
// CHECK-DAG: ![[RRE]] = !{![[RRI1:[0-9]+]], ![[RRI2:[0-9]+]]}
// CHECK-DAG: ![[RRI1]] = !DIDerivedType(tag: DW_TAG_inheritance, scope: ![[RR]], baseType: ![[RRB1:[0-9]+]]
// CHECK-DAG: ![[RRB1]] = !DICompositeType(tag: DW_TAG_structure_type, name: "R1", {{.*}}identifier: "$s4main2R1_pD")
// CHECK-DAG: ![[RRI2]] = !DIDerivedType(tag: DW_TAG_inheritance, scope: ![[RR]], baseType: ![[RRB2:[0-9]+]]
// CHECK-DAG: ![[RRB2]] = !DICompositeType(tag: DW_TAG_structure_type, name: "R2", {{.*}}identifier: "$s4main2R2_pD")

// Three protocols, redundancy that only becomes visible two levels deep, with a
// global for every alias so all of DA/DB/DC are emitted in the same module.
protocol D1 {}
protocol D2 {}
protocol D3 {}
typealias DA = D1 & D2
typealias DB = DA & D3
typealias DC = DB & DA
struct SD: DC {}
let gc: DC = SD()
let gb: DB = SD()
let ga: DA = SD()

// DC and DB both canonicalize to `D1 & D2 & D3`, so they share a UID and are
// emitted as a single composite with the three protocols flattened as members.
// DA is the distinct `D1 & D2` composite.
// CHECK-DAG: ![[DC:[0-9]+]] = !DICompositeType(tag: DW_TAG_structure_type, name: "$s4main2D1_AA2D2AA2D3pD", {{.*}}elements: ![[DCE:[0-9]+]], {{.*}}identifier: "$s4main2D1_AA2D2AA2D3pD")
// CHECK-DAG: ![[DCE]] = !{![[DCI1:[0-9]+]], ![[DCI2:[0-9]+]], ![[DCI3:[0-9]+]]}
// CHECK-DAG: ![[DCI1]] = !DIDerivedType(tag: DW_TAG_inheritance, scope: ![[DC]], baseType: ![[DCB1:[0-9]+]]
// CHECK-DAG: ![[DCB1]] = !DICompositeType(tag: DW_TAG_structure_type, name: "D1", {{.*}}identifier: "$s4main2D1_pD")
// CHECK-DAG: ![[DCI2]] = !DIDerivedType(tag: DW_TAG_inheritance, scope: ![[DC]], baseType: ![[DCB2:[0-9]+]]
// CHECK-DAG: ![[DCB2]] = !DICompositeType(tag: DW_TAG_structure_type, name: "D2", {{.*}}identifier: "$s4main2D2_pD")
// CHECK-DAG: ![[DCI3]] = !DIDerivedType(tag: DW_TAG_inheritance, scope: ![[DC]], baseType: ![[DCB3:[0-9]+]]
// CHECK-DAG: ![[DCB3]] = !DICompositeType(tag: DW_TAG_structure_type, name: "D3", {{.*}}identifier: "$s4main2D3_pD")
// CHECK-DAG: ![[DA:[0-9]+]] = !DICompositeType(tag: DW_TAG_structure_type, name: "$s4main2D1_AA2D2pD", {{.*}}elements: ![[DAE:[0-9]+]], {{.*}}identifier: "$s4main2D1_AA2D2pD")
// CHECK-DAG: ![[DAE]] = !{{{.*}}}

// A composition that is *not* redundant (its two members share only a partial
// protocol set) must keep its sugared members and must not be redirected.
protocol E1 {}
protocol E2 {}
protocol E3 {}
typealias E12 = E1 & E2
typealias E23 = E2 & E3
typealias E123 = E12 & E23
func useE(_ x: E123) {}

// E123 is NOT redundant, so it keeps its sugared members: the composite has the
// nested E12 and E23 compositions (as typedefs) as members rather than being
// flattened to E1/E2/E3. This guards against over-eager canonicalization.
// CHECK-DAG: ![[E123:[0-9]+]] = !DICompositeType(tag: DW_TAG_structure_type, name: "$s4main2E1_AA2E2AA2E3pD", {{.*}}elements: ![[E123E:[0-9]+]], {{.*}}identifier: "$s4main2E1_AA2E2AA2E3pD")
// CHECK-DAG: ![[E123E]] = !{![[E123I1:[0-9]+]], ![[E123I2:[0-9]+]]}
// CHECK-DAG: ![[E123I1]] = !DIDerivedType(tag: DW_TAG_inheritance, scope: ![[E123]], baseType: ![[E12T:[0-9]+]]
// CHECK-DAG: ![[E12T]] = !DIDerivedType(tag: DW_TAG_typedef, name: "$s4main3E12aD"
// CHECK-DAG: ![[E123I2]] = !DIDerivedType(tag: DW_TAG_inheritance, scope: ![[E123]], baseType: ![[E23T:[0-9]+]]
// CHECK-DAG: ![[E23T]] = !DIDerivedType(tag: DW_TAG_typedef, name: "$s4main3E23aD"

// Redundant `AnyObject` class constraint, with globals of both aliases.
protocol F1 {}
typealias FA = AnyObject & F1
typealias FB = FA & AnyObject
final class SF: F1 {}
let gf: FB = SF()
let gfa: FA = SF()

// FA and FB both canonicalize to `F1 & AnyObject`; the redundant AnyObject is
// folded into a single composite with F1 as its member.
// CHECK-DAG: ![[FA:[0-9]+]] = !DICompositeType(tag: DW_TAG_structure_type, name: "$s4main2F1_XlD", {{.*}}elements: ![[FAE:[0-9]+]], {{.*}}identifier: "$s4main2F1_XlD")
// CHECK-DAG: ![[FAE]] = !{![[FAI1:[0-9]+]]}
// CHECK-DAG: ![[FAI1]] = !DIDerivedType(tag: DW_TAG_inheritance, scope: ![[FA]], baseType: ![[FAB:[0-9]+]]
// CHECK-DAG: ![[FAB]] = !DICompositeType(tag: DW_TAG_structure_type, name: "F1", {{.*}}identifier: "$s4main2F1_pD")

// Redundant superclass constraint.
class GBase {}
protocol G1 {}
typealias GA = GBase & G1
typealias GB = GA & GBase
func useG(_ x: GB) {}

// GA and GB both canonicalize to `GBase & G1`; the redundant superclass is
// folded into a single composite whose members are the class and the protocol.
// CHECK-DAG: ![[GA:[0-9]+]] = !DICompositeType(tag: DW_TAG_structure_type, name: "$s4main2G1_AA5GBaseCXcD", {{.*}}elements: ![[GAE:[0-9]+]], {{.*}}identifier: "$s4main2G1_AA5GBaseCXcD")
// CHECK-DAG: ![[GAE]] = !{![[GAI1:[0-9]+]], ![[GAI2:[0-9]+]]}
// CHECK-DAG: ![[GAI1]] = !DIDerivedType(tag: DW_TAG_inheritance, scope: ![[GA]], baseType: ![[GAB1:[0-9]+]]
// CHECK-DAG: ![[GAB1]] = !DICompositeType(tag: DW_TAG_structure_type, name: "GBase", {{.*}}identifier: "$s4main5GBaseCD")
// CHECK-DAG: ![[GAI2]] = !DIDerivedType(tag: DW_TAG_inheritance, scope: ![[GA]], baseType: ![[GAB2:[0-9]+]]
// CHECK-DAG: ![[GAB2]] = !DICompositeType(tag: DW_TAG_structure_type, name: "G1", {{.*}}identifier: "$s4main2G1_pD")

// The redundant composition reused as an array element and as a stored field.
let harr: [PQP] = []
struct IBox { let x: PQP }

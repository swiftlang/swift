// RUN: %target-swift-frontend -enable-experimental-feature SymbolLinkageMarkers -primary-file %s -emit-sil -parse-as-library | %FileCheck %s --check-prefix=SIL
// RUN: %target-swift-frontend -enable-experimental-feature SymbolLinkageMarkers -primary-file %s -emit-ir  -parse-as-library | %FileCheck %s --check-prefix=IR

// REQUIRES: swift_in_compiler
// REQUIRES: swift_feature_SymbolLinkageMarkers

@_section("__DATA,__mysection") var g0: Int = 1
@_section("__DATA,__mysection") var g1: (Int, Int) = (42, 43)
@_section("__DATA,__mysection") var g2: Bool = true
@_section("__DATA,__mysection") public var g3: Bool = true
@_section("__DATA,__mysection") var g4: UnsafeMutablePointer<Int>? = nil
@_section("__DATA,__mysection") var g5: UnsafeMutablePointer<Int>? = UnsafeMutablePointer(bitPattern: 0x42424242)
@_section("__TEXT,__mysection") @_used func foo() {}

struct MyStruct {
	@_section("__DATA,__mysection") static var static0: Int = 1
	@_section("__TEXT,__mysection") @_used func foo() {}
}

@_section("__TEXT,__mysection")
var functionptr = testit
func testit(_ e: consuming Any) -> Any { e }

// SIL: @_section("__DATA,__mysection") @_hasStorage @_hasInitialValue var g0: Int { get set }
// SIL: @_section("__DATA,__mysection") @_hasStorage @_hasInitialValue var g1: (Int, Int) { get set }
// SIL: @_section("__DATA,__mysection") @_hasStorage @_hasInitialValue var g2: Bool { get set }
// SIL: @_section("__DATA,__mysection") @_hasStorage @_hasInitialValue public var g3: Bool { get set }
// SIL: @_section("__DATA,__mysection") @_hasStorage @_hasInitialValue var g4: UnsafeMutablePointer<Int>? { get set }
// SIL: @_section("__DATA,__mysection") @_hasStorage @_hasInitialValue var g5: UnsafeMutablePointer<Int>? { get set }
// SIL: @_section("__TEXT,__mysection") @_used func foo()
// SIL: struct MyStruct {
// SIL:   @_section("__DATA,__mysection") @_hasStorage @_hasInitialValue static var static0: Int { get set }
// SIL:   @_section("__TEXT,__mysection") @_used func foo()

// SIL: sil private [global_init_once_fn] @$s7section2g0_WZ : $@convention(c)
// SIL: sil hidden [global_init] @$s7section2g0Sivau : $@convention(thin)
// SIL: sil private [global_init_once_fn] @$s7section2g1_WZ : $@convention(c)
// SIL: sil hidden [global_init] @$s7section2g1Si_Sitvau : $@convention(thin)
// SIL: sil private [global_init_once_fn] @$s7section2g2_WZ : $@convention(c)
// SIL: sil hidden [global_init] @$s7section2g2Sbvau : $@convention(thin)
// SIL: sil private [global_init_once_fn] @$s7section2g3_WZ : $@convention(c)
// SIL: sil [global_init] @$s7section2g3Sbvau : $@convention(thin)
// SIL: sil private [global_init_once_fn] @$s7section2g4_WZ : $@convention(c)
// SIL: sil hidden [global_init] @$s7section2g4SpySiGSgvau : $@convention(thin)
// SIL: sil private [global_init_once_fn] @$s7section2g5_WZ : $@convention(c)
// SIL: sil hidden [global_init] @$s7section2g5SpySiGSgvau : $@convention(thin)
// SIL: sil hidden [used] [section "__TEXT,__mysection"] @$s7section3fooyyF : $@convention(thin)
// SIL: sil private [global_init_once_fn] @$s7section8MyStructV7static0_WZ : $@convention(c)
// SIL: sil hidden [global_init] @$s7section8MyStructV7static0Sivau : $@convention(thin)
// SIL: sil hidden [used] [section "__TEXT,__mysection"] @$s7section8MyStructV3fooyyF : $@convention(method)
// SIL: sil hidden @$s7section6testityypypnF : $@convention(thin) (@in Any) -> @out Any {

// IR:  @"$s7section2g0Sivp" = hidden global %TSi <{ {{(i64|i32)}} 1 }>, section "__DATA,__mysection"
// IR:  @"$s7section2g1Si_Sitvp" = hidden global <{ %TSi, %TSi }> <{ %TSi <{ {{(i64|i32)}} 42 }>, %TSi <{ {{(i64|i32)}} 43 }> }>, section "__DATA,__mysection"
// IR:  @"$s7section2g2Sbvp" = hidden global %TSb <{ i1 true }>, section "__DATA,__mysection"
// IR:  @"$s7section2g3Sbvp" = {{.*}}global %TSb <{ i1 true }>, section "__DATA,__mysection"
// IR:  @"$s7section2g4SpySiGSgvp" = hidden global {{i64|i32}} 0, section "__DATA,__mysection"
// IR:  @"$s7section2g5SpySiGSgvp" = hidden global {{i64|i32}} 1111638594, section "__DATA,__mysection"
// IR:  @"$s7section8MyStructV7static0SivpZ" = hidden global %TSi <{ {{(i64|i32)}} 1 }>, section "__DATA,__mysection"
// IR:  define {{.*}}@"$s7section3fooyyF"(){{.*}} section "__TEXT,__mysection"
// IR:  define {{.*}}@"$s7section8MyStructV3fooyyF"() #0 section "__TEXT,__mysection"

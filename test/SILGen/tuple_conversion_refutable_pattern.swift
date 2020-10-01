// RUN: %target-swift-emit-silgen %s | %FileCheck %s

// CHECK-LABEL: sil hidden [ossa] @$s34tuple_conversion_refutable_pattern2f13argySi1a_SiSg1bt_tF : $@convention(thin) (Int, Optional<Int>) -> () {
func f1(arg: (a: Int, b: Int?)) {
  guard case let (x, y?) = arg else { return }
}

// CHECK-LABEL: sil hidden [ossa] @$s34tuple_conversion_refutable_pattern2f13argyyXl1a_yXlSg1bt_tF : $@convention(thin) (@guaranteed AnyObject, @guaranteed Optional<AnyObject>) -> () {
func f1(arg: (a: AnyObject, b: AnyObject?)) {
  guard case let (x, y?) = arg else { return }
}

// CHECK-LABEL: sil hidden [ossa] @$s34tuple_conversion_refutable_pattern2f13argyyp1a_ypSg1bt_tF : $@convention(thin) (@in_guaranteed Any, @in_guaranteed Optional<Any>) -> () {
func f1(arg: (a: Any, b: Any?)) {
  guard case let (x, y?) = arg else { return }
}

// CHECK-LABEL: sil hidden [ossa] @$s34tuple_conversion_refutable_pattern2f23argySi1a_Si1bt_tF : $@convention(thin) (Int, Int) -> () {
func f2(arg: (a: Int, b: Int)) {
  guard case let (x, 4) = arg else { return }
}

// CHECK-LABEL: sil hidden [ossa] @$s34tuple_conversion_refutable_pattern2f23argySi1a_SS1bt_tF : $@convention(thin) (Int, @guaranteed String) -> () {
func f2(arg: (a: Int, b: String)) {
  guard case let (x, "") = arg else { return }
}

// CHECK-LABEL: sil hidden [ossa] @$s34tuple_conversion_refutable_pattern2f33argySi1a_Si1bt_tF : $@convention(thin) (Int, Int) -> () {
func f3(arg: (a: Int, b: Int)) {
  guard case let (x, is String) = arg else { return }
}

// CHECK-LABEL: sil hidden [ossa] @$s34tuple_conversion_refutable_pattern2f33argySi1a_yXl1bt_tF : $@convention(thin) (Int, @guaranteed AnyObject) -> () {
func f3(arg: (a: Int, b: AnyObject)) {
  guard case let (x, is String) = arg else { return }
}

// CHECK-LABEL: sil hidden [ossa] @$s34tuple_conversion_refutable_pattern2f33argySi1a_yp1bt_tF : $@convention(thin) (Int, @in_guaranteed Any) -> () {
func f3(arg: (a: Int, b: Any)) {
  guard case let (x, is String) = arg else { return }
}

// CHECK-LABEL: sil hidden [ossa] @$s34tuple_conversion_refutable_pattern2f43argySi1a_Sb1bt_tF : $@convention(thin) (Int, Bool) -> () {
func f4(arg: (a: Int, b: Bool)) {
  guard case let (x, false) = arg else { return }
}

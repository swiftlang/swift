// RUN: %target-swift-frontend -Xllvm -sil-print-types -emit-sil -verify -Xllvm -sil-disable-pass=simplification %s | %FileCheck %s

struct Point {
  let x: Int
  var y: Int
}

struct Rectangle {
  var topLeft, bottomRight: Point
}

@dynamicMemberLookup
struct Lens<T> {
  var obj: T

  init(_ obj: T) {
    self.obj = obj
  }

  subscript<U>(dynamicMember member: KeyPath<T, U>) -> Lens<U> {
    get { return Lens<U>(obj[keyPath: member]) }
  }

  subscript<U>(dynamicMember member: WritableKeyPath<T, U>) -> Lens<U> {
    get { return Lens<U>(obj[keyPath: member]) }
    set { obj[keyPath: member] = newValue.obj }
  }

  // Used to make sure that keypath and string based lookup are
  // property disambiguated.
  subscript(dynamicMember member: String) -> Lens<Int> {
    return Lens<Int>(42)
  }
}

var topLeft = Point(x: 0, y: 0)
var bottomRight = Point(x: 10, y: 10)

var lens = Lens(Rectangle(topLeft: topLeft,
                          bottomRight: bottomRight))

// CHECK: function_ref @$s29keypath_dynamic_member_lookup4LensV0B6MemberACyqd__Gs15WritableKeyPathCyxqd__G_tcluig
// CHECK-NEXT: apply %{{[0-9]+}}<Rectangle, Point>({{.*}})
// CHECK: function_ref @$s29keypath_dynamic_member_lookup4LensV0B6MemberACyqd__Gs7KeyPathCyxqd__G_tcluig
// CHECK-NEXT: apply %{{[0-9]+}}<Point, Int>({{.*}})
_ = lens.topLeft.x

// CHECK: function_ref @$s29keypath_dynamic_member_lookup4LensV0B6MemberACyqd__Gs15WritableKeyPathCyxqd__G_tcluig
// CHECK-NEXT: apply %{{[0-9]+}}<Rectangle, Point>({{.*}})
// CHECK: function_ref @$s29keypath_dynamic_member_lookup4LensV0B6MemberACyqd__Gs15WritableKeyPathCyxqd__G_tcluig
// CHECK-NEXT: apply %{{[0-9]+}}<Point, Int>({{.*}})
_ = lens.topLeft.y

lens.topLeft = Lens(Point(x: 1, y: 2)) // Ok
lens.bottomRight.y = Lens(12)          // Ok

@dynamicMemberLookup
class A<T> {
  var value: T

  init(_ v: T) {
    self.value = v
  }

  subscript<U>(dynamicMember member: KeyPath<T, U>) -> U {
    get { return value[keyPath: member] }
  }
}

// Let's make sure that keypath dynamic member lookup
// works with inheritance

class B<T> : A<T> {}

func bar(_ b: B<Point>) {
  let _: Int = b.x
  let _ = b.y
}

struct Point3D {
  var x, y, z: Int
}

// Make sure that explicitly declared members take precedence
class C<T> : A<T> {
  var x: Float = 42
}

func baz(_ c: C<Point3D>) {
  // CHECK: ref_element_addr {{.*}} : $C<Point3D>, #C.x
  let _ = c.x
  // CHECK: [[Y:%.*]] = keypath $KeyPath<Point3D, Int>, (root $Point3D; stored_property #Point3D.z : $Int)
  // CHECK: [[KEYPATH:%.*]] = function_ref @$s29keypath_dynamic_member_lookup1AC0B6Memberqd__s7KeyPathCyxqd__G_tcluig
  // CHECK-NEXT: apply [[KEYPATH]]<Point3D, Int>({{.*}}, [[Y]], {{.*}})
  let _ = c.z
}

@dynamicMemberLookup
struct SubscriptLens<T> {
  var value: T

  subscript(foo: String) -> Int {
    get { return 42 }
  }

  subscript<U>(dynamicMember member: KeyPath<T, U>) -> U {
    get { return value[keyPath: member] }
  }

  subscript<U>(dynamicMember member: WritableKeyPath<T, U>) -> U {
    get { return value[keyPath: member] }
    set { value[keyPath: member] = newValue }
  }
}

func keypath_with_subscripts(_ arr: SubscriptLens<[Int]>,
                             _ dict: inout SubscriptLens<[String: Int]>) {
  // CHECK: keypath $WritableKeyPath<Array<Int>, ArraySlice<Int>>, (root $Array<Int>; settable_property $ArraySlice<Int>,  id @$sSays10ArraySliceVyxGSnySiGcig : {{.*}})
  _ = arr[0..<3]
  // CHECK: keypath $KeyPath<Array<Int>, Int>, (root $Array<Int>; gettable_property $Int,  id @$sSa5countSivg : {{.*}})
  for idx in 0..<arr.count {
    // CHECK: keypath $WritableKeyPath<Array<Int>, Int>, (root $Array<Int>; settable_property $Int,  id @$sSayxSicig : {{.*}})
    let _ = arr[idx]
    // CHECK: keypath $WritableKeyPath<Array<Int>, Int>, (root $Array<Int>; settable_property $Int,  id @$sSayxSicig : {{.*}})
    print(arr[idx])
  }

  // CHECK: function_ref @$s29keypath_dynamic_member_lookup13SubscriptLensVySiSScig
  _ = arr["hello"]
  // CHECK: function_ref @$s29keypath_dynamic_member_lookup13SubscriptLensVySiSScig
  _ = dict["hello"]

  if let index = dict.value.firstIndex(where: { $0.value == 42 }) {
    // CHECK: keypath $KeyPath<Dictionary<String, Int>, (key: String, value: Int)>, (root $Dictionary<String, Int>; gettable_property $(key: String, value: Int),  id @$sSDyx3key_q_5valuetSD5IndexVyxq__Gcig : {{.*}})
    let _ = dict[index]
  }
  // CHECK: keypath $WritableKeyPath<Dictionary<String, Int>, Optional<Int>>, (root $Dictionary<String, Int>; settable_property $Optional<Int>,  id @$sSDyq_Sgxcig : {{.*}})
  dict["ultimate question"] = 42
}

struct DotStruct {
  var x, y: Int
}

class DotClass {
  var x, y: Int

  init(x: Int, y: Int) {
    self.x = x
    self.y = y
  }
}

@dynamicMemberLookup
struct DotLens<T> {
  var value: T

  subscript<U>(dynamicMember member: WritableKeyPath<T, U>) -> U {
    get { return value[keyPath: member] }
    set { value[keyPath: member] = newValue }
  }

  subscript<U>(dynamicMember member: ReferenceWritableKeyPath<T, U>) -> U {
    get { return value[keyPath: member] }
    set { value[keyPath: member] = newValue }
  }
}

func dot_struct_test(_ lens: inout DotLens<DotStruct>) {
  // CHECK: keypath $WritableKeyPath<DotStruct, Int>, (root $DotStruct; stored_property #DotStruct.x : $Int)
  lens.x = 1
  // CHECK: keypath $WritableKeyPath<DotStruct, Int>, (root $DotStruct; stored_property #DotStruct.y : $Int)
  let _ = lens.y
}

func dot_class_test(_ lens: inout DotLens<DotClass>) {
  // CHECK: keypath $ReferenceWritableKeyPath<DotClass, Int>, (root $DotClass; settable_property $Int,  id #DotClass.x!getter : (DotClass) -> () -> Int, getter @$s29keypath_dynamic_member_lookup8DotClassC1xSivpACTK : {{.*}})
  lens.x = 1
  // CHECK: keypath $ReferenceWritableKeyPath<DotClass, Int>, (root $DotClass; settable_property $Int,  id #DotClass.y!getter : (DotClass) -> () -> Int, getter @$s29keypath_dynamic_member_lookup8DotClassC1ySivpACTK : {{.*}})
  let _ = lens.y
}

@dynamicMemberLookup
struct OverloadedLens<T> {
  var value: T

  subscript<U>(keyPath: KeyPath<T, U>) -> U {
    get { return value[keyPath: keyPath] }
  }

  subscript<U>(dynamicMember keyPath: KeyPath<T, U>) -> U {
    get { return value[keyPath: keyPath] }
  }
}

// Make sure if there is a subscript which accepts key path,
// existing dynamic member overloads wouldn't interfere.
func test_direct_subscript_ref(_ lens: OverloadedLens<Point>) {
  // CHECK: function_ref @$s29keypath_dynamic_member_lookup14OverloadedLensVyqd__s7KeyPathCyxqd__Gcluig
  _ = lens[\.x]
  // CHECK: function_ref @$s29keypath_dynamic_member_lookup14OverloadedLensVyqd__s7KeyPathCyxqd__Gcluig
  _ = lens[\.y]

  // CHECK: function_ref @$s29keypath_dynamic_member_lookup14OverloadedLensV0B6Memberqd__s7KeyPathCyxqd__G_tcluig
  _ = lens.x
  // CHECK: function_ref @$s29keypath_dynamic_member_lookup14OverloadedLensV0B6Memberqd__s7KeyPathCyxqd__G_tcluig
  _ = lens.y
}

func test_keypath_dynamic_lookup_inside_keypath() {
  // CHECK: keypath $KeyPath<Point, Int>, (root $Point; stored_property #Point.x : $Int)
  // CHECK-NEXT: keypath $KeyPath<Lens<Point>, Lens<Int>>, (root $Lens<Point>; gettable_property $Lens<Int>,  id @$s29keypath_dynamic_member_lookup4LensV0B6MemberACyqd__Gs7KeyPathCyxqd__G_tcluig : {{.*}})
  _ = \Lens<Point>.x
  // CHECK: keypath $WritableKeyPath<Rectangle, Point>, (root $Rectangle; stored_property #Rectangle.topLeft : $Point)
  // CHECK-NEXT: keypath $WritableKeyPath<Point, Int>, (root $Point; stored_property #Point.y : $Int)
  // CHECK-NEXT: keypath $WritableKeyPath<Lens<Rectangle>, Lens<Int>>, (root $Lens<Rectangle>; settable_property $Lens<Point>,  id @$s29keypath_dynamic_member_lookup4LensV0B6MemberACyqd__Gs15WritableKeyPathCyxqd__G_tcluig : {{.*}})
  _ = \Lens<Rectangle>.topLeft.y
  // CHECK: keypath $KeyPath<Array<Int>, Int>, (root $Array<Int>; gettable_property $Int,  id @$sSa5countSivg : {{.*}})
  // CHECK-NEXT: keypath $KeyPath<Lens<Array<Int>>, Lens<Int>>, (root $Lens<Array<Int>>; gettable_property $Lens<Int>,  id @$s29keypath_dynamic_member_lookup4LensV0B6MemberACyqd__Gs7KeyPathCyxqd__G_tcluig : {{.*}})
  _ = \Lens<[Int]>.count
  // CHECK: keypath $WritableKeyPath<Array<Int>, Int>, (root $Array<Int>; settable_property $Int,  id @$sSayxSicig : {{.*}})
  // CHECK-NEXT: keypath $WritableKeyPath<Lens<Array<Int>>, Lens<Int>>, (root $Lens<Array<Int>>; settable_property $Lens<Int>,  id @$s29keypath_dynamic_member_lookup4LensV0B6MemberACyqd__Gs15WritableKeyPathCyxqd__G_tcluig : {{.*}})
  _ = \Lens<[Int]>.[0]
  // CHECK: keypath $WritableKeyPath<Array<Array<Int>>, Array<Int>>, (root $Array<Array<Int>>; settable_property $Array<Int>,  id @$sSayxSicig : {{.*}})
  // CHECK-NEXT: keypath $KeyPath<Array<Int>, Int>, (root $Array<Int>; gettable_property $Int,  id @$sSa5countSivg : {{.*}})
  // CHECK-NEXT: keypath $KeyPath<Lens<Array<Array<Int>>>, Lens<Int>>, (root $Lens<Array<Array<Int>>>; settable_property $Lens<Array<Int>>,  id @$s29keypath_dynamic_member_lookup4LensV0B6MemberACyqd__Gs15WritableKeyPathCyxqd__G_tcluig : {{.*}})
  _ = \Lens<[[Int]]>.[0].count
}

func test_recursive_dynamic_lookup(_ pointLens: Lens<Lens<Point>>, _ arrLens: Lens<Lens<[Point]>>) {
  // CHECK: keypath $KeyPath<Point, Int>, (root $Point; stored_property #Point.x : $Int)
  // CHECK-NEXT: keypath $KeyPath<Lens<Point>, Lens<Int>>, (root $Lens<Point>; gettable_property $Lens<Int>,  id @$s29keypath_dynamic_member_lookup4LensV0B6MemberACyqd__Gs7KeyPathCyxqd__G_tcluig : {{.*}})
  // CHECK: function_ref @$s29keypath_dynamic_member_lookup4LensV0B6MemberACyqd__Gs7KeyPathCyxqd__G_tcluig
  _ = pointLens.x
  // CHECK: keypath $KeyPath<Point, Int>, (root $Point; stored_property #Point.x : $Int)
  // CHECK: function_ref @$s29keypath_dynamic_member_lookup4LensV0B6MemberACyqd__Gs7KeyPathCyxqd__G_tcluig
  _ = pointLens.obj.x
  // CHECK: [[FIRST_OBJ:%.*]] = struct_extract {{.*}} : $Lens<Lens<Point>>, #Lens.obj
  // CHECK-NEXT: [[SECOND_OBJ:%.*]] = struct_extract [[FIRST_OBJ]] : $Lens<Point>, #Lens.obj
  // CHECK-NEXT: struct_extract [[SECOND_OBJ]] : $Point, #Point.y
  _ = pointLens.obj.obj.y
  // CHECK: keypath $KeyPath<Point, Int>, (root $Point; stored_property #Point.x : $Int)
  // CHECK-NEXT: keypath $KeyPath<Lens<Point>, Lens<Int>>, (root $Lens<Point>; gettable_property $Lens<Int>,  id @$s29keypath_dynamic_member_lookup4LensV0B6MemberACyqd__Gs7KeyPathCyxqd__G_tcluig : {{.*}})
  // CHECK-NEXT: keypath $KeyPath<Lens<Lens<Point>>, Lens<Lens<Int>>>, (root $Lens<Lens<Point>>; gettable_property $Lens<Lens<Int>>,  id @$s29keypath_dynamic_member_lookup4LensV0B6MemberACyqd__Gs7KeyPathCyxqd__G_tcluig : {{.*}})
  _ = \Lens<Lens<Point>>.x
  // CHECK: keypath $WritableKeyPath<Rectangle, Point>, (root $Rectangle; stored_property #Rectangle.topLeft : $Point)
  // CHECK-NEXT: keypath $WritableKeyPath<Lens<Rectangle>, Lens<Point>>, (root $Lens<Rectangle>; settable_property $Lens<Point>,  id @$s29keypath_dynamic_member_lookup4LensV0B6MemberACyqd__Gs15WritableKeyPathCyxqd__G_tcluig : {{.*}})
  // CHECK-NEXT: keypath $KeyPath<Point, Int>, (root $Point; stored_property #Point.x : $Int)
  // CHECK-NEXT: keypath $KeyPath<Lens<Point>, Lens<Int>>, (root $Lens<Point>; gettable_property $Lens<Int>,  id @$s29keypath_dynamic_member_lookup4LensV0B6MemberACyqd__Gs7KeyPathCyxqd__G_tcluig : {{.*}})
  // CHECK-NEXT: keypath $KeyPath<Lens<Lens<Rectangle>>, Lens<Lens<Int>>>, (root $Lens<Lens<Rectangle>>; settable_property $Lens<Lens<Point>>,  id @$s29keypath_dynamic_member_lookup4LensV0B6MemberACyqd__Gs15WritableKeyPathCyxqd__G_tcluig : {{.*}})
  _ = \Lens<Lens<Rectangle>>.topLeft.x

  // CHECK: keypath $WritableKeyPath<Array<Point>, Point>, (root $Array<Point>; settable_property $Point, id @$sSayxSicig
  // CHECK-NEXT: keypath $WritableKeyPath<Lens<Array<Point>>, Lens<Point>>, (root $Lens<Array<Point>>; settable_property $Lens<Point>, id @$s29keypath_dynamic_member_lookup4LensV0B6MemberACyqd__Gs15WritableKeyPathCyxqd__G_tcluig
  // CHECK: function_ref @$s29keypath_dynamic_member_lookup4LensV0B6MemberACyqd__Gs15WritableKeyPathCyxqd__G_tcluig
  // CHECK: keypath $KeyPath<Point, Int>, (root $Point; stored_property #Point.x : $Int)
  // CHECK-NEXT: keypath $KeyPath<Lens<Point>, Lens<Int>>, (root $Lens<Point>; gettable_property $Lens<Int>, id @$s29keypath_dynamic_member_lookup4LensV0B6MemberACyqd__Gs7KeyPathCyxqd__G_tcluig
  // CHECK: function_ref @$s29keypath_dynamic_member_lookup4LensV0B6MemberACyqd__Gs7KeyPathCyxqd__G_tcluig
  _ = arrLens[0].x // arrLens[dynamicMember: \.[dynamicMember: \.[0]]][dynamicMember: \.[dynamicMember: \.x]]

  _ = \Lens<Lens<[Point]>>.[0].x // \Lens<Lens<[Point]>>.[dynamicMember: \.[dynamicMember: \.[0]]].[dynamicMember: \.[dynamicMember: \.x]]
  // CHECK: keypath $WritableKeyPath<Array<Point>, Point>, (root $Array<Point>; settable_property $Point, id @$sSayxSicig
  // CHECK-NEXT: keypath $WritableKeyPath<Lens<Array<Point>>, Lens<Point>>, (root $Lens<Array<Point>>; settable_property $Lens<Point>, id @$s29keypath_dynamic_member_lookup4LensV0B6MemberACyqd__Gs15WritableKeyPathCyxqd__G_tcluig
  // CHECK-NEXT: keypath $KeyPath<Point, Int>, (root $Point; stored_property #Point.x : $Int)
  // CHECK-NEXT: keypath $KeyPath<Lens<Point>, Lens<Int>>, (root $Lens<Point>; gettable_property $Lens<Int>, id @$s29keypath_dynamic_member_lookup4LensV0B6MemberACyqd__Gs7KeyPathCyxqd__G_tcluig
  // CHECK-NEXT: keypath $KeyPath<Lens<Lens<Array<Point>>>, Lens<Lens<Int>>>, (root $Lens<Lens<Array<Point>>>; settable_property $Lens<Lens<Point>>, id @$s29keypath_dynamic_member_lookup4LensV0B6MemberACyqd__Gs15WritableKeyPathCyxqd__G_tcluig
}

@dynamicMemberLookup
struct RefWritableBox<T> {
  var obj: T

  init(_ obj: T) {
    self.obj = obj
  }

  subscript<U>(dynamicMember member: KeyPath<T, U>) -> U {
    get { return obj[keyPath: member] }
  }

  subscript<U>(dynamicMember member: ReferenceWritableKeyPath<T, U>) -> U {
    get { return obj[keyPath: member] }
    set { obj[keyPath: member] = newValue }
  }
}

func prefer_readonly_keypath_over_reference_writable() {
  class C {
    let foo: Int

    init(_ foo: Int) {
      self.foo = foo
    }
  }

  var box = RefWritableBox(C(42))
  // expected-warning@-1 {{variable 'box' was never mutated; consider changing to 'let' constant}}

  // CHECK: function_ref RefWritableBox.subscript.getter
  // CHECK-NEXT: function_ref @$s29keypath_dynamic_member_lookup14RefWritableBoxV0B6Memberqd__s7KeyPathCyxqd__G_tcluig
  _ = box.foo
}


// rdar://problem/52779809 - conditional conformance shadows names of members reachable through dynamic lookup

protocol P {
  var foo: Int { get }
}

@dynamicMemberLookup struct Ref<T> {
  var value: T

  subscript<U>(dynamicMember member: KeyPath<T, U>) -> U {
    get { return value[keyPath: member] }
  }
}

extension P {
  var foo: Int { return 42 }
}

struct S {
  var foo: Int { return 0 }
  var baz: Int { return 1 }
}

struct Q {
  var bar: Int { return 1 }
}

extension Ref : P where T == Q {
  var baz: String { return "hello" }
}

func rdar52779809(_ ref1: Ref<S>, _ ref2: Ref<Q>) {
  // CHECK: function_ref @$s29keypath_dynamic_member_lookup3RefV0B6Memberqd__s7KeyPathCyxqd__G_tcluig
  _ = ref1.foo // Ok
  // CHECK: function_ref @$s29keypath_dynamic_member_lookup3RefV0B6Memberqd__s7KeyPathCyxqd__G_tcluig
  _ = ref1.baz // Ok
  // CHECK: function_ref @$s29keypath_dynamic_member_lookup1PPAAE3fooSivg
  _ = ref2.foo // Ok
  // CHECK: function_ref @$s29keypath_dynamic_member_lookup3RefV0B6Memberqd__s7KeyPathCyxqd__G_tcluig
  _ = ref2.bar // Ok
}

func make_sure_delayed_keypath_dynamic_member_works() {
  @propertyWrapper @dynamicMemberLookup
  struct Wrapper<T> {
    var storage: T? = nil

    var wrappedValue: T {
      get { storage! }
    }

    var projectedValue: Wrapper<T> { self }

    init() { }

    init(wrappedValue: T) {
      storage = wrappedValue
    }

    subscript<Property>(dynamicMember keyPath: KeyPath<T, Property>) -> Wrapper<Property> {
      get { .init() }
    }
  }

  struct Field {
    @Wrapper var v: Bool = true
  }

  struct Arr {
    var fields: [Field] = []
  }

  struct Test {
    @Wrapper var data: Arr

    func test(_ index: Int) {
      let _ = self.$data.fields[index].v.wrappedValue
    }
  }
}


// https://github.com/apple/swift/issues/53865
// Ambiguity in expression which matches both dynamic member lookup and
// declaration from constrained extension

@dynamicMemberLookup
struct S_53865<RawValue> {
  var rawValue: RawValue

  subscript<Subject>(dynamicMember keyPath: KeyPath<RawValue, Subject>) -> Subject {
    rawValue[keyPath: keyPath]
  }
}

extension S_53865: Hashable, Equatable where RawValue: Hashable {
  func hash(into hasher: inout Hasher) {
    hasher.combine(self.rawValue)
  }
}

func test_constrained_ext_vs_dynamic_member() {
  // CHECK: function_ref @$s29keypath_dynamic_member_lookup7S_53865VAASHRzlE9hashValueSivg
  _ = S_53865<Int>(rawValue: 1).hashValue // Ok, keep choice from constrained extension
}

// https://github.com/apple/swift/issues/54310
// https://github.com/apple/swift/issues/57571
// Make sure we properly handle IUO unwraps for key path dynamic members.

struct S_54310_Base {
  var i: Int!
  subscript(_ x: Int) -> Int! { x }
}

@dynamicMemberLookup
struct S_54310 {
  subscript(dynamicMember kp: KeyPath<S_54310_Base, Int>) -> Void { () }
}

@dynamicMemberLookup
struct S_57571 {
  subscript(dynamicMember kp: KeyPath<S_54310_Base, Int>) -> Int! { 0 }
}

// CHECK-LABEL: sil hidden @$s29keypath_dynamic_member_lookup13testIUOUnwrapyyAA7S_54310V_AA7S_57571VtF
func testIUOUnwrap(_ x: S_54310, _ y: S_57571) {
  // CHECK: keypath $KeyPath<S_54310_Base, Int>, (root $S_54310_Base; stored_property #S_54310_Base.i : $Optional<Int>; optional_force : $Int)
  x.i

  // CHECK: keypath $KeyPath<S_54310_Base, Int>, (root $S_54310_Base; gettable_property $Optional<Int>,  id @$s29keypath_dynamic_member_lookup12S_54310_BaseVySiSgSicig : $@convention(method) (Int, S_54310_Base) -> Optional<Int>, getter @$s29keypath_dynamic_member_lookup12S_54310_BaseVySiSgSicipACTK : $@convention(keypath_accessor_getter) (@in_guaranteed S_54310_Base, @in_guaranteed Int) -> @out Optional<Int>, indices [%$0 : $Int : $Int], indices_equals @$sSiTH : $@convention(keypath_accessor_equals) (@in_guaranteed Int, @in_guaranteed Int) -> Bool, indices_hash @$sSiTh : $@convention(keypath_accessor_hash) (@in_guaranteed Int) -> Int; optional_force : $Int)
  x[5]

  // CHECK: [[INNER_KP:%[0-9]+]] = keypath $KeyPath<S_54310_Base, Int>, (root $S_54310_Base; stored_property #S_54310_Base.i : $Optional<Int>; optional_force : $Int)
  // CHECK: $KeyPath<S_54310, ()>, (root $S_54310; gettable_property $(),  id @$s29keypath_dynamic_member_lookup7S_54310V0B6Memberys7KeyPathCyAA12S_54310_BaseVSiG_tcig : $@convention(method) (@guaranteed KeyPath<S_54310_Base, Int>, S_54310) -> (), getter @$s29keypath_dynamic_member_lookup7S_54310V0B6Memberys7KeyPathCyAA12S_54310_BaseVSiG_tcipACTK : $@convention(keypath_accessor_getter) (@in_guaranteed S_54310, @in_guaranteed KeyPath<S_54310_Base, Int>) -> @out (), indices [%$0 : $KeyPath<S_54310_Base, Int> : $KeyPath<S_54310_Base, Int>], indices_equals @$ss7KeyPathCy29keypath_dynamic_member_lookup12S_54310_BaseVSiGTH : $@convention(keypath_accessor_equals) (@in_guaranteed KeyPath<S_54310_Base, Int>, @in_guaranteed KeyPath<S_54310_Base, Int>) -> Bool, indices_hash @$ss7KeyPathCy29keypath_dynamic_member_lookup12S_54310_BaseVSiGTh : $@convention(keypath_accessor_hash) (@in_guaranteed KeyPath<S_54310_Base, Int>) -> Int) ([[INNER_KP]])
  _ = \S_54310.i

  // CHECK: [[INNER_SUB_KP:%[0-9]+]] = keypath $KeyPath<S_54310_Base, Int>, (root $S_54310_Base; gettable_property $Optional<Int>, id @$s29keypath_dynamic_member_lookup12S_54310_BaseVySiSgSicig : $@convention(method) (Int, S_54310_Base) -> Optional<Int>, getter @$s29keypath_dynamic_member_lookup12S_54310_BaseVySiSgSicipACTK : $@convention(keypath_accessor_getter) (@in_guaranteed S_54310_Base, @in_guaranteed Int) -> @out Optional<Int>, indices [%$0 : $Int : $Int], indices_equals @$sSiTH : $@convention(keypath_accessor_equals) (@in_guaranteed Int, @in_guaranteed Int) -> Bool, indices_hash @$sSiTh : $@convention(keypath_accessor_hash) (@in_guaranteed Int) -> Int; optional_force : $Int)
  // CHECK: keypath $KeyPath<S_54310, ()>, (root $S_54310; gettable_property $(),  id @$s29keypath_dynamic_member_lookup7S_54310V0B6Memberys7KeyPathCyAA12S_54310_BaseVSiG_tcig : $@convention(method) (@guaranteed KeyPath<S_54310_Base, Int>, S_54310) -> (), getter @$s29keypath_dynamic_member_lookup7S_54310V0B6Memberys7KeyPathCyAA12S_54310_BaseVSiG_tcipACTK : $@convention(keypath_accessor_getter) (@in_guaranteed S_54310, @in_guaranteed KeyPath<S_54310_Base, Int>) -> @out (), indices [%$0 : $KeyPath<S_54310_Base, Int> : $KeyPath<S_54310_Base, Int>], indices_equals @$ss7KeyPathCy29keypath_dynamic_member_lookup12S_54310_BaseVSiGTH : $@convention(keypath_accessor_equals) (@in_guaranteed KeyPath<S_54310_Base, Int>, @in_guaranteed KeyPath<S_54310_Base, Int>) -> Bool, indices_hash @$ss7KeyPathCy29keypath_dynamic_member_lookup12S_54310_BaseVSiGTh : $@convention(keypath_accessor_hash) (@in_guaranteed KeyPath<S_54310_Base, Int>) -> Int) ([[INNER_SUB_KP]])
  _ = \S_54310.[5]

  // https://github.com/apple/swift/issues/57571: Make sure we can handle IUO
  // unwraps in both the inner and outer key paths.

  // CHECK: [[INNER_KP2:%[0-9]+]] = keypath $KeyPath<S_54310_Base, Int>, (root $S_54310_Base; stored_property #S_54310_Base.i : $Optional<Int>; optional_force : $Int)
  // CHECK: keypath $KeyPath<S_57571, Optional<Int>>, (root $S_57571; gettable_property $Optional<Int>,  id @$s29keypath_dynamic_member_lookup7S_57571V0B6MemberSiSgs7KeyPathCyAA12S_54310_BaseVSiG_tcig : $@convention(method) (@guaranteed KeyPath<S_54310_Base, Int>, S_57571) -> Optional<Int>, getter @$s29keypath_dynamic_member_lookup7S_57571V0B6MemberSiSgs7KeyPathCyAA12S_54310_BaseVSiG_tcipACTK : $@convention(keypath_accessor_getter) (@in_guaranteed S_57571, @in_guaranteed KeyPath<S_54310_Base, Int>) -> @out Optional<Int>, indices [%$0 : $KeyPath<S_54310_Base, Int> : $KeyPath<S_54310_Base, Int>], indices_equals @$ss7KeyPathCy29keypath_dynamic_member_lookup12S_54310_BaseVSiGTH : $@convention(keypath_accessor_equals) (@in_guaranteed KeyPath<S_54310_Base, Int>, @in_guaranteed KeyPath<S_54310_Base, Int>) -> Bool, indices_hash @$ss7KeyPathCy29keypath_dynamic_member_lookup12S_54310_BaseVSiGTh : $@convention(keypath_accessor_hash) (@in_guaranteed KeyPath<S_54310_Base, Int>) -> Int) ([[INNER_KP2]])
  _ = \S_57571.i

  // CHECK: [[INNER_KP3:%[0-9]+]] = keypath $KeyPath<S_54310_Base, Int>, (root $S_54310_Base; stored_property #S_54310_Base.i : $Optional<Int>; optional_force : $Int)
  // CHECK: keypath $KeyPath<S_57571, Int>, (root $S_57571; gettable_property $Optional<Int>,  id @$s29keypath_dynamic_member_lookup7S_57571V0B6MemberSiSgs7KeyPathCyAA12S_54310_BaseVSiG_tcig : $@convention(method) (@guaranteed KeyPath<S_54310_Base, Int>, S_57571) -> Optional<Int>, getter @$s29keypath_dynamic_member_lookup7S_57571V0B6MemberSiSgs7KeyPathCyAA12S_54310_BaseVSiG_tcipACTK : $@convention(keypath_accessor_getter) (@in_guaranteed S_57571, @in_guaranteed KeyPath<S_54310_Base, Int>) -> @out Optional<Int>, indices [%$0 : $KeyPath<S_54310_Base, Int> : $KeyPath<S_54310_Base, Int>], indices_equals @$ss7KeyPathCy29keypath_dynamic_member_lookup12S_54310_BaseVSiGTH : $@convention(keypath_accessor_equals) (@in_guaranteed KeyPath<S_54310_Base, Int>, @in_guaranteed KeyPath<S_54310_Base, Int>) -> Bool, indices_hash @$ss7KeyPathCy29keypath_dynamic_member_lookup12S_54310_BaseVSiGTh : $@convention(keypath_accessor_hash) (@in_guaranteed KeyPath<S_54310_Base, Int>) -> Int; optional_force : $Int) ([[INNER_KP3]])
  let _: KeyPath<S_57571, Int> = \S_57571.i

  // CHECK: [[INNER_KP4:%[0-9]+]] = keypath $KeyPath<S_54310_Base, Int>, (root $S_54310_Base; gettable_property $Optional<Int>,  id @$s29keypath_dynamic_member_lookup12S_54310_BaseVySiSgSicig : $@convention(method) (Int, S_54310_Base) -> Optional<Int>, getter @$s29keypath_dynamic_member_lookup12S_54310_BaseVySiSgSicipACTK : $@convention(keypath_accessor_getter) (@in_guaranteed S_54310_Base, @in_guaranteed Int) -> @out Optional<Int>, indices [%$0 : $Int : $Int], indices_equals @$sSiTH : $@convention(keypath_accessor_equals) (@in_guaranteed Int, @in_guaranteed Int) -> Bool, indices_hash @$sSiTh : $@convention(keypath_accessor_hash) (@in_guaranteed Int) -> Int; optional_force : $Int)
  // CHECK: keypath $KeyPath<S_57571, Optional<Int>>, (root $S_57571; gettable_property $Optional<Int>,  id @$s29keypath_dynamic_member_lookup7S_57571V0B6MemberSiSgs7KeyPathCyAA12S_54310_BaseVSiG_tcig : $@convention(method) (@guaranteed KeyPath<S_54310_Base, Int>, S_57571) -> Optional<Int>, getter @$s29keypath_dynamic_member_lookup7S_57571V0B6MemberSiSgs7KeyPathCyAA12S_54310_BaseVSiG_tcipACTK : $@convention(keypath_accessor_getter) (@in_guaranteed S_57571, @in_guaranteed KeyPath<S_54310_Base, Int>) -> @out Optional<Int>, indices [%$0 : $KeyPath<S_54310_Base, Int> : $KeyPath<S_54310_Base, Int>], indices_equals @$ss7KeyPathCy29keypath_dynamic_member_lookup12S_54310_BaseVSiGTH : $@convention(keypath_accessor_equals) (@in_guaranteed KeyPath<S_54310_Base, Int>, @in_guaranteed KeyPath<S_54310_Base, Int>) -> Bool, indices_hash @$ss7KeyPathCy29keypath_dynamic_member_lookup12S_54310_BaseVSiGTh : $@convention(keypath_accessor_hash) (@in_guaranteed KeyPath<S_54310_Base, Int>) -> Int) ([[INNER_KP4]])
  _ = \S_57571.[0]

  // CHECK: [[INNER_KP5:%[0-9]+]] = keypath $KeyPath<S_54310_Base, Int>, (root $S_54310_Base; gettable_property $Optional<Int>,  id @$s29keypath_dynamic_member_lookup12S_54310_BaseVySiSgSicig : $@convention(method) (Int, S_54310_Base) -> Optional<Int>, getter @$s29keypath_dynamic_member_lookup12S_54310_BaseVySiSgSicipACTK : $@convention(keypath_accessor_getter) (@in_guaranteed S_54310_Base, @in_guaranteed Int) -> @out Optional<Int>, indices [%$0 : $Int : $Int], indices_equals @$sSiTH : $@convention(keypath_accessor_equals) (@in_guaranteed Int, @in_guaranteed Int) -> Bool, indices_hash @$sSiTh : $@convention(keypath_accessor_hash) (@in_guaranteed Int) -> Int; optional_force : $Int)
  // CHECK: keypath $KeyPath<S_57571, Int>, (root $S_57571; gettable_property $Optional<Int>,  id @$s29keypath_dynamic_member_lookup7S_57571V0B6MemberSiSgs7KeyPathCyAA12S_54310_BaseVSiG_tcig : $@convention(method) (@guaranteed KeyPath<S_54310_Base, Int>, S_57571) -> Optional<Int>, getter @$s29keypath_dynamic_member_lookup7S_57571V0B6MemberSiSgs7KeyPathCyAA12S_54310_BaseVSiG_tcipACTK : $@convention(keypath_accessor_getter) (@in_guaranteed S_57571, @in_guaranteed KeyPath<S_54310_Base, Int>) -> @out Optional<Int>, indices [%$0 : $KeyPath<S_54310_Base, Int> : $KeyPath<S_54310_Base, Int>], indices_equals @$ss7KeyPathCy29keypath_dynamic_member_lookup12S_54310_BaseVSiGTH : $@convention(keypath_accessor_equals) (@in_guaranteed KeyPath<S_54310_Base, Int>, @in_guaranteed KeyPath<S_54310_Base, Int>) -> Bool, indices_hash @$ss7KeyPathCy29keypath_dynamic_member_lookup12S_54310_BaseVSiGTh : $@convention(keypath_accessor_hash) (@in_guaranteed KeyPath<S_54310_Base, Int>) -> Int; optional_force : $Int) ([[INNER_KP5]])
  let _: KeyPath<S_57571, Int> = \S_57571.[0]

  // CHECK: [[INNER_KP6:%[0-9]+]] = keypath $KeyPath<S_54310_Base, Int>, (root $S_54310_Base; stored_property #S_54310_Base.i : $Optional<Int>; optional_force : $Int)
  // CHECK: [[YI_OPT:%[0-9]+]] = apply {{%[0-9]+}}([[INNER_KP6]], {{%[0-9]+}}) : $@convention(method) (@guaranteed KeyPath<S_54310_Base, Int>, S_57571) -> Optional<Int>
  // CHECK: switch_enum [[YI_OPT]]
  // CHECK: unreachable
  // CHECK: bb{{[0-9]+}}(%{{[0-9]+}} : $Int)
  let _: Int = y.i

  // CHECK: [[INNER_KP7:%[0-9]+]] = keypath $KeyPath<S_54310_Base, Int>, (root $S_54310_Base; gettable_property $Optional<Int>,  id @$s29keypath_dynamic_member_lookup12S_54310_BaseVySiSgSicig : $@convention(method) (Int, S_54310_Base) -> Optional<Int>, getter @$s29keypath_dynamic_member_lookup12S_54310_BaseVySiSgSicipACTK : $@convention(keypath_accessor_getter) (@in_guaranteed S_54310_Base, @in_guaranteed Int) -> @out Optional<Int>, indices [%$0 : $Int : $Int], indices_equals @$sSiTH : $@convention(keypath_accessor_equals) (@in_guaranteed Int, @in_guaranteed Int) -> Bool, indices_hash @$sSiTh : $@convention(keypath_accessor_hash) (@in_guaranteed Int) -> Int; optional_force : $Int)
  // CHECK: [[Y0_OPT:%[0-9]+]] = apply {{%[0-9]+}}([[INNER_KP7]], {{%[0-9]+}}) : $@convention(method) (@guaranteed KeyPath<S_54310_Base, Int>, S_57571) -> Optional<Int>
  // CHECK: switch_enum [[Y0_OPT]]
  // CHECK: unreachable
  // CHECK: bb{{[0-9]+}}(%{{[0-9]+}} : $Int)
  let _: Int = y[0]
}

// https://github.com/apple/swift/issues/54313
// Make sure the outer key path reflects the mutability of the
// 'dynamicMember:' subscript.

struct S_54313_Base {
  var mutable: Int
  let immutable: Int
}

@dynamicMemberLookup
struct S_54313_Mutable {
  subscript(dynamicMember kp: KeyPath<S_54313_Base, Int>) -> Int {
    get { 5 } set {}
  }
}

@dynamicMemberLookup
struct S_54313_Immutable {
  subscript(dynamicMember kp: KeyPath<S_54313_Base, Int>) -> Int {
    get { 5 }
  }
}

// CHECK-LABEL: sil hidden @$s29keypath_dynamic_member_lookup21testKeyPathMutabilityyyF : $@convention(thin) () -> ()
func testKeyPathMutability() {
  // CHECK: keypath $KeyPath<S_54313_Base, Int>, (root $S_54313_Base; stored_property #S_54313_Base.mutable : $Int)
  // CHECK: keypath $WritableKeyPath<S_54313_Mutable, Int>, (root $S_54313_Mutable; settable_property $Int
  _ = \S_54313_Mutable.mutable

  // CHECK: keypath $KeyPath<S_54313_Base, Int>, (root $S_54313_Base; stored_property #S_54313_Base.immutable : $Int)
  // CHECK: keypath $WritableKeyPath<S_54313_Mutable, Int>, (root $S_54313_Mutable; settable_property $Int
  _ = \S_54313_Mutable.immutable

  // CHECK: keypath $KeyPath<S_54313_Base, Int>, (root $S_54313_Base; stored_property #S_54313_Base.mutable : $Int)
  // CHECK: keypath $KeyPath<S_54313_Immutable, Int>, (root $S_54313_Immutable; gettable_property $Int
  _ = \S_54313_Immutable.mutable

  // CHECK: keypath $KeyPath<S_54313_Base, Int>, (root $S_54313_Base; stored_property #S_54313_Base.immutable : $Int)
  // CHECK: keypath $KeyPath<S_54313_Immutable, Int>, (root $S_54313_Immutable; gettable_property $Int
  _ = \S_54313_Immutable.immutable
}

// https://github.com/apple/swift/issues/54352
// Make sure we properly handle default arguments.

struct HasDefaultedSubscript {
  subscript(_ x: Int = 0) -> Int { x }
}

@dynamicMemberLookup
struct S_54352 {
  subscript(dynamicMember kp: KeyPath<HasDefaultedSubscript, Int>) -> Int { 0 }
}

// CHECK-LABEL: sil hidden @$s29keypath_dynamic_member_lookup28testDynamicMemberWithDefaultyyAA7S_54352VF : $@convention(thin) (S_54352) -> ()
func testDynamicMemberWithDefault(_ x: S_54352) {
  // CHECK: [[DEF_FN:%[0-9]+]] = function_ref @$s29keypath_dynamic_member_lookup21HasDefaultedSubscriptVyS2icipfA_ : $@convention(thin) () -> Int
  // CHECK: [[DEF_ARG:%[0-9]+]] = apply [[DEF_FN]]()
  // CHECK: [[KP:%[0-9]+]] = keypath $KeyPath<HasDefaultedSubscript, Int>, (root $HasDefaultedSubscript; gettable_property $Int,  id @$s29keypath_dynamic_member_lookup21HasDefaultedSubscriptVyS2icig : $@convention(method) (Int, HasDefaultedSubscript) -> Int, getter @$s29keypath_dynamic_member_lookup21HasDefaultedSubscriptVyS2icipACTK : $@convention(keypath_accessor_getter) (@in_guaranteed HasDefaultedSubscript, @in_guaranteed Int) -> @out Int, indices [%$0 : $Int : $Int], indices_equals @$sSiTH : $@convention(keypath_accessor_equals) (@in_guaranteed Int, @in_guaranteed Int) -> Bool, indices_hash @$sSiTh : $@convention(keypath_accessor_hash) (@in_guaranteed Int) -> Int) ([[DEF_ARG]])
  // CHECK: [[SUB_GET:%[0-9]+]] = function_ref @$s29keypath_dynamic_member_lookup7S_54352V0B6MemberSis7KeyPathCyAA21HasDefaultedSubscriptVSiG_tcig : $@convention(method) (@guaranteed KeyPath<HasDefaultedSubscript, Int>, S_54352) -> Int
  // CHECK: apply [[SUB_GET]]([[KP]], {{%[0-9]+}})
  _ = x[]

  // CHECK: [[DEF_FN:%[0-9]+]] = function_ref @$s29keypath_dynamic_member_lookup21HasDefaultedSubscriptVyS2icipfA_ : $@convention(thin) () -> Int
  // CHECK: [[DEF_ARG:%[0-9]+]] = apply [[DEF_FN]]()
  // CHECK: [[INNER_KP:%[0-9]+]] = keypath $KeyPath<HasDefaultedSubscript, Int>, (root $HasDefaultedSubscript; gettable_property $Int,  id @$s29keypath_dynamic_member_lookup21HasDefaultedSubscriptVyS2icig : $@convention(method) (Int, HasDefaultedSubscript) -> Int, getter @$s29keypath_dynamic_member_lookup21HasDefaultedSubscriptVyS2icipACTK : $@convention(keypath_accessor_getter) (@in_guaranteed HasDefaultedSubscript, @in_guaranteed Int) -> @out Int, indices [%$0 : $Int : $Int], indices_equals @$sSiTH : $@convention(keypath_accessor_equals) (@in_guaranteed Int, @in_guaranteed Int) -> Bool, indices_hash @$sSiTh : $@convention(keypath_accessor_hash) (@in_guaranteed Int) -> Int) ([[DEF_ARG]])
  // CHECK: [[OUTER_KP:%[0-9]+]] = keypath $KeyPath<S_54352, Int>, (root $S_54352; gettable_property $Int,  id @$s29keypath_dynamic_member_lookup7S_54352V0B6MemberSis7KeyPathCyAA21HasDefaultedSubscriptVSiG_tcig : $@convention(method) (@guaranteed KeyPath<HasDefaultedSubscript, Int>, S_54352) -> Int, getter @$s29keypath_dynamic_member_lookup7S_54352V0B6MemberSis7KeyPathCyAA21HasDefaultedSubscriptVSiG_tcipACTK : $@convention(keypath_accessor_getter) (@in_guaranteed S_54352, @in_guaranteed KeyPath<HasDefaultedSubscript, Int>) -> @out Int, indices [%$0 : $KeyPath<HasDefaultedSubscript, Int> : $KeyPath<HasDefaultedSubscript, Int>], indices_equals @$ss7KeyPathCy29keypath_dynamic_member_lookup21HasDefaultedSubscriptVSiGTH : $@convention(keypath_accessor_equals) (@in_guaranteed KeyPath<HasDefaultedSubscript, Int>, @in_guaranteed KeyPath<HasDefaultedSubscript, Int>) -> Bool, indices_hash @$ss7KeyPathCy29keypath_dynamic_member_lookup21HasDefaultedSubscriptVSiGTh : $@convention(keypath_accessor_hash) (@in_guaranteed KeyPath<HasDefaultedSubscript, Int>) -> Int) ([[INNER_KP]])
  _ = \S_54352.[]
}

// https://github.com/apple/swift/issues/54150
// Key path dynamic member lookup crash

@dynamicMemberLookup
protocol P_54150 {
  subscript(dynamicMember member: KeyPath<Self, Any>) -> Any { get }
}

extension P_54150 {
  subscript(dynamicMember member: KeyPath<Self, Any>) -> Any {
    self[keyPath: member] // Ok
    // CHECK: function_ref @swift_getAtKeyPath
    // CHECK-NEXT: apply %{{.*}}<Self, Any>({{.*}})
  }
}

@dynamicMemberLookup
struct S_54150 {
  let value: Int

  subscript<T>(dynamicMember member: KeyPath<Self, T>) -> T {
    return self[keyPath: member]
    // CHECK: function_ref @swift_getAtKeyPath
    // CHECK-NEXT: apply %{{.*}}<S_54150, T>({{.*}})
  }
}

// https://github.com/swiftlang/swift/issues/56837
@dynamicMemberLookup
struct Issue56837<Value, Integer: FixedWidthInteger> {
    var value: Value
    init(_ value: Value) {
        self.value = value
    }
    subscript<T>(dynamicMember keyPath: KeyPath<Value, T>) -> T {
        get { return self.value[keyPath: keyPath] }
    }
    subscript(type value: Integer) -> Bool {
        get { return true }
    }
}
struct TestIssue56837 {
    typealias T1 = Issue56837<String, Int8>
    typealias T2 = Issue56837<T1, Int16>
    typealias T3 = Issue56837<T2, Int32>
    typealias T4 = Issue56837<T3, Int64>
    static func test() {
        let value: T4 = .init(.init(.init(.init("Swift"))))
        _ = value[type: Int64.max]
        _ = value[type: Int32.max]
        _ = value[type: Int16.max]
        _ = value[type: Int8.max]
    }
}

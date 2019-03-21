// RUN: %target-swift-frontend -emit-sil -verify %s | %FileCheck %s

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
// CHECK-NEXT: apply %45<Rectangle, Point>({{.*}})
// CHECK: function_ref @$s29keypath_dynamic_member_lookup4LensV0B6MemberACyqd__Gs7KeyPathCyxqd__G_tcluig
// CHECK-NEXT: apply %54<Point, Int>({{.*}})
_ = lens.topLeft.x

// CHECK: function_ref @$s29keypath_dynamic_member_lookup4LensV0B6MemberACyqd__Gs15WritableKeyPathCyxqd__G_tcluig
// CHECK-NEXT: apply %69<Rectangle, Point>({{.*}})
// CHECK: function_ref @$s29keypath_dynamic_member_lookup4LensV0B6MemberACyqd__Gs15WritableKeyPathCyxqd__G_tcluig
// CHECK-NEXT: apply %76<Point, Int>({{.*}})
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

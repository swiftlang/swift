// RUN: %empty-directory(%t)
// RUN: %target-build-swift %s -g -Xfrontend -enable-experimental-keypath-components -o %t/a.out
// RUN: %target-run %t/a.out
// REQUIRES: executable_test

import StdlibUnittest

var keyPathImpl = TestSuite("key path implementation")

func align<T>(_ offset: Int, to: T.Type) -> Int {
  let alignMask = MemoryLayout<T>.alignment - 1
  return (offset + alignMask) & ~alignMask
}

// FIXME: Object header size will eventually be MemoryLayout<Int>.size * 2
let classHeaderSize = MemoryLayout<Int>.size + 8

class C<T> {
  var x: Int
  var y: LifetimeTracked?
  var z: T

  init(x: Int, y: LifetimeTracked?, z: T) {
    self.x = x
    self.y = y
    self.z = z
  }

  static var x_offset: Int { return classHeaderSize }
  static var y_offset: Int { return x_offset + MemoryLayout<Int>.size }
  static var z_offset: Int {
    return align(y_offset + MemoryLayout<LifetimeTracked?>.size, to: T.self)
  }
}

struct Point: Equatable {
  var x: Double
  var y: Double
  var trackLifetime = LifetimeTracked(123)
  
  init(x: Double, y: Double) {
    self.x = x
    self.y = y
  }
  
  static func ==(a: Point, b: Point) -> Bool {
    return a.x == b.x && a.y == b.y
  }

  static var x_offset: Int {
    return 0
  }
  static var y_offset: Int {
    return MemoryLayout<Double>.size
  }
}


struct S<T: Equatable>: Equatable {
  var x: Int
  var y: LifetimeTracked?
  var z: T
  var p: Point
  var c: C<T>
  
  static func ==(a: S, b: S) -> Bool {
    return a.x == b.x
      && a.y === b.y
      && a.z == b.z
      && a.p == b.p
      && a.c === b.c
  }

  static var x_offset: Int { return 0 }
  static var y_offset: Int { return MemoryLayout<Int>.size }
  static var z_offset: Int {
    return align(y_offset + MemoryLayout<LifetimeTracked?>.size,
                 to: T.self)
  }
  static var p_offset: Int {
    return align(z_offset + MemoryLayout<T>.size, to: Point.self)
  }
  static var c_offset: Int {
    return p_offset + MemoryLayout<Point>.size
  }
}

class Oroborous {
  var o: Oroborous

  init() { fatalError() }
}

struct CratePair<T, U> {
  var left: Crate<T>
  var right: Crate<U>

  static var left_offset: Int { return 0 }
  static var right_offset: Int { return MemoryLayout<Crate<T>>.size }
}
class Crate<T> {
  var value: T
  
  init(value: T) { self.value = value }

  static var value_offset: Int { return align(classHeaderSize, to: T.self) }
}

// Helper to build keypaths with specific layouts
struct TestKeyPathBuilder {
  var buffer: UnsafeMutableRawBufferPointer
  var state: State = .header
  var hasReferencePrefix = false

  init(buffer: UnsafeMutableRawBufferPointer) {
    self.buffer = buffer
  }

  enum State {
    case header, component, type

    mutating func advance() {
      switch self {
      case .header, .type: self = .component
      case .component:     self = .type
      }
    }
  }
  
  func finish() {
    assert(buffer.count == 0,
           "Did not fill entire buffer")
    assert(state == .type, "should end expecting a type")
    assert(!hasReferencePrefix, "unterminated reference prefix")
  }
  
  mutating func push(_ value: UInt32) {
    buffer.storeBytes(of: value, as: UInt32.self)
    buffer = .init(start: buffer.baseAddress! + 4, count: buffer.count - 4)
  }
  mutating func push(_ value: Any.Type) {
    var misalign = Int(bitPattern: buffer.baseAddress) % MemoryLayout<Int>.alignment
    if misalign != 0 {
      misalign = MemoryLayout<Int>.alignment - misalign
      buffer = .init(start: buffer.baseAddress! + misalign,
                     count: buffer.count - misalign)
    }
    buffer.storeBytes(of: value, as: Any.Type.self)
    buffer = .init(start: buffer.baseAddress! + MemoryLayout<Int>.size,
                   count: buffer.count - MemoryLayout<Int>.size)
  }
  
  mutating func addHeader(trivial: Bool, hasReferencePrefix: Bool) {
    assert(state == .header, "not expecting a header")
    let size = buffer.count - MemoryLayout<Int>.size
    assert(buffer.count > 0 && buffer.count <= 0x3FFF_FFFF,
           "invalid buffer size")
    let header: UInt32 = UInt32(size)
      | (trivial ? 0x8000_0000 : 0)
      | (hasReferencePrefix ? 0x4000_0000 : 0)
    push(header)
    if MemoryLayout<Int>.size == 8 {
      push(0)
    }
    self.hasReferencePrefix = hasReferencePrefix
    state.advance()
  }
  
  mutating func addOffsetComponent(offset: Int,
                                   kindMask: UInt32,
                                   forceOverflow: Bool,
                                   endsReferencePrefix: Bool) {
    assert(state == .component, "not expecting a component")
    assert(offset >= 0 && offset <= 0x7FFF_FFFF,
           "invalid offset")
    let referencePrefixMask: UInt32 = endsReferencePrefix ? 0x8000_0000 : 0
    if forceOverflow || offset >= 0x1FFF_FFFF {
      // Offset is overflowed into another word
      push(referencePrefixMask | kindMask | 0x1FFF_FFFF)
      push(UInt32(offset))
    } else {
      // Offset is packed in-line
      push(referencePrefixMask | kindMask | UInt32(offset))
    }
    if endsReferencePrefix {
      assert(hasReferencePrefix, "ending nonexistent reference prefix")
      hasReferencePrefix = false
    }
    state.advance()
  }
  
  mutating func addStructComponent(offset: Int,
                                   forceOverflow: Bool = false,
                                   endsReferencePrefix: Bool = false) {
    addOffsetComponent(offset: offset, kindMask: 0,
                       forceOverflow: forceOverflow,
                       endsReferencePrefix: endsReferencePrefix)
  }

  mutating func addClassComponent(offset: Int,
                                  forceOverflow: Bool = false,
                                  endsReferencePrefix: Bool = false) {
    addOffsetComponent(offset: offset, kindMask: 0x4000_0000,
                       forceOverflow: forceOverflow,
                       endsReferencePrefix: endsReferencePrefix)
  }
  
  mutating func addType(_ type: Any.Type) {
    assert(state == .type, "not expecting a type")
    push(type)
    state.advance()
  }
}

extension AnyKeyPath {
  static func build(capacityInBytes: Int,
                withBuilder: (inout TestKeyPathBuilder) -> Void) -> Self {
    return _create(capacityInBytes: capacityInBytes) {
      var builder = TestKeyPathBuilder(buffer: $0)
      withBuilder(&builder)
      builder.finish()
    }
  }
}

keyPathImpl.test("struct components") {
  let s_x = WritableKeyPath<S<String>, Int>
    .build(capacityInBytes: MemoryLayout<Int>.size + 4) {
      $0.addHeader(trivial: true, hasReferencePrefix: false)
      $0.addStructComponent(offset: S<String>.x_offset)
    }

  let s_y = WritableKeyPath<S<String>, LifetimeTracked?>
    .build(capacityInBytes: MemoryLayout<Int>.size + 4) {
      $0.addHeader(trivial: true, hasReferencePrefix: false)
      $0.addStructComponent(offset: S<String>.y_offset)
    }

  let s_z = WritableKeyPath<S<String>, String>
    .build(capacityInBytes: MemoryLayout<Int>.size + 4) {
      $0.addHeader(trivial: true, hasReferencePrefix: false)
      $0.addStructComponent(offset: S<String>.z_offset)
    }

  let s_p = WritableKeyPath<S<String>, Point>
    .build(capacityInBytes: MemoryLayout<Int>.size + 4) {
      $0.addHeader(trivial: true, hasReferencePrefix: false)
      $0.addStructComponent(offset: S<String>.p_offset)
    }

  let twoComponentSize = MemoryLayout<Int>.size * 3 + 4
  let s_p_x = WritableKeyPath<S<String>, Double>
    .build(capacityInBytes: twoComponentSize) {
      $0.addHeader(trivial: true, hasReferencePrefix: false)
      $0.addStructComponent(offset: S<String>.p_offset)
      $0.addType(Point.self)
      $0.addStructComponent(offset: Point.x_offset)
    }

  let s_p_y = WritableKeyPath<S<String>, Double>
    .build(capacityInBytes: twoComponentSize) {
      $0.addHeader(trivial: true, hasReferencePrefix: false)
      $0.addStructComponent(offset: S<String>.p_offset)
      $0.addType(Point.self)
      $0.addStructComponent(offset: Point.y_offset)
    }


  // \("") forces the string to be computed at runtime (and therefore allocated)
  let c = C(x: 679, y: nil, z: "buffalo\("")")
  var value = S(x: 1738, y: nil, z: "bottles of beer\("")",
                p: .init(x: 0.5, y: -0.5), c: c)
  expectEqual(value[keyPath: s_x], 1738)
  value[keyPath: s_x] = 679
  expectEqual(value[keyPath: s_x], 679)

  expectTrue(value[keyPath: s_y] === nil)

  let object1 = LifetimeTracked(1739)
  value[keyPath: s_y] = object1
  expectTrue(value[keyPath: s_y] === object1)
  expectTrue(value.y === object1)

  let object2 = LifetimeTracked(1740)
  value[keyPath: s_y] = object2
  expectTrue(value[keyPath: s_y] === object2)
  expectTrue(value.y === object2)

  expectEqual(value[keyPath: s_z], "bottles of beer")
  value[keyPath: s_z] = "cans of lemonade\("")"
  expectEqual(value[keyPath: s_z], "cans of lemonade")
  expectEqual(value.z, "cans of lemonade")
  
  expectEqual(value[keyPath: s_p], Point(x: 0.5, y: -0.5))
  expectEqual(value.p, Point(x: 0.5, y: -0.5))
  expectEqual(value[keyPath: s_p_x], 0.5)
  expectEqual(value.p.x, 0.5)
  expectEqual(value[keyPath: s_p_y], -0.5)
  expectEqual(value.p.y, -0.5)

  value[keyPath: s_p] = Point(x: -3.0, y: 4.0)
  expectEqual(value[keyPath: s_p], Point(x: -3.0, y: 4.0))
  expectEqual(value.p, Point(x: -3.0, y: 4.0))
  expectEqual(value[keyPath: s_p_x], -3.0)
  expectEqual(value.p.x, -3.0)
  expectEqual(value[keyPath: s_p_y], 4.0)
  expectEqual(value.p.y, 4.0)
  
  value[keyPath: s_p_x] = 5.0
  expectEqual(value[keyPath: s_p], Point(x: 5.0, y: 4.0))
  expectEqual(value.p, Point(x: 5.0, y: 4.0))
  expectEqual(value[keyPath: s_p_x], 5.0)
  expectEqual(value.p.x, 5.0)
  expectEqual(value[keyPath: s_p_y], 4.0)
  expectEqual(value.p.y, 4.0)
  
  value[keyPath: s_p_y] = -11.0
  expectEqual(value[keyPath: s_p], Point(x: 5.0, y: -11.0))
  expectEqual(value.p, Point(x: 5.0, y: -11.0))
  expectEqual(value[keyPath: s_p_x], 5.0)
  expectEqual(value.p.x, 5.0)
  expectEqual(value[keyPath: s_p_y], -11.0)
  expectEqual(value.p.y, -11.0)
  
  value[keyPath: s_p].x = 65.0
  expectEqual(value[keyPath: s_p], Point(x: 65.0, y: -11.0))
  expectEqual(value.p, Point(x: 65.0, y: -11.0))
  expectEqual(value[keyPath: s_p_x], 65.0)
  expectEqual(value.p.x, 65.0)
  expectEqual(value[keyPath: s_p_y], -11.0)
  expectEqual(value.p.y, -11.0)
}

keyPathImpl.test("class components") {
  let c_x = ReferenceWritableKeyPath<C<String>, Int>
    .build(capacityInBytes: MemoryLayout<Int>.size + 4) {
      $0.addHeader(trivial: true, hasReferencePrefix: false)
      $0.addClassComponent(offset: C<String>.x_offset)
    }

  let c_y = ReferenceWritableKeyPath<C<String>, LifetimeTracked?>
    .build(capacityInBytes: MemoryLayout<Int>.size + 4) {
      $0.addHeader(trivial: true, hasReferencePrefix: false)
      $0.addClassComponent(offset: C<String>.y_offset)
    }

  let c_z = ReferenceWritableKeyPath<C<String>, String>
    .build(capacityInBytes: MemoryLayout<Int>.size + 4) {
      $0.addHeader(trivial: true, hasReferencePrefix: false)
      $0.addClassComponent(offset: C<String>.z_offset)
    }

  let c = C(x: 679, y: nil, z: "buffalo\("")")
  let value = c
  
  expectEqual(value[keyPath: c_x], 679)
  value[keyPath: c_x] = 1738
  expectEqual(value[keyPath: c_x], 1738)
  expectEqual(value.x, 1738)

  expectTrue(value[keyPath: c_y] === nil)

  let object1 = LifetimeTracked(680)
  value[keyPath: c_y] = object1
  expectTrue(value[keyPath: c_y] === object1)
  expectTrue(value.y === object1)

  let object2 = LifetimeTracked(681)
  value[keyPath: c_y] = object2
  expectTrue(value[keyPath: c_y] === object2)
  expectTrue(value.y === object2)

  expectEqual(value[keyPath: c_z], "buffalo")
  value[keyPath: c_z] = "water buffalo\("")"
  expectEqual(value[keyPath: c_z], "water buffalo")
  expectEqual(value.z, "water buffalo")
  
  var mutValue = value
  let value_c_x: WritableKeyPath = c_x
  let value_c_y: WritableKeyPath = c_y
  
  expectEqual(value[keyPath: value_c_x], 1738)
  mutValue[keyPath: value_c_x] = 86
  expectTrue(value === mutValue)
  expectEqual(value[keyPath: c_x], 86)
  expectEqual(value[keyPath: value_c_x], 86)
  expectEqual(value.x, 86)

  expectTrue(value[keyPath: value_c_y] === object2)
  mutValue[keyPath: value_c_y] = object1
  expectTrue(value === mutValue)
  expectTrue(value[keyPath: c_y] === object1)
  expectTrue(value[keyPath: value_c_y] === object1)
  expectTrue(value.y === object1)
}

keyPathImpl.test("reference prefix") {
  let s_c_x = ReferenceWritableKeyPath<S<String>, Int>
    .build(capacityInBytes: 3 * MemoryLayout<Int>.size + 4) {
      $0.addHeader(trivial: true, hasReferencePrefix: true)
      $0.addStructComponent(offset: S<String>.c_offset,
                            endsReferencePrefix: true)
      $0.addType(C<String>.self)
      $0.addClassComponent(offset: C<String>.x_offset)
    }

  let s_c_y = ReferenceWritableKeyPath<S<String>, LifetimeTracked?>
    .build(capacityInBytes: 3 * MemoryLayout<Int>.size + 4) {
      $0.addHeader(trivial: true, hasReferencePrefix: true)
      $0.addStructComponent(offset: S<String>.c_offset,
                            endsReferencePrefix: true)
      $0.addType(C<String>.self)
      $0.addClassComponent(offset: C<String>.y_offset)
    }

  let s_c_z = ReferenceWritableKeyPath<S<String>, String>
    .build(capacityInBytes: 3 * MemoryLayout<Int>.size + 4) {
      $0.addHeader(trivial: true, hasReferencePrefix: true)
      $0.addStructComponent(offset: S<String>.c_offset,
                            endsReferencePrefix: true)
      $0.addType(C<String>.self)
      $0.addClassComponent(offset: C<String>.z_offset)
    }
  
  let c = C(x: 679, y: nil, z: "buffalo\("")")
  let value = S(x: 1738, y: nil, z: "bottles of beer\("")",
                p: .init(x: 0.5, y: -0.5), c: c)

  expectEqual(value[keyPath: s_c_x], 679)
  value[keyPath: s_c_x] = 6
  expectEqual(value[keyPath: s_c_x], 6)
  expectEqual(value.c.x, 6)
  expectTrue(value.c.y === nil)
  expectEqual(value.c.z, "buffalo")

  let object1 = LifetimeTracked(7)
  let object2 = LifetimeTracked(8)
  expectTrue(value[keyPath: s_c_y] === nil)

  value[keyPath: s_c_y] = object1
  expectTrue(value[keyPath: s_c_y] === object1)
  expectTrue(value.c.y === object1)
  expectEqual(value.c.x, 6)
  expectEqual(value.c.z, "buffalo")

  value[keyPath: s_c_y] = object2
  expectTrue(value[keyPath: s_c_y] === object2)
  expectTrue(value.c.y === object2)
  expectEqual(value.c.x, 6)
  expectEqual(value.c.z, "buffalo")
  
  expectEqual(value[keyPath: s_c_z], "buffalo")
  value[keyPath: s_c_z] = "antelope"
  expectTrue(value[keyPath: s_c_z] == "antelope")
  expectTrue(value.c.z == "antelope")
  expectEqual(value.c.x, 6)
  expectTrue(value.c.y === object2)
  
  let value_s_c_x: WritableKeyPath = s_c_x
  let value_s_c_y: WritableKeyPath = s_c_y
  let value_s_c_z: WritableKeyPath = s_c_z
  
  var mutValue = value
  
  mutValue[keyPath: value_s_c_x] = 7
  expectEqual(value, mutValue)
  expectEqual(value[keyPath: s_c_x], 7)
  expectEqual(value.c.x, 7)
  expectTrue(value.c.y === object2)
  expectEqual(value.c.z, "antelope")

  mutValue[keyPath: value_s_c_y] = object1
  expectEqual(value, mutValue)
  expectTrue(value[keyPath: s_c_y] === object1)
  expectTrue(value.c.y === object1)
  expectEqual(value.c.x, 7)
  expectEqual(value.c.z, "antelope")
  
  mutValue[keyPath: value_s_c_z] = "elk"
  expectEqual(value, mutValue)
  expectTrue(value[keyPath: s_c_z] == "elk")
  expectTrue(value.c.z == "elk")
  expectEqual(value.c.x, 7)
  expectTrue(value.c.y === object1)
}

keyPathImpl.test("overflowed offsets") {
  let s_p = WritableKeyPath<S<String>, Point>
    .build(capacityInBytes: MemoryLayout<Int>.size + 8) {
      $0.addHeader(trivial: true, hasReferencePrefix: false)
      $0.addStructComponent(offset: S<String>.p_offset,
                            forceOverflow: true)
    }

  let c_z = ReferenceWritableKeyPath<C<String>, String>
    .build(capacityInBytes: MemoryLayout<Int>.size + 8) {
      $0.addHeader(trivial: true, hasReferencePrefix: false)
      $0.addClassComponent(offset: C<String>.z_offset,
                           forceOverflow: true)
    }
  
  let c = C(x: 679, y: LifetimeTracked(42), z: "buffalo\("")")
  var sValue = S(x: 1738, y: LifetimeTracked(43),
                 z: "bottles of beer\("")",
                 p: .init(x: 0.5, y: -0.5), c: c)
  let cValue = c
  
  expectEqual(sValue[keyPath: s_p], Point(x: 0.5, y: -0.5))
  sValue[keyPath: s_p] = Point(x: 1.0, y: -1.0)
  expectEqual(sValue.p, Point(x: 1.0, y: -1.0))
  expectEqual(sValue[keyPath: s_p], Point(x: 1.0, y: -1.0))
  
  expectEqual(cValue[keyPath: c_z], "buffalo")
  cValue[keyPath: c_z] = "dik dik"
  expectEqual(cValue.z, "dik dik")
  expectEqual(cValue[keyPath: c_z], "dik dik")
}

keyPathImpl.test("equality") {
  let s_c_z_p_x = ReferenceWritableKeyPath<S<S<String>>, Double>
    .build(capacityInBytes: 7 * MemoryLayout<Int>.size + 4) {
      $0.addHeader(trivial: true, hasReferencePrefix: true)
      // S<S<String>>.c
      $0.addStructComponent(offset: S<S<String>>.c_offset,
                            endsReferencePrefix: true)
      $0.addType(C<S<String>>.self)
      // C<S<String>>.z
      $0.addClassComponent(offset: C<S<String>>.z_offset)
      $0.addType(S<String>.self)
      // S<String>.p
      $0.addStructComponent(offset: S<String>.p_offset)
      $0.addType(Point.self)
      // Point.x
      $0.addStructComponent(offset: Point.x_offset)
    }

  expectEqual(s_c_z_p_x, s_c_z_p_x)
  expectEqual(s_c_z_p_x.hashValue, s_c_z_p_x.hashValue)

  // Structurally equivalent to s_c_z_p_x
  let s_c_z_p_x_2 = ReferenceWritableKeyPath<S<S<String>>, Double>
    .build(capacityInBytes: 7 * MemoryLayout<Int>.size + 4) {
      $0.addHeader(trivial: true, hasReferencePrefix: true)
      // S<S<String>>.c
      $0.addStructComponent(offset: S<S<String>>.c_offset,
                            endsReferencePrefix: true)
      $0.addType(C<S<String>>.self)
      // C<S<String>>.z
      $0.addClassComponent(offset: C<S<String>>.z_offset)
      $0.addType(S<String>.self)
      // S<String>.p
      $0.addStructComponent(offset: S<String>.p_offset)
      $0.addType(Point.self)
      // Point.x
      $0.addStructComponent(offset: Point.x_offset)
    }

  expectEqual(s_c_z_p_x, s_c_z_p_x_2)
  expectEqual(s_c_z_p_x.hashValue, s_c_z_p_x_2.hashValue)

  expectEqual(s_c_z_p_x_2, s_c_z_p_x)
  expectEqual(s_c_z_p_x_2.hashValue, s_c_z_p_x.hashValue)

  // Structurally equivalent, force-overflowed offset components
  let s_c_z_p_x_3 = ReferenceWritableKeyPath<S<S<String>>, Double>
    .build(capacityInBytes: 4 * MemoryLayout<Int>.size + 4 * 8) {
      $0.addHeader(trivial: true, hasReferencePrefix: true)
      // S<S<String>>.c
      $0.addStructComponent(offset: S<S<String>>.c_offset,
                            forceOverflow: true,
                            endsReferencePrefix: true)
      $0.addType(C<S<String>>.self)
      // C<S<String>>.z
      $0.addClassComponent(offset: C<S<String>>.z_offset,
                           forceOverflow: true)
      $0.addType(S<String>.self)
      // S<String>.p
      $0.addStructComponent(offset: S<String>.p_offset,
                            forceOverflow: true)
      $0.addType(Point.self)
      // Point.x
      $0.addStructComponent(offset: Point.x_offset,
                            forceOverflow: true)
    }

  expectEqual(s_c_z_p_x, s_c_z_p_x_3)
  expectEqual(s_c_z_p_x.hashValue, s_c_z_p_x_3.hashValue)

  expectEqual(s_c_z_p_x_3, s_c_z_p_x)
  expectEqual(s_c_z_p_x_3.hashValue, s_c_z_p_x.hashValue)

  // Same path type, different suffixes
  let s_c_z_p_y = ReferenceWritableKeyPath<S<S<String>>, Double>
    .build(capacityInBytes: 7 * MemoryLayout<Int>.size + 4) {
      $0.addHeader(trivial: true, hasReferencePrefix: true)
      // S<S<String>>.c
      $0.addStructComponent(offset: S<S<String>>.c_offset,
                            endsReferencePrefix: true)
      $0.addType(C<S<String>>.self)
      // C<S<String>>.z
      $0.addClassComponent(offset: C<S<String>>.z_offset)
      $0.addType(S<String>.self)
      // S<String>.p
      $0.addStructComponent(offset: S<String>.p_offset)
      $0.addType(Point.self)
      // Point.y
      $0.addStructComponent(offset: Point.y_offset)
    }

  expectNotEqual(s_c_z_p_x, s_c_z_p_y)
  expectNotEqual(s_c_z_p_y, s_c_z_p_x)

  // Different path type
  let s_c_z_p = ReferenceWritableKeyPath<S<S<String>>, Point>
    .build(capacityInBytes: 5 * MemoryLayout<Int>.size + 4) {
      $0.addHeader(trivial: true, hasReferencePrefix: true)
      // S<S<String>>.c
      $0.addStructComponent(offset: S<S<String>>.c_offset,
                            endsReferencePrefix: true)
      $0.addType(C<S<String>>.self)
      // C<S<String>>.z
      $0.addClassComponent(offset: C<S<String>>.z_offset)
      $0.addType(S<String>.self)
      // S<String>.p
      $0.addStructComponent(offset: S<String>.p_offset)
    }

  expectNotEqual(s_c_z_p_x, s_c_z_p)
  expectNotEqual(s_c_z_p, s_c_z_p_x)

  // Same path, no reference prefix
  let s_c_z_p_x_readonly = KeyPath<S<S<String>>, Double>
    .build(capacityInBytes: 7 * MemoryLayout<Int>.size + 4) {
      $0.addHeader(trivial: true, hasReferencePrefix: false)
      // S<S<String>>.c
      $0.addStructComponent(offset: S<S<String>>.c_offset)
      $0.addType(C<S<String>>.self)
      // C<S<String>>.z
      $0.addClassComponent(offset: C<S<String>>.z_offset)
      $0.addType(S<String>.self)
      // S<String>.p
      $0.addStructComponent(offset: S<String>.p_offset)
      $0.addType(Point.self)
      // Point.x
      $0.addStructComponent(offset: Point.x_offset)
    }

  expectNotEqual(s_c_z_p_x, s_c_z_p_x_readonly)
  expectNotEqual(s_c_z_p_x_readonly, s_c_z_p_x)

  // Same path type, different paths
  let s_p_y_readonly = KeyPath<S<S<String>>, Double>
    .build(capacityInBytes: 3 * MemoryLayout<Int>.size + 4) {
      $0.addHeader(trivial: true, hasReferencePrefix: false)
      // S<S<String>>.p
      $0.addStructComponent(offset: S<S<String>>.p_offset)
      $0.addType(Point.self)
      // Point.y
      $0.addStructComponent(offset: Point.y_offset)
    }

  expectNotEqual(s_p_y_readonly, s_c_z_p_x_readonly)
  expectNotEqual(s_c_z_p_x_readonly, s_p_y_readonly)

  let o_o_o_o = ReferenceWritableKeyPath<Oroborous, Oroborous>
    .build(capacityInBytes: 5 * MemoryLayout<Int>.size + 4) {
      $0.addHeader(trivial: true, hasReferencePrefix: false)
      // O.o
      $0.addClassComponent(offset: classHeaderSize)
      $0.addType(Oroborous.self)
      // O.o
      $0.addClassComponent(offset: classHeaderSize)
      $0.addType(Oroborous.self)
      // O.o
      $0.addClassComponent(offset: classHeaderSize)
    }

  // Different reference prefix length
  let o_o_o_o_rp1 = ReferenceWritableKeyPath<Oroborous, Oroborous>
    .build(capacityInBytes: 5 * MemoryLayout<Int>.size + 4) {
      $0.addHeader(trivial: true, hasReferencePrefix: true)
      // O.o
      $0.addClassComponent(offset: classHeaderSize,
                           endsReferencePrefix: true)
      $0.addType(Oroborous.self)
      // O.o
      $0.addClassComponent(offset: classHeaderSize)
      $0.addType(Oroborous.self)
      // O.o
      $0.addClassComponent(offset: classHeaderSize)
    }
  let o_o_o_o_rp2 = ReferenceWritableKeyPath<Oroborous, Oroborous>
    .build(capacityInBytes: 5 * MemoryLayout<Int>.size + 4) {
      $0.addHeader(trivial: true, hasReferencePrefix: true)
      // O.o
      $0.addClassComponent(offset: classHeaderSize)
      $0.addType(Oroborous.self)
      // O.o
      $0.addClassComponent(offset: classHeaderSize,
                           endsReferencePrefix: true)
      $0.addType(Oroborous.self)
      // O.o
      $0.addClassComponent(offset: classHeaderSize)
    }
  let o_o_o_o_rp2_2 = ReferenceWritableKeyPath<Oroborous, Oroborous>
    .build(capacityInBytes: 5 * MemoryLayout<Int>.size + 4) {
      $0.addHeader(trivial: true, hasReferencePrefix: true)
      // O.o
      $0.addClassComponent(offset: classHeaderSize)
      $0.addType(Oroborous.self)
      // O.o
      $0.addClassComponent(offset: classHeaderSize,
                           endsReferencePrefix: true)
      $0.addType(Oroborous.self)
      // O.o
      $0.addClassComponent(offset: classHeaderSize)
    }

  expectNotEqual(o_o_o_o, o_o_o_o_rp1)
  expectNotEqual(o_o_o_o_rp1, o_o_o_o)

  expectNotEqual(o_o_o_o_rp1, o_o_o_o_rp2)
  expectNotEqual(o_o_o_o_rp2, o_o_o_o_rp1)

  expectNotEqual(o_o_o_o, o_o_o_o_rp2)
  expectNotEqual(o_o_o_o_rp2, o_o_o_o)
  
  expectEqual(o_o_o_o_rp2, o_o_o_o_rp2_2)
  expectEqual(o_o_o_o_rp2_2, o_o_o_o_rp2)
  expectEqual(o_o_o_o_rp2.hashValue, o_o_o_o_rp2_2.hashValue)

  // Same type, different length of components with same prefix
  let o_o_o = ReferenceWritableKeyPath<Oroborous, Oroborous>
    .build(capacityInBytes: 3 * MemoryLayout<Int>.size + 4) {
      $0.addHeader(trivial: true, hasReferencePrefix: false)
      // O.o
      $0.addClassComponent(offset: classHeaderSize)
      $0.addType(Oroborous.self)
      // O.o
      $0.addClassComponent(offset: classHeaderSize)
    }

  expectNotEqual(o_o_o, o_o_o_o)
  expectNotEqual(o_o_o_o, o_o_o)
}

keyPathImpl.test("appending") {
  let s_p = WritableKeyPath<S<String>, Point>
    .build(capacityInBytes: MemoryLayout<Int>.size + 4) {
      $0.addHeader(trivial: true, hasReferencePrefix: false)
      $0.addStructComponent(offset: S<String>.p_offset)
    }
  let p_y = WritableKeyPath<Point, Double>
    .build(capacityInBytes: MemoryLayout<Int>.size + 4) {
      $0.addHeader(trivial: true, hasReferencePrefix: false)
      $0.addStructComponent(offset: Point.y_offset)
    }
  
  let s_p_y = s_p.appending(path: p_y)

  let c = C(x: 679, y: nil, z: "buffalo\("")")
  var value = S(x: 1738, y: nil, z: "bottles of beer\("")",
                p: .init(x: 0.5, y: -0.5), c: c)
  
  expectEqual(value[keyPath: s_p_y], -0.5)
  value[keyPath: s_p_y] = 4.0
  expectEqual(value[keyPath: s_p_y], 4.0)
  expectEqual(value.p.x, 0.5)
  expectEqual(value.p.y, 4.0)

  let s_p_y2 = s_p.appending(path: p_y)
  expectEqual(s_p_y, s_p_y2)
  expectEqual(s_p_y2, s_p_y)
  expectEqual(s_p_y.hashValue, s_p_y2.hashValue)
  
  let s_p_y_manual = WritableKeyPath<S<String>, Double>
    .build(capacityInBytes: 3 * MemoryLayout<Int>.size + 4) {
      $0.addHeader(trivial: true, hasReferencePrefix: false)
      $0.addStructComponent(offset: S<String>.p_offset)
      $0.addType(Point.self)
      $0.addStructComponent(offset: Point.y_offset)
    }
  expectEqual(s_p_y, s_p_y_manual)
  expectEqual(s_p_y_manual, s_p_y)
  expectEqual(s_p_y.hashValue, s_p_y_manual.hashValue)
  
  let c_z = ReferenceWritableKeyPath<C<S<String>>, S<String>>
    .build(capacityInBytes: MemoryLayout<Int>.size + 4) {
      $0.addHeader(trivial: true, hasReferencePrefix: false)
      $0.addClassComponent(offset: C<S<String>>.z_offset)
    }
  
  let value2 = C(x: 17, y: LifetimeTracked(38), z: value)
  
  let c_z_p_y = c_z.appending(path: s_p_y)
  
  expectEqual(value2[keyPath: c_z_p_y], 4.0)
  value2[keyPath: c_z_p_y] = 5.0
  expectEqual(value2[keyPath: c_z_p_y], 5.0)
  expectEqual(value2.z.p.y, 5.0)
  expectEqual(value2.z.p.x, 0.5)
  
  let c_z_p_y_manual = ReferenceWritableKeyPath<C<S<String>>, Double>
    .build(capacityInBytes: 5 * MemoryLayout<Int>.size + 4) {
      $0.addHeader(trivial: true, hasReferencePrefix: false)
      $0.addClassComponent(offset: C<S<String>>.z_offset)
      $0.addType(S<String>.self)
      $0.addStructComponent(offset: S<String>.p_offset)
      $0.addType(Point.self)
      $0.addStructComponent(offset: Point.y_offset)
    }
  
  expectEqual(c_z_p_y, c_z_p_y_manual)
  expectEqual(c_z_p_y_manual, c_z_p_y)
  expectEqual(c_z_p_y.hashValue, c_z_p_y_manual.hashValue)
  
  let s_c = WritableKeyPath<S<S<String>>, C<S<String>>>
    .build(capacityInBytes: MemoryLayout<Int>.size + 4) {
      $0.addHeader(trivial: true, hasReferencePrefix: false)
      $0.addStructComponent(offset: S<S<String>>.c_offset)
    }
  
  let s_c_z_p_y = s_c.appending(path: c_z_p_y)
  
  let value3 = S(x: 679, y: nil, z: value,
                 p: value.p, c: value2)
  expectEqual(value3[keyPath: s_c_z_p_y], 5.0)
  value3[keyPath: s_c_z_p_y] = 11.0
  expectEqual(value3[keyPath: s_c_z_p_y], 11.0)
  expectEqual(value2[keyPath: c_z_p_y], 11.0)
  
  let s_c_z_p_y_manual = ReferenceWritableKeyPath<S<S<String>>, Double>
    .build(capacityInBytes: 7 * MemoryLayout<Int>.size + 4) {
      $0.addHeader(trivial: true, hasReferencePrefix: true)
      $0.addStructComponent(offset: S<S<String>>.c_offset,
                            endsReferencePrefix: true)
      $0.addType(C<S<String>>.self)
      $0.addClassComponent(offset: C<S<String>>.z_offset)
      $0.addType(S<String>.self)
      $0.addStructComponent(offset: S<String>.p_offset)
      $0.addType(Point.self)
      $0.addStructComponent(offset: Point.y_offset)
    }
  
  expectEqual(s_c_z_p_y, s_c_z_p_y_manual)
  expectEqual(s_c_z_p_y_manual, s_c_z_p_y)
  expectEqual(s_c_z_p_y_manual.hashValue, s_c_z_p_y.hashValue)
  
  typealias CP = CratePair<S<S<String>>, Int>
  let cratePair_left_value = ReferenceWritableKeyPath<CP, S<S<String>>>
    .build(capacityInBytes: 3 * MemoryLayout<Int>.size + 4) {
      $0.addHeader(trivial: true, hasReferencePrefix: true)
      $0.addStructComponent(offset: CratePair<S<S<String>>, Int>.left_offset,
                            endsReferencePrefix: true)
      $0.addType(Crate<S<S<String>>>.self)
      $0.addClassComponent(offset: Crate<S<S<String>>>.value_offset)
    }
  
  let cratePair_left_value_c_z_p_y
    = cratePair_left_value.appending(path: s_c_z_p_y)
  
  let crate1 = Crate(value: value3)
  let crate2 = Crate(value: 9)
  let cratePair = CratePair(left: crate1, right: crate2)
  expectEqual(cratePair[keyPath: cratePair_left_value_c_z_p_y], 11.0)
  cratePair[keyPath: cratePair_left_value_c_z_p_y] = 13.0
  expectEqual(cratePair[keyPath: cratePair_left_value_c_z_p_y], 13.0)
  expectEqual(value3[keyPath: s_c_z_p_y], 13.0)
  expectEqual(value2[keyPath: c_z_p_y], 13.0)

  let cratePair_left_value_c_z_p_y_manual
    = ReferenceWritableKeyPath<CP, Double>
      .build(capacityInBytes: 11 * MemoryLayout<Int>.size + 4) {
        $0.addHeader(trivial: true, hasReferencePrefix: true)
        $0.addStructComponent(offset: CP.left_offset)
        $0.addType(Crate<S<S<String>>>.self)
        $0.addClassComponent(offset: Crate<S<S<String>>>.value_offset)
        $0.addType(S<S<String>>.self)
        $0.addStructComponent(offset: S<S<String>>.c_offset,
                              endsReferencePrefix: true)
        $0.addType(C<S<String>>.self)
        $0.addClassComponent(offset: C<S<String>>.z_offset)
        $0.addType(S<String>.self)
        $0.addStructComponent(offset: S<String>.p_offset)
        $0.addType(Point.self)
        $0.addStructComponent(offset: Point.y_offset)
      }
  expectEqual(cratePair_left_value_c_z_p_y,
              cratePair_left_value_c_z_p_y_manual)
  expectEqual(cratePair_left_value_c_z_p_y_manual,
              cratePair_left_value_c_z_p_y)
  expectEqual(cratePair_left_value_c_z_p_y_manual.hashValue,
              cratePair_left_value_c_z_p_y.hashValue)
}

runAllTests()

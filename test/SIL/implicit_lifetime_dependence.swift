// RUN: %target-swift-frontend %s \
// RUN: -emit-sil  -disable-availability-checking \
// RUN: -enable-experimental-feature NonescapableTypes \
// RUN: -disable-experimental-parser-round-trip \
// RUN: | %FileCheck %s
// FIXME: Remove '-disable-experimental-parser-round-trip' (rdar://137636751).
// REQUIRES: asserts

struct BufferView : ~Escapable {
  let ptr: UnsafeRawBufferPointer
  let c: Int
  @lifetime(borrow ptr)
  init(_ ptr: UnsafeRawBufferPointer, _ c: Int) {
    self.ptr = ptr
    self.c = c
  }
  @_unsafeNonescapableResult
  init(independent ptr: UnsafeRawBufferPointer, _ c: Int) {
    self.ptr = ptr
    self.c = c
  }
// CHECK-LABEL: sil hidden @$s28implicit_lifetime_dependence10BufferViewVyA2ChcfC : $@convention(method) (@guaranteed BufferView, @thin BufferView.Type) -> @lifetime(copy 0) @owned BufferView {
  init(_ otherBV: borrowing BufferView) {
    self.ptr = otherBV.ptr
    self.c = otherBV.c
  }
// CHECK-LABEL: sil hidden @$s28implicit_lifetime_dependence10BufferViewVyA2CcfC : $@convention(method) (@owned BufferView, @thin BufferView.Type) -> @lifetime(copy 0) @owned BufferView {
  init(_ otherBV: consuming BufferView) {
    self.ptr = otherBV.ptr
    self.c = otherBV.c
  }
// CHECK-LABEL: sil hidden @$s28implicit_lifetime_dependence10BufferViewVyACSW_SaySiGhtcfC : $@convention(method) (UnsafeRawBufferPointer, @guaranteed Array<Int>, @thin BufferView.Type) -> @lifetime(borrow 1) @owned BufferView {
  init(_ ptr: UnsafeRawBufferPointer, _ a: borrowing Array<Int>) {
    self.ptr = ptr
    self.c = a.count
  }
}

struct MutableBufferView : ~Escapable, ~Copyable {
  let ptr: UnsafeMutableRawBufferPointer
  let c: Int
  @lifetime(borrow ptr)
  init(_ ptr: UnsafeMutableRawBufferPointer, _ c: Int) {
    self.ptr = ptr
    self.c = c
  }
}

func testBasic() {
  let a = [Int](repeating: 0, count: 4)
  a.withUnsafeBytes {
    let view = BufferView($0, a.count)
    let derivedView = derive(view)
    let newView = consumeAndCreate(derivedView)
    use(newView)    
  }
}

// CHECK-LABEL: sil hidden @$s28implicit_lifetime_dependence6deriveyAA10BufferViewVADF : $@convention(thin) (@guaranteed BufferView) -> @lifetime(copy 0)  @owned BufferView {
func derive(_ x: borrowing BufferView) -> BufferView {
  return BufferView(x.ptr, x.c)
}

func derive(_ unused: Int, _ x: borrowing BufferView) -> BufferView {
  return BufferView(independent: x.ptr, x.c)
}

// CHECK-LABEL: sil hidden @$s28implicit_lifetime_dependence16consumeAndCreateyAA10BufferViewVADnF : $@convention(thin) (@owned BufferView) -> @lifetime(copy 0)  @owned BufferView {
func consumeAndCreate(_ x: consuming BufferView) -> BufferView {
  return BufferView(independent: x.ptr, x.c)
}

func use(_ x: borrowing BufferView) {}

struct Wrapper : ~Escapable {
  var _view: BufferView
  var view: BufferView {
// CHECK: sil hidden @$s28implicit_lifetime_dependence7WrapperV4viewAA10BufferViewVvr : $@yield_once @convention(method) (@guaranteed Wrapper) -> @lifetime(copy 0) @yields @guaranteed BufferView {
    _read {
      yield _view
    }
// CHECK: sil hidden @$s28implicit_lifetime_dependence7WrapperV4viewAA10BufferViewVvM : $@yield_once @convention(method) (@inout Wrapper) -> @lifetime(copy 0) @yields @inout BufferView {
    _modify {
      yield &_view
    }
  }
// CHECK-LABEL: sil hidden @$s28implicit_lifetime_dependence7WrapperVyAcA10BufferViewVcfC : $@convention(method) (@owned BufferView, @thin Wrapper.Type) -> @lifetime(copy 0) @owned Wrapper {
  init(_ view: consuming BufferView) {
    self._view = view
  }
// CHECK-LABEL: sil hidden @$s28implicit_lifetime_dependence7WrapperV8getView1AA10BufferViewVyKF : $@convention(method) (@guaranteed Wrapper) -> @lifetime(copy 0)  (@owned BufferView, @error any Error) {
  borrowing func getView1() throws -> BufferView {
    return _view
  }

// CHECK-LABEL: sil hidden @$s28implicit_lifetime_dependence7WrapperV8getView2AA10BufferViewVyYaKF : $@convention(method) @async (@owned Wrapper) -> @lifetime(copy 0)  (@owned BufferView, @error any Error) {
  consuming func getView2() async throws -> BufferView {
    return _view
  }
}

struct Container1 : ~Copyable {
  let ptr: UnsafeRawBufferPointer
  let c: Int
// CHECK: sil hidden @$s28implicit_lifetime_dependence10Container1V4viewAA10BufferViewVvg : $@convention(method) (@guaranteed Container1) -> @lifetime(borrow 0) @owned BufferView {
  var view: BufferView {
    get {
      return BufferView(ptr, c)
    }
  }
}

struct Container2 : ~Copyable {
  let ptr: UnsafeMutableRawBufferPointer
  let c: Int
// CHECK: sil hidden @$s28implicit_lifetime_dependence10Container2V11mutableViewAA013MutableBufferF0Vvr : $@yield_once @convention(method) (@guaranteed Container2) -> @lifetime(borrow 0) @yields @guaranteed MutableBufferView {
  var mutableView: MutableBufferView {
    _read {
      yield MutableBufferView(ptr, c)
    }
  }
}


struct FakeRange<Bound> {
  public let lowerBound: Bound
  public let upperBound: Bound
}

struct GenericBufferView<Element> : ~Escapable {
  typealias Index = Int
  typealias Pointer = UnsafeRawPointer

  let baseAddress: Pointer
  let count: Int

// CHECK-LABEL: sil hidden @$s28implicit_lifetime_dependence17GenericBufferViewV11baseAddress5countACyxGSV_SitcfC : $@convention(method) <Element> (UnsafeRawPointer, Int, @thin GenericBufferView<Element>.Type) -> @lifetime(borrow 0) @owned GenericBufferView<Element> {
  init<Storage>(baseAddress: Pointer,
                count: Int,
                dependsOn: borrowing Storage) {
    self = GenericBufferView<Element>(baseAddress: baseAddress,
                                      count: count)
  }
  // unsafe private API
  @lifetime(borrow baseAddress)
  init(baseAddress: Pointer, count: Int) {
    precondition(count >= 0, "Count must not be negative")
    self.baseAddress = baseAddress
    self.count = count
  } 
  subscript(position: Pointer) -> Element {
    get {
      if _isPOD(Element.self) {
        return position.loadUnaligned(as: Element.self)
      }
      else {
        return position.load(as: Element.self)
      }
    }
  }
// CHECK: sil hidden @$s28implicit_lifetime_dependence17GenericBufferViewVyACyxGAA9FakeRangeVySVGcig : $@convention(method) <Element> (FakeRange<UnsafeRawPointer>, @guaranteed GenericBufferView<Element>) -> @lifetime(copy 1) @owned GenericBufferView<Element> {
  subscript(bounds: FakeRange<Pointer>) -> Self {
    get {
      GenericBufferView(
        baseAddress: UnsafeRawPointer(bounds.lowerBound),
        count: bounds.upperBound.distance(to:bounds.lowerBound) / MemoryLayout<Element>.stride
      )
    }
  }
}

// CHECK-LABEL: sil hidden @$s28implicit_lifetime_dependence23tupleLifetimeDependenceyAA10BufferViewV_ADtADF : $@convention(thin) (@guaranteed BufferView) -> @lifetime(copy 0)  (@owned BufferView, @owned BufferView) {
func tupleLifetimeDependence(_ x: borrowing BufferView) -> (BufferView, BufferView) {
  return (BufferView(x.ptr, x.c), BufferView(x.ptr, x.c))
}

public struct OuterNE: ~Escapable {
  // A public property generates an implicit setter with an infered dependence on 'newValue'.
  //
  // [inner1.setter]
  // CHECK-LABEL: sil [transparent] @$s28implicit_lifetime_dependence7OuterNEV6inner1AC05InnerE0Vvs : $@convention(method) (@owned OuterNE.InnerNE, @lifetime(copy 0) @inout OuterNE) -> () {
  public var inner1: InnerNE

  // Explicit setter with an infered dependence on 'newValue'.
  public var inner2: InnerNE {
    get { inner1 }
    set { inner1 = newValue }
  }

  public struct InnerNE: ~Escapable {
    init<Owner: ~Escapable & ~Copyable>(
      owner: borrowing Owner
    ) {}
  }

  init<Owner: ~Copyable & ~Escapable>(owner: borrowing Owner) {
    self.inner1 = InnerNE(owner: owner)
  }

  // Infer a dependence from 'self' on 'value'. We might revoke this rule once we have syntax.
  //
  // CHECK-LABEL: sil hidden @$s28implicit_lifetime_dependence7OuterNEV8setInner5valueyAC0gE0V_tF : $@convention(method) (@guaranteed OuterNE.InnerNE, @lifetime(copy 0) @inout OuterNE) -> () {
  mutating func setInner(value: InnerNE) {
    self.inner1 = value
  }
}

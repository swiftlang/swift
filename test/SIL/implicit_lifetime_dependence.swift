// RUN: %target-swift-frontend %s \
// RUN: -emit-sil  -disable-availability-checking \
// RUN: -enable-experimental-feature NonescapableTypes | %FileCheck %s
// REQUIRES: asserts

struct BufferView : ~Escapable {
  let ptr: UnsafeRawBufferPointer
  let c: Int
  init(_ ptr: UnsafeRawBufferPointer, _ c: Int) -> dependsOn(ptr) Self {
    self.ptr = ptr
    self.c = c
  }
  @_unsafeNonescapableResult
  init(independent ptr: UnsafeRawBufferPointer, _ c: Int) {
    self.ptr = ptr
    self.c = c
  }
// CHECK-LABEL: sil hidden @$s28implicit_lifetime_dependence10BufferViewVyACYliSU_AChcfC : $@convention(method) (@guaranteed BufferView, @thin BufferView.Type) -> _inherit(0)  @owned BufferView {
  init(_ otherBV: borrowing BufferView) {
    self.ptr = otherBV.ptr
    self.c = otherBV.c
  }
// CHECK-LABEL: sil hidden @$s28implicit_lifetime_dependence10BufferViewVyACYliSU_ACcfC : $@convention(method) (@owned BufferView, @thin BufferView.Type) -> _inherit(0)  @owned BufferView {
  init(_ otherBV: consuming BufferView) {
    self.ptr = otherBV.ptr
    self.c = otherBV.c
  }
// CHECK-LABEL: sil hidden @$s28implicit_lifetime_dependence10BufferViewVyACYlsUSU_SW_SaySiGhtcfC : $@convention(method) (UnsafeRawBufferPointer, @guaranteed Array<Int>, @thin BufferView.Type) -> _scope(1)  @owned BufferView {
  init(_ ptr: UnsafeRawBufferPointer, _ a: borrowing Array<Int>) {
    self.ptr = ptr
    self.c = a.count
  }
}

struct MutableBufferView : ~Escapable, ~Copyable {
  let ptr: UnsafeMutableRawBufferPointer
  let c: Int
  init(_ ptr: UnsafeMutableRawBufferPointer, _ c: Int) -> dependsOn(ptr) Self {
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

// CHECK-LABEL: sil hidden @$s28implicit_lifetime_dependence6deriveyAA10BufferViewVYliS_ADF : $@convention(thin) (@guaranteed BufferView) -> _inherit(0)  @owned BufferView {
func derive(_ x: borrowing BufferView) -> BufferView {
  return BufferView(x.ptr, x.c)
}

func derive(_ unused: Int, _ x: borrowing BufferView) -> BufferView {
  return BufferView(independent: x.ptr, x.c)
}

// CHECK-LABEL: sil hidden @$s28implicit_lifetime_dependence16consumeAndCreateyAA10BufferViewVYliS_ADnF : $@convention(thin) (@owned BufferView) -> _inherit(0)  @owned BufferView {
func consumeAndCreate(_ x: consuming BufferView) -> BufferView {
  return BufferView(independent: x.ptr, x.c)
}

func use(_ x: borrowing BufferView) {}

struct Wrapper : ~Escapable {
  var _view: BufferView
  var view: BufferView {
// CHECK: sil hidden @$s28implicit_lifetime_dependence7WrapperV4viewAA10BufferViewVvr : $@yield_once @convention(method) (@guaranteed Wrapper) -> _inherit(0) @yields @guaranteed BufferView {
    _read {
      yield _view
    }
// CHECK: sil hidden @$s28implicit_lifetime_dependence7WrapperV4viewAA10BufferViewVvM : $@yield_once @convention(method) (@inout Wrapper) -> _inherit(0) @yields @inout BufferView {
    _modify {
      yield &_view
    }
  }
// CHECK-LABEL: sil hidden @$s28implicit_lifetime_dependence7WrapperVyACYliSU_AA10BufferViewVcfC : $@convention(method) (@owned BufferView, @thin Wrapper.Type) -> _inherit(0)  @owned Wrapper {
  init(_ view: consuming BufferView) {
    self._view = view
  }
// CHECK-LABEL: sil hidden @$s28implicit_lifetime_dependence7WrapperV8getView1AA10BufferViewVYliS_yKF : $@convention(method) (@guaranteed Wrapper) -> _inherit(0)  (@owned BufferView, @error any Error) {
  borrowing func getView1() throws -> BufferView {
    return _view
  }

// CHECK-LABEL: sil hidden @$s28implicit_lifetime_dependence7WrapperV8getView2AA10BufferViewVYliS_yYaKF : $@convention(method) @async (@owned Wrapper) -> _inherit(0)  (@owned BufferView, @error any Error) {
  consuming func getView2() async throws -> BufferView {
    return _view
  }
}

struct Container1 : ~Copyable {
  let ptr: UnsafeRawBufferPointer
  let c: Int
// CHECK: sil hidden @$s28implicit_lifetime_dependence10Container1V4viewAA10BufferViewVvg : $@convention(method) (@guaranteed Container1) -> _scope(0) @owned BufferView {
  var view: BufferView {
    get {
      return BufferView(ptr, c)
    }
  }
}

struct Container2 : ~Copyable {
  let ptr: UnsafeMutableRawBufferPointer
  let c: Int
// CHECK: sil hidden @$s28implicit_lifetime_dependence10Container2V11mutableViewAA013MutableBufferF0Vvr : $@yield_once @convention(method) (@guaranteed Container2) -> _scope(0) @yields @guaranteed MutableBufferView {
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

// CHECK-LABEL: sil hidden @$s28implicit_lifetime_dependence17GenericBufferViewV11baseAddress5count9dependsOnACyxGYlsUUSU_SV_Siqd__htclufC : $@convention(method) <Element><Storage> (UnsafeRawPointer, Int, @in_guaranteed Storage, @thin GenericBufferView<Element>.Type) -> _scope(2)  @owned GenericBufferView<Element> {
  init<Storage>(baseAddress: Pointer,
                count: Int,
                dependsOn: borrowing Storage) {
    self = GenericBufferView<Element>(baseAddress: baseAddress,
                                      count: count)
  }
  // unsafe private API
  init(baseAddress: Pointer, count: Int) -> dependsOn(baseAddress) Self {
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
// CHECK: sil hidden @$s28implicit_lifetime_dependence17GenericBufferViewVyACyxGAA9FakeRangeVySVGcig : $@convention(method) <Element> (FakeRange<UnsafeRawPointer>, @guaranteed GenericBufferView<Element>) -> _inherit(1) @owned GenericBufferView<Element> {
  subscript(bounds: FakeRange<Pointer>) -> Self {
    get {
      GenericBufferView(
        baseAddress: UnsafeRawPointer(bounds.lowerBound),
        count: bounds.upperBound.distance(to:bounds.lowerBound) / MemoryLayout<Element>.stride
      )
    }
  }
}

// CHECK-LABEL: sil hidden @$s28implicit_lifetime_dependence23tupleLifetimeDependenceyAA10BufferViewV_ADtYliS_ADF : $@convention(thin) (@guaranteed BufferView) -> _inherit(0)  (@owned BufferView, @owned BufferView) {
func tupleLifetimeDependence(_ x: borrowing BufferView) -> (BufferView, BufferView) {
  return (BufferView(x.ptr, x.c), BufferView(x.ptr, x.c))
}

public struct OuterNE: ~Escapable {
  // A public property generates an implicit setter with an infered dependence on 'newValue'.
  //
  // [inner1.setter]
  // CHECK-LABEL: sil [transparent] @$s28implicit_lifetime_dependence7OuterNEV6inner1AC05InnerE0Vvs : $@convention(method) (@owned OuterNE.InnerNE, _inherit(0) @inout OuterNE) -> () {
  public var inner1: InnerNE

  // Explicit setter with an infered dependence on 'newValue'.
  public var inner2: InnerNE {
    get { inner1 }
    set { inner1 = newValue }
  }

  public struct InnerNE: ~Escapable {
    init<Owner: ~Escapable & ~Copyable>(
      owner: borrowing Owner
    ) -> dependsOn(owner) Self {}
  }

  init<Owner: ~Copyable & ~Escapable>(owner: borrowing Owner) -> dependsOn(owner) Self {
    self.inner1 = InnerNE(owner: owner)
  }

  // Infer a dependence from 'self' on 'value'. We might revoke this rule once we have dependsOn(self:) syntax.
  //
  // CHECK-LABEL: sil hidden @$s28implicit_lifetime_dependence7OuterNEV8setInner5valueyAC0gE0V_tF : $@convention(method) (@guaranteed OuterNE.InnerNE, _inherit(0) @inout OuterNE) -> () {
  mutating func setInner(value: InnerNE) {
    self.inner1 = value
  }
}

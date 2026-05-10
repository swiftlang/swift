// RUN: %target-swift-emit-sil -enable-experimental-feature Lifetimes -primary-file %s | %FileCheck %s

// REQUIRES: swift_feature_Lifetimes

struct NE : ~Escapable {
  let _p: UnsafeRawPointer

  @_lifetime(borrow p)
  init(_ p: UnsafeRawPointer) { self._p = p }
}

struct NCNE : ~Copyable, ~Escapable {
  let _p: UnsafeRawPointer

  @_lifetime(borrow p)
  init(_ p: UnsafeRawPointer) {
    self._p = p
  }
}

struct NEStoredProp : ~Escapable {
  var ne: NE
  // NEStoredProp.ne.getter
  // CHECK-LABEL: sil hidden [transparent] @$s9accessors12NEStoredPropV2neAA2NEVvg : $@convention(method) (@guaranteed NEStoredProp) -> @lifetime(copy 0) @owned NE {

  // NEStoredProp.ne.setter
  // CHECK-LABEL: sil hidden [transparent] @$s9accessors12NEStoredPropV2neAA2NEVvs : $@convention(method) (@owned NE, @lifetime(copy 0, copy 1) @inout NEStoredProp) -> () {

  // NEStoredProp.ne.modify
  // CHECK-LABEL: sil hidden [transparent] @$s9accessors12NEStoredPropV2neAA2NEVvM : $@yield_once @convention(method) (@lifetime(copy 0) @inout NEStoredProp) -> @lifetime(copy 0) @yields @inout NE {

  // NEStoredProp.init(ne:)
  // CHECK-LABEL: sil hidden @$s9accessors12NEStoredPropV2neAcA2NEV_tcfC : $@convention(method) (@owned NE, @thin NEStoredProp.Type) -> @lifetime(copy 0) @owned NEStoredProp {
}

struct NCNEStoredProp : ~Copyable & ~Escapable {
  var ncne: NCNE
  // NCNEStoredProp.ncne.read
  // CHECK-LABEL: sil hidden [transparent] @$s9accessors14NCNEStoredPropV4ncneAA4NCNEVvr : $@yield_once @convention(method) (@guaranteed NCNEStoredProp) -> @lifetime(copy 0) @yields @guaranteed NCNE {

  // NCNEStoredProp.ncne.setter
  // CHECK-LABEL: sil hidden [transparent] @$s9accessors14NCNEStoredPropV4ncneAA4NCNEVvs : $@convention(method) (@owned NCNE, @lifetime(copy 0, copy 1) @inout NCNEStoredProp) -> () {

  // NCNEStoredProp.ncne.modify
  // CHECK-LABEL: sil hidden [transparent] @$s9accessors14NCNEStoredPropV4ncneAA4NCNEVvM : $@yield_once @convention(method) (@lifetime(copy 0) @inout NCNEStoredProp) -> @lifetime(copy 0) @yields @inout NCNE {

  // NCNEStoredProp.init(ncne:)
  // CHECK-LABEL: sil hidden @$s9accessors14NCNEStoredPropV4ncneAcA4NCNEV_tcfC : $@convention(method) (@owned NCNE, @thin NCNEStoredProp.Type) -> @lifetime(copy 0) @owned NCNEStoredProp {
}

struct NCNEHolder : ~Copyable, ~Escapable {
  var p: UnsafeRawPointer

  // this cannot be public in order to synthesize the _read accessor.
  var ncneBorrow: NCNE {
    // CHECK-LABEL: sil{{.*}} @$s9accessors10NCNEHolderV10ncneBorrowAA4NCNEVvr : $@yield_once @convention(method) (@guaranteed NCNEHolder) -> @lifetime(borrow 0) @yields @guaranteed NCNE {
    @_lifetime(borrow self)
    borrowing get {
      _overrideLifetime(NCNE(p), borrowing: self)
    }
    // CHECK-LABEL: sil{{.*}} @$s9accessors10NCNEHolderV10ncneBorrowAA4NCNEVvM : $@yield_once @convention(method) (@lifetime(copy 0) @inout NCNEHolder) -> @lifetime(borrow 0) @yields @inout NCNE {
    @_lifetime(self: copy self)
    set {
      p = newValue._p
    }
  }

  // this cannot be public in order to synthesize the _read accessor.
  var ncneCopy: NCNE {
    // CHECK-LABEL: sil{{.*}} @$s9accessors10NCNEHolderV8ncneCopyAA4NCNEVvr : $@yield_once @convention(method) (@guaranteed NCNEHolder) -> @lifetime(copy 0) @yields @guaranteed NCNE {
    @_lifetime(copy self)
    borrowing get {
      _overrideLifetime(NCNE(p), copying: self)
    }
    // CHECK-LABEL: sil{{.*}} @$s9accessors10NCNEHolderV8ncneCopyAA4NCNEVvM : $@yield_once @convention(method) (@lifetime(copy 0) @inout NCNEHolder) -> @lifetime(copy 0) @yields @inout NCNE {
    @_lifetime(self: copy self)
    set {
      p = newValue._p
    }
  }
}

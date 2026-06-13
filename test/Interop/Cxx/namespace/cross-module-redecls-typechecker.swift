// RUN: rm -rf %t
// RUN: split-file %s %t
// RUN: %target-swift-frontend -typecheck -verify -suppress-notes \
// RUN:   -cxx-interoperability-mode=default \
// RUN:   -I %t%{fs-sep}Inputs %t%{fs-sep}importer.swift

//--- Inputs/module.modulemap
module ModuleA {
  header "headerA.h"
  requires cplusplus
}

module ModuleB {
  header "headerB.h"
  requires cplusplus
}

//--- Inputs/headerA.h
namespace NS {
  struct SA { using FromA = int; };

  struct Incomplete;

  struct XSB;
  struct XSA {
    using FromA = int;
    using Other = XSB;
    using NonCanon = Incomplete *;
  };

  namespace NNS {
    struct NSA { using FromA = int; };
    struct NXSB;
    struct NXSA {
      using FromA = int;
      using Other = NXSB;
    };
  }

  inline namespace INS {
    struct ISA { using FromA = int; };
    struct ISB;
    struct ISX { using FromBoth = int; };
  }
}

//--- Inputs/headerB.h
namespace NS {
  struct SB { using FromB = int; };

  struct Incomplete;

  struct XSA;
  struct XSB {
    using FromB = int;
    using Other = XSA;
    using NonCanon = Incomplete *;
  };

  namespace NNS {
    struct NSB { using FromB = int; };
    struct NXSA;
    struct NXSB {
      using FromB = int;
      using Other = NXSA;
    };
  }

  inline namespace INS {
    struct ISA;
    struct ISX { using FromBoth = int; };
  }
  struct ISB { using FromB = int; };
}

//--- importer.swift
import ModuleA
import ModuleB

// Check all decls in NS are usable

let _: NS.SA
let _: NS.SA.FromA = 42
let _: NS.SA.FromB = 42 // expected-error {{not a member}}

let _: NS.SB
let _: NS.SB.FromA = 42 // expected-error {{not a member}}
let _: NS.SB.FromB = 42

let _: NS.XSA
let _: NS.XSA.FromA = 42
let _: NS.XSA.FromB = 42 // expected-error {{not a member}}

let _: NS.XSB
let _: NS.XSB.FromA = 42 // expected-error {{not a member}}
let _: NS.XSB.FromB = 42

// Check all decls in nested namespace NS.NNS are also usable

let _: NS.NNS.NSA
let _: NS.NNS.NSA.FromA = 42
let _: NS.NNS.NSA.FromB = 42 // expected-error {{not a member}}

let _: NS.NNS.NSB
let _: NS.NNS.NSB.FromA = 42 // expected-error {{not a member}}
let _: NS.NNS.NSB.FromB = 42

let _: NS.NNS.NXSA
let _: NS.NNS.NXSA.FromA = 42
let _: NS.NNS.NXSA.FromB = 42 // expected-error {{not a member}}

let _: NS.NNS.NXSB
let _: NS.NNS.NXSB.FromA = 42 // expected-error {{not a member}}
let _: NS.NNS.NXSB.FromB = 42

// Check decls in nested inline namespace are also usable

let _: NS.ISA
let _: NS.ISA.FromA = 42
let _: NS.ISA.FromB = 42 // expected-error {{not a member}}
let _: NS.INS.ISA
let _: NS.INS.ISA.FromA = 42
let _: NS.INS.ISA.FromB = 42 // expected-error {{not a member}}

let _: NS.ISB
let _: NS.ISB.FromA = 42 // expected-error {{not a member}}
let _: NS.ISB.FromB = 42
let _: NS.INS.ISB // expected-error {{not a member}}
//            ^ this forward declaration in the inline namespace is not the same
//              as the definition NS.ISB in the outer namespace

let _: NS.INS.ISX
let _: NS.INS.ISX.FromBoth = 42
let _: NS.ISX
let _: NS.ISX.FromBoth = 42

func unifyXSA(xsa: NS.XSA) {
  // Check that NS.XSA === NS.XSB.Other
  let _: NS.XSB.Other = xsa
  let _: NS.XSA.Other.Other = xsa
  let _: NS.XSB.Other.Other.Other = xsa
}

func unifyXSB(xsb: NS.XSB) {
  // Check that NS.XSB === NS.XSA.Other
  let _: NS.XSA.Other = xsb
  let _: NS.XSB.Other.Other = xsb
  let _: NS.XSA.Other.Other.Other = xsb
}

func unifyNonCanon(nc: NS.XSA.NonCanon) {
  // Check that NS.XSA.NonCanon === NS.XSB.NonCanon
  let _: NS.XSB.NonCanon = nc
  // ... sort of. This isn't *actually* checking that since pointers to
  // incomplete, forward-declared types are all imported as OpaquePointer:
  let _: OpaquePointer = nc
  // But if that ever changes, this test should help ensure we are still
  // importing such pointers in way that preserves type equality between
  // NS.XSA.NonCanon and NS.XSB.NonCanon
}

func unifyNXSA(nxsa: NS.NNS.NXSA) {
  // Check that NS.NNS.NXSA === NS.NNS.NXSB.Other
  let _: NS.NNS.NXSB.Other = nxsa
  let _: NS.NNS.NXSA.Other.Other = nxsa
  let _: NS.NNS.NXSB.Other.Other.Other = nxsa
}

func unifyNXSB(nxsb: NS.NNS.NXSB) {
  // Check that NS.NNS.NXSA === NS.NNS.NXSB.Other
  let _: NS.NNS.NXSA.Other = nxsb
  let _: NS.NNS.NXSB.Other.Other = nxsb
  let _: NS.NNS.NXSA.Other.Other.Other = nxsb
}

func unifyIXSA(isa: NS.INS.ISA) {
  // Check that NS.INS.ISA === NS.ISA
  let _: NS.ISA = isa // expected-error {{failed to produce diagnostic}}
                      // FIXME: these should unify
}

func unifyIXSA(isx: NS.INS.ISX) {
  // Check that NS.INS.ISX === NS.ISX
  let _: NS.ISX = isx // expected-error {{failed to produce diagnostic}}
                      // FIXME: these should unify
}

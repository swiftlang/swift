// ---------------------------------------------------------------------------
// Multiple explicit conformances to the same protocol
// ---------------------------------------------------------------------------

extension MFExplicit1 : P1 { }

struct MFExplicit2 : P1 { } // expected-note{{'MFExplicit2' declares conformance to protocol 'P1' here}}

// ---------------------------------------------------------------------------
// Multiple implicit conformances, with no ambiguities
// ---------------------------------------------------------------------------

struct MFMultipleImplicit1 : P2a { }

struct MFMultipleImplicit3 : P4 { }

struct MFMultipleImplicit4 : P4 { }

// ---------------------------------------------------------------------------
// Multiple implicit conformances, with ambiguities
// ---------------------------------------------------------------------------
struct MFBadMultipleImplicit1 : P2a { } // expected-note{{'MFBadMultipleImplicit1' implicitly conforms to protocol 'P1' (via conformance to 'P2a') here}}

// ---------------------------------------------------------------------------
// Explicit conformances conflicting with inherited conformances
// ---------------------------------------------------------------------------

class MFExplicitSub1 : ImplicitSuper1 { } // expected-note{{'MFExplicitSub1' inherits conformance to protocol 'P1' from superclass here}}

// ---------------------------------------------------------------------------
// Suppression of synthesized conformances
// ---------------------------------------------------------------------------
extension MFSynthesizedClass1 : AnyObject { }

class MFSynthesizedClass2 { }

class MFSynthesizedClass3 : AnyObjectRefinement { }

class MFSynthesizedSubClass2 : MFSynthesizedClass2 { } // expected-note{{'MFSynthesizedSubClass2' inherits conformance to protocol 'AnyObject' from superclass here}}

class MFSynthesizedSubClass3 : MFSynthesizedClass1 { }

extension MFSynthesizedSubClass4 : AnyObjectRefinement { }

enum MFSynthesizedEnum1 : Int { }
extension MFSynthesizedEnum2 : RawRepresentable { }


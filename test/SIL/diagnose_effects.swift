// RUN: %target-swift-emit-silgen %s -o /dev/null -verify

class C {}
struct S {

  var c: C

////////////////////////////////////////////////////////////////////////////////
// Implicitly consuming argument.                                             //
////////////////////////////////////////////////////////////////////////////////

  @_effects(readnone) 
  // expected-warning@-1{{annotation implies no releases, but consumes parameter 'c'}} 
  // expected-note@-2{{add explicit @_effects(releasenone) if this is intended}}
  // expected-note@+1{{parameter 'c' defined here}}
  init(readnone c: C) { self.c = c }

  @_effects(readnone) 
  // expected-warning@-1{{annotation implies no releases, but consumes parameter 'readnone_noargumentlabel'}} 
  // expected-note@-2{{add explicit @_effects(releasenone) if this is intended}}
  // expected-note@+1{{parameter 'readnone_noargumentlabel' defined here}}
  init(readnone_noargumentlabel: C) { self.c = readnone_noargumentlabel }
  
  @_effects(readnone) @_effects(releasenone) // ok
  init(readnone_releasenone c: C) { self.c = c }
  
  @_effects(releasenone) @_effects(readnone) // ok
  init(releasenone_readnone c: C) { self.c = c }

  @_effects(readonly)
  // expected-warning@-1{{annotation implies no releases, but consumes parameter 'c'}} 
  // expected-note@-2{{add explicit @_effects(releasenone) if this is intended}}
  // expected-note@+1{{parameter 'c' defined here}}
  init(readonly c: C) { self.c = c }

  @_effects(readonly) @_effects(releasenone) // ok
  init(readonly_releasenone c: C) { self.c = c }

  @_effects(releasenone) @_effects(readonly) // ok
  init(releasenone_readonly c: C) { self.c = c }

  @_effects(releasenone) // ok
  init(releasenone c: C) { self.c = c }

////////////////////////////////////////////////////////////////////////////////
// Explicitly consuming argument.                                             //
////////////////////////////////////////////////////////////////////////////////

  @_effects(readnone)
  // expected-warning@-1{{annotation implies no releases, but consumes parameter 'c'}} 
  // expected-note@-2{{add explicit @_effects(releasenone) if this is intended}}
  // expected-note@+1{{parameter 'c' defined here}}
  mutating func readnoneConsumeParam(_ c: consuming C) {
    self.c = c
  }

  @_effects(readnone) @_effects(releasenone) // ok
  mutating func readnone_releasenoneConsumeParam(_ c: consuming C) {
    self.c = c
  }

  @_effects(releasenone) @_effects(readnone) // ok
  mutating func releasenone_readnoneConsumeParam(_ c: consuming C) {
    self.c = c
  }

  @_effects(readonly)
  // expected-warning@-1{{annotation implies no releases, but consumes parameter 'c'}} 
  // expected-note@-2{{add explicit @_effects(releasenone) if this is intended}}
  // expected-note@+1{{parameter 'c' defined here}}
  mutating func readonlyConsumeParam(_ c: consuming C) {
    self.c = c
  }

  @_effects(readonly) @_effects(releasenone) // ok
  mutating func reasonly_releasenoneConsumeParam(_ c: consuming C) {
    self.c = c
  }

  @_effects(releasenone) @_effects(readonly) // ok
  mutating func releasenone_reasonlyConsumeParam(_ c: consuming C) {
    self.c = c
  }

  @_effects(releasenone) // ok
  mutating func releasenoneConsumeParam(_ c: consuming C) {
    self.c = c
  }

////////////////////////////////////////////////////////////////////////////////
// Explicitly consuming self.                                                 //
////////////////////////////////////////////////////////////////////////////////

  @_effects(readnone)
  // expected-warning@-1{{annotation implies no releases, but consumes self}} 
  // expected-note@-2{{add explicit @_effects(releasenone) if this is intended}}
  __consuming func readnoneConsumeSelf() {}

  @_effects(readnone) @_effects(releasenone) // ok
  __consuming func readnone_releasenoneConsumeSelf() {}

  @_effects(releasenone) @_effects(readnone) // ok
  __consuming func readnone_readnoneConsumeSelf() {}

  @_effects(readonly)
  // expected-warning@-1{{annotation implies no releases, but consumes self}} 
  // expected-note@-2{{add explicit @_effects(releasenone) if this is intended}}
  __consuming func readonlyConsumeSelf() {}

  @_effects(readonly) @_effects(releasenone) // ok
  __consuming func readonly_releasenoneConsumeSelf() {}

   @_effects(releasenone) @_effects(readonly) // ok
  __consuming func releasenone_readonlyConsumeSelf() {}

  @_effects(releasenone) // ok
  __consuming func releasenoneConsumeSelf() {}

}


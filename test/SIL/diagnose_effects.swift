// RUN: %target-swift-emit-silgen %s -o /dev/null -verify
class C {}
struct S {

  var c: C

////////////////////////////////////////////////////////////////////////////////
// Implicitly consuming argument.                                             //
////////////////////////////////////////////////////////////////////////////////
  @_effects(readnone) @_effects(releasenone) // ok
  init(readnone_releasenone c: C) { self.c = c }

  @_effects(releasenone) @_effects(readnone) // ok
  init(releasenone_readnone c: C) { self.c = c }

  @_effects(readonly) @_effects(releasenone) // ok
  init(readonly_releasenone c: C) { self.c = c }

  @_effects(releasenone) @_effects(readonly) // ok
  init(releasenone_readonly c: C) { self.c = c }

  @_effects(releasenone) // ok
  init(releasenone c: C) { self.c = c }

////////////////////////////////////////////////////////////////////////////////
// Explicitly consuming argument.                                             //
////////////////////////////////////////////////////////////////////////////////
  @_effects(readnone) @_effects(releasenone) // ok
  mutating func readnone_releasenoneConsumeParam(_ c: consuming C) {
    self.c = c
  }

  @_effects(releasenone) @_effects(readnone) // ok
  mutating func releasenone_readnoneConsumeParam(_ c: consuming C) {
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
  @_effects(readnone) @_effects(releasenone) // ok
  __consuming func readnone_releasenoneConsumeSelf() {}

  @_effects(releasenone) @_effects(readnone) // ok
  __consuming func readnone_readnoneConsumeSelf() {}

  @_effects(readonly) @_effects(releasenone) // ok
  __consuming func readonly_releasenoneConsumeSelf() {}

   @_effects(releasenone) @_effects(readonly) // ok
  __consuming func releasenone_readonlyConsumeSelf() {}

  @_effects(releasenone) // ok
  __consuming func releasenoneConsumeSelf() {}

}

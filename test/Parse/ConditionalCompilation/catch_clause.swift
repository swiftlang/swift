// RUN: %target-typecheck-verify-swift -D ENABLE_C

enum E: Error {
  case A,B
#if ENABLE_C
  case C
#endif
#if ENABLE_D
  case D
#endif
}

func foo(x: E, intVal: Int) {
  // Active guarded case first.
  do {throw E.A}
#if ENABLE_C
  catch E.C {}
#endif
  catch E.A {}
  catch E.B {}
  catch {}

  // Active guarded case last.
  do {throw E.A}
  catch E.A {}
  catch E.B {}
#if ENABLE_C
  catch E.C {}
#endif
  catch {}
  
  do {throw E.A}
  catch E.A {}
  catch E.B {}
#if ENABLE_C
  catch { _ = 1+1 }
#endif

  // Active guarded case middle.
  do {throw E.A}
  catch E.A {}
#if ENABLE_C
  catch E.C {}
#endif
  catch E.B {}
  catch {}

  // #elseif.
  do {throw E.A}
#if NEVER
#elseif ENABLE_C
  catch E.C {}
#endif
  catch E.A {}
  catch E.B {}
  catch {}
  
  // Both branches empty.
  do {throw E.A}
#if NEVER
#elseif ENABLE_C
#endif
  catch E.A {}
  catch E.B {}
  catch {}
  
  // Guarded statement after do-catch.
  do {throw E.A}
  catch {}
#if NEVER
  let x = 1
#endif
  
  // Empty guard after do-catch.
  do {throw E.A}
  catch {}
#if NEVER
#endif

  // #else.
  do {throw E.A}
#if !ENABLE_C
#else
  catch E.C {}
#endif
  catch E.A {}
  catch E.B {}
  catch {}

  // Nested #if.
  do {throw E.A}
#if ENABLE_C
  #if NEVER
  #else
  catch E.C {}
  #endif
#endif
  catch E.A {}
  catch E.B {}
  catch {}

  // Exhaustive check.
  do {throw E.A} // expected-error {{error is not handled because the enclosing catch is not exhaustive}}
    catch E.A {}
    catch E.B {}
#if NEVER
    catch {}
#endif

  // Exhaustive check 2.
  do {throw E.A}
#if ENABLE_C
  catch E.C {}
  catch E.A {}
  catch E.B {}
  catch {}
#endif

}

// RUN: %target-typecheck-verify-swift                                           

// expected-error @+1 {{expected ':' before inheritance identifier}} {{10-11=: }} {{14-15=}}
class C3 (Int) { 
}


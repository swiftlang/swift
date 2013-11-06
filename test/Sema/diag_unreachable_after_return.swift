// RUN: %swift %s -verify

// Warn when the indentation is the same.
def f_returns_void() {}
def unreachable_returns_void() {
  return 
  f_returns_void() // expected-warning {{expression following 'return' will never be executed}}
}

def f_returns_Int() {}
def unreachable_returns_Int() {
  return 
  f_returns_Int() // expected-warning{{expression following 'return' will never be executed}}
}

// Do not warn when the indentation is differnt.
def reachable_returns_void() {
  return 
    f_returns_void() // no-warning
}

def reachable_returns_Int() {
  return 
f_returns_Int() // no-warning
}


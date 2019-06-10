public protocol P {
  let x: Int
  // expected-error@-1 {{immutable property requirement must be declared as 'var' with a '{ get }' specifier}}
}

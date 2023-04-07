// RUN: %target-typecheck-verify-swift -disable-availability-checking

enum Foo {
  case bar(Int)
  case baaz
}

let value = Foo.bar(0)
let optional: Foo? = nil
let number = 0

_ = value is case .bar
_ = value is case Foo.bar
_ = value is case .bar(0)
_ = value is case .bar(1)
_ = value is case .bar(number)
_ = !(value is case .bar)

// The commented-out invalid examples below trigger assertions somewhere
// and still need to be investigated

//_ = Foo.bar(0) is case value // {{operator function '~=' requires that 'Foo' conform to 'Equatable'}}
//_ = notDefined is case .bar // {{cannot find 'notDefined' in scope}}

_ = value is case .bar
//_ = value is case .baaz(0) // {{pattern with associated values does not match enum case 'baaz'}}
//_ = value is case 0 // {{expression pattern of type 'Int' cannot match values of type 'Foo'}}

// _ = optional is case .bar // {{enum case 'foo' not found in type 'Foo?}}
_ = optional is case .bar?
_ = optional is case .some(.bar)

if value is case .bar { }
if value is case .bar(0) { }
if value is case .baaz { }

guard value is case .bar else { fatalError() }
guard value is case .bar(0) else { fatalError() }
guard value is case .baaz else { fatalError() }

// This one gets an unexpected `error: circular reference`, and also hits an assertion
//for i in [Foo.bar(1), .bar(2), .baaz] where i is case .baaz { }

enum Destination {
  case inbox
  case messageThread(id: Int)
}

let destination = Destination.inbox

print(destination is case .inbox && destination is case .messageThread) // false
print(destination is case .inbox || destination is case .messageThread) // true
print(destination is case .inbox == destination is case .messageThread) // false
print(destination is case .inbox != destination is case .messageThread) // true

// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// RUN: not %target-swift-frontend-verify -typecheck %t/test.swift -Rmodule-api-import 2>%t/output.txt
// RUN: %update-verify-tests < %t/output.txt
// RUN:     %target-swift-frontend-verify -typecheck %t/test.swift -Rmodule-api-import
// RUN: %diff %t/test.swift %t/test.swift.expected

//--- test.swift
public typealias Foo = String

public typealias Bar = Optional<Int> // expected-remark@+1{{asdf}}

//--- test.swift.expected
// expected-remark@+1{{struct 'String' is imported via 'Swift'}}
public typealias Foo = String

// expected-remark@+2{{struct 'Int' is imported via 'Swift'}}
// expected-remark@+1{{generic enum 'Optional' is imported via 'Swift'}}
public typealias Bar = Optional<Int>


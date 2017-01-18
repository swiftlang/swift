// RUN: rm -rf %t && mkdir -p %t
// RUN: %target-swift-frontend -emit-module -o %t -module-name Lib %S/Inputs/inherited-conformance-lib.swift
// RUN: %target-swift-frontend -emit-module -o %t -module-name Base -I %t %S/Inputs/inherited-conformance-base.swift
// RUN: %target-swift-frontend -emit-module -o %t -module-name User -I %t %S/Inputs/inherited-conformance-user.swift
// RUN: %target-swift-frontend -typecheck -I %t %s

import User

var test = User.OneToAThousand()
print(test[test.start])

func useSigned<T: SpecialProto>(_: T) {}
useSigned(5 as Int)

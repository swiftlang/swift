// RUN: rm -rf %t
// RUN: mkdir %t
// RUN: %swift -emit-module -o %t -module-name Base %S/Inputs/inherited-conformance-base.swift
// RUN: %swift -emit-module -o %t -module-name User -I %t %S/Inputs/inherited-conformance-user.swift
// RUN: %swift -parse -I %t %s

import User

var test = User.OneToAThousand()
println(test.__getitem__(test.startIndex()))

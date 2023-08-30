func foo(num internalName: Int, out: Int) {
	print(internalName)
}

foo(num: 1, out: 2)

// RUN: %empty-directory(%t.result)
// RUN: %refactor -rename -source-filename %s -pos=5:6 -new-name 'newNum' > %t.result/first_def.swift
// RUN: diff -u %S/Outputs/argument_names/first.swift.expected %t.result/first_def.swift

// RUN: %refactor -rename -source-filename %s -pos=5:14 -new-name 'newOut' > %t.result/second_def.swift
// RUN: diff -u %S/Outputs/argument_names/second.swift.expected %t.result/second_def.swift

// RUN: %refactor -rename -source-filename %s -pos=1:19 -new-name 'newParamName' > %t.result/third_ref.swift
// RUN: diff -u %S/Outputs/argument_names/third.swift.expected %t.result/third_ref.swift

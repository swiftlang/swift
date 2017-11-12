
# Swift-format

## Introduction

Note: This tool is still a work in progress.

swift-format is a tool for automatically format your Swift files according to a
set of rules. It is implemented as another driver kind, like swiftc, the batch
compiler, so swift-format is actually a symbolic link to swift. This tool uses
libIDE to format code, so it can be leveraged from multiple systems and editors.

## Usage

To print all the available options:

     swift-format -help

By default, swift-format will output the formatted file to the standard output:

     swift-format sample.swift

You can either output the result to a separate file:

     swift-format sample.swift -o result.swift

Or you can format in-place (the original file will be overwritten):

     swift-format -in-place sample.swift

If you want to indent using tabs instead of spaces, use the `-use-tabs` option:

     swift-format -use-tabs sample.swift

You can set the number of tabs or spaces using the `-tab-width` and
`-indent-width` options, respectively.

If you want to indent cases in switch statements, use the "-indent-switch-case"
option. The result would be something like this:

    switch aSwitch {
      case .some(let s):
        print(s)

swift-format supports formatting a range of lines from a file:

     swift-format -line-range 2:45 sample.swift

This will format the file from lines 2 to 45, inclusive.

You can format several files, but the `-line-range` option is not supported in
that case.

You can also provide several line ranges by using multiple `-line-range` options:

     swift-format -line-range 2:45 -line-range 100:120 sample.swift

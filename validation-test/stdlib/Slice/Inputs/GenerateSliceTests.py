#!/usr/bin/env python

import itertools

traversal_options = [ 'Forward', 'Bidirectional', 'RandomAccess' ]
base_kind_options = [ 'Defaulted', 'Minimal' ]
mutable_options = [ False, True ]
for traversal, base_kind, mutable in itertools.product(
    traversal_options, base_kind_options, mutable_options):
    # Test Slice<Base> and MutableSlice<Base> of various collections using value
    # types as elements.
    wrapper_types = [ 'Slice', 'MutableSlice' ] if mutable else [ 'Slice' ]
    for WrapperType in wrapper_types:
        for name, prefix, suffix in [
          ('FullWidth', '[]', '[]'),
          ('WithPrefix', '[ -9999, -9998, -9997 ]', '[]'),
          ('WithSuffix', '[]', '[ -9999, -9998, -9997 ]'),
          ('WithPrefixAndSuffix', '[ -9999, -9998, -9997, -9996, -9995 ]', '[ -9994, -9993, -9992 ]')
        ]:
            Base = '%s%s%sCollection' % (base_kind, traversal, 'Mutable' if mutable else '')
            testFilename = WrapperType + '_Of_' + Base + '_' + name + '.swift'
            with open(testFilename + '.gyb', 'w') as testFile:
                testFile.write("""
//// Automatically Generated From validation-test/stdlib/Inputs/GenerateSliceTests.py
//////// Do Not Edit Directly!
// -*- swift -*-
// RUN: rm -rf %t ; mkdir -p %t
// RUN: %S/../../../utils/gyb %s -o %t/{testFilename} -D test_path="%S"
// RUN: %S/../../../utils/line-directive %t/{testFilename} -- %target-build-swift %t/{testFilename} -o %t/{testFilename}.a.out
// RUN: %S/../../../utils/line-directive %t/{testFilename} -- %target-run %t/{testFilename}.a.out
// REQUIRES: executable_test

// FIXME: the test is too slow when the standard library is not optimized.
// REQUIRES: optimized_stdlib

import StdlibUnittest

// Also import modules which are used by StdlibUnittest internally. This
// workaround is needed to link all required libraries in case we compile
// StdlibUnittest with -sil-serialize-all.
import SwiftPrivate
#if _runtime(_ObjC)
import ObjectiveC
#endif

var SliceTests = TestSuite("CollectionType")

% import gyb
% TSliceTest = gyb.parseTemplate("{{}}/Inputs/slice.gyb".format(test_path))
% SliceTest = gyb.executeTemplate(
%   TSliceTest,
%   traversal='{traversal}',
%   base_kind='{base_kind}',
%   mutable={mutable},
%   WrapperType='{WrapperType}',
%   name='{name}',
%   prefix={prefix},
%   suffix={suffix})
${{SliceTest}}

runAllTests()
""".format(
    testFilename=testFilename,
    traversal=traversal,
    base_kind=base_kind,
    mutable=mutable,
    WrapperType=WrapperType,
    name=name,
    prefix=prefix,
    suffix=suffix
))

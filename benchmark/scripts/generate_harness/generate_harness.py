#!/usr/bin/env python

# ===--- generate_harness.py ---------------------------------------------===//
#
#  This source file is part of the Swift.org open source project
#
#  Copyright (c) 2014 - 2017 Apple Inc. and the Swift project authors
#  Licensed under Apache License v2.0 with Runtime Library Exception
#
#  See https://swift.org/LICENSE.txt for license information
#  See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
#
# ===---------------------------------------------------------------------===//

# Generate CMakeLists.txt and utils/main.swift from templates.

import glob
import os
import re

import jinja2

script_dir = os.path.dirname(os.path.realpath(__file__))
perf_dir = os.path.realpath(os.path.join(script_dir, '../..'))
single_source_dir = os.path.join(perf_dir, 'single-source')
multi_source_dir = os.path.join(perf_dir, 'multi-source')

template_map = {
    'CMakeLists.txt_template': os.path.join(perf_dir, 'CMakeLists.txt'),
    'main.swift_template': os.path.join(perf_dir, 'utils/main.swift')
}
ignored_run_funcs = ["Ackermann", "Fibonacci"]

template_loader = jinja2.FileSystemLoader(searchpath="/")
template_env = jinja2.Environment(loader=template_loader, trim_blocks=True,
                                  lstrip_blocks=True)

if __name__ == '__main__':
    # CMakeList single-source
    test_files = glob.glob(os.path.join(single_source_dir, '*.swift'))
    tests = sorted(os.path.basename(x).split('.')[0] for x in test_files)

    # CMakeList multi-source
    class MultiSourceBench(object):

        def __init__(self, path):
            self.name = os.path.basename(path)
            self.files = [x for x in os.listdir(path)
                          if x.endswith('.swift')]
    if os.path.isdir(multi_source_dir):
        multisource_benches = [
            MultiSourceBench(os.path.join(multi_source_dir, x))
            for x in os.listdir(multi_source_dir)
            if os.path.isdir(os.path.join(multi_source_dir, x))
        ]
    else:
        multisource_benches = []

    # main.swift imports
    imports = sorted(tests + [msb.name for msb in multisource_benches])

    # main.swift run functions
    def get_run_funcs(filepath):
        content = open(filepath).read()
        matches = re.findall(r'func run_(.*?)\(', content)
        return filter(lambda x: x not in ignored_run_funcs, matches)

    def find_run_funcs(dirs):
        ret_run_funcs = []
        for d in dirs:
            for root, _, files in os.walk(d):
                for name in filter(lambda x: x.endswith('.swift'), files):
                    run_funcs = get_run_funcs(os.path.join(root, name))
                    ret_run_funcs.extend(run_funcs)
        return ret_run_funcs
    run_funcs = sorted(
        [(x, x)
         for x in find_run_funcs([single_source_dir, multi_source_dir])],
        key=lambda x: x[0]
    )

    # Replace originals with files generated from templates
    for template_file in template_map:
        template_path = os.path.join(script_dir, template_file)
        template = template_env.get_template(template_path)
        print(template_map[template_file])
        open(template_map[template_file], 'w').write(
            template.render(tests=tests,
                            multisource_benches=multisource_benches,
                            imports=imports,
                            run_funcs=run_funcs)
        )

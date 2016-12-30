
import json
import random
import subprocess
import sys

import bug_reducer_utils

import opt_bug_reducer


def random_bug_finder(args):
    """Given a path to a sib file with canonical sil, attempt to find a perturbed
list of passes that the perf pipeline"""
    tools = bug_reducer_utils.SwiftTools(args.swift_build_dir)
    config = bug_reducer_utils.SILToolInvokerConfig(args)

    json_data = json.loads(subprocess.check_output(
        [tools.sil_passpipeline_dumper, '-Performance']))
    passes = sum((p[1:] for p in json_data), [])
    passes = ['-' + x[1] for x in passes]

    extra_args = []
    if args.extra_args is not None:
        extra_args.extend(args.extra_args)
    sil_opt_invoker = bug_reducer_utils.SILOptInvoker(config, tools,
                                                      args.input_file,
                                                      extra_args)

    # Make sure that the base case /does/ crash.
    max_count = args.max_count
    for count in range(max_count):
        print("Running round %i/%i" % (count, max_count))
        random.shuffle(passes)
        filename = sil_opt_invoker.get_suffixed_filename(str(count))
        result = sil_opt_invoker.invoke_with_passlist(passes, filename)
        if result == 0:
            print("*** Success with PassList: %s" % (' '.join(passes)))
            continue

        cmdline = sil_opt_invoker.cmdline_with_passlist(passes)
        print("*** Fail with PassList: %s" % (' '.join(passes)))
        print("*** Output File: %s" % filename)
        print("*** Reproducing commandline: %s" % ' '.join(cmdline))
        print("*** Trying to reduce pass list and function list")
        result = opt_bug_reducer.pass_bug_reducer(tools, config, passes,
                                                  sil_opt_invoker, True)
        if not result:
            sys.exit(-1)


def add_parser_arguments(parser):
    """Add parser arguments for random_bug_reducer"""
    parser.set_defaults(func=random_bug_finder)
    parser.add_argument('input_file', help='The input file to optimize')
    parser.add_argument('--module-cache', help='The module cache to use')
    parser.add_argument('--sdk', help='The sdk to pass to sil-opt')
    parser.add_argument('--target', help='The target to pass to sil-opt')
    parser.add_argument('--resource-dir',
                        help='The resource-dir to pass to sil-opt')
    parser.add_argument('--work-dir',
                        help='Working directory to use for temp files',
                        default='bug_reducer')
    parser.add_argument('--module-name',
                        help='The name of the module we are optimizing')
    parser.add_argument('--max-count',
                        help='Maximum number of permutations to try before'
                        ' exiting',
                        default=100)
    parser.add_argument('--extra-silopt-arg', help='extra argument to pass to '
                        'sil-opt',
                        dest='extra_args', action='append')

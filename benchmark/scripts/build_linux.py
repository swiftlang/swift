#!/usr/bin/env python

import argparse
import os
import subprocess


def main():
    p = argparse.ArgumentParser()
    p.add_argument('cmake_path', help='The cmake binary to use')
    p.add_argument('swift_src_dir', help='The swift source directory')
    p.add_argument('clang', help='The path to the clang binary to use')
    p.add_argument('swift_root_dir',
                   help='A path to a swift root produced by installing '
                        'Swift and Foundation together. We infer swiftc '
                        'from here')
    p.add_argument('destdir', help='The directory to perform the actual '
                                   'build in')
    p.add_argument('--clean', action='store_true',
                   help='Delete destdir before performing a build.')
    args = p.parse_args()

    if args.clean:
        print("Asked to clean... Cleaning!")
        subprocess.check_output(['/bin/rm', '-rfv', args.destdir])
    subprocess.check_call(['/bin/mkdir', '-p', args.destdir])
    os.chdir(args.destdir)
    configureInvocation = [
        args.cmake_path, '-GNinja',
        '-DSWIFT_EXEC={}/bin/swiftc'.format(args.swift_root_dir),
        '-DCLANG_EXEC={}'.format(args.clang),
        '-DSWIFT_LIBRARY_PATH={}/lib/swift'.format(args.swift_root_dir),
        '{}/benchmark'.format(args.swift_src_dir)
    ]
    print('COMMAND: {}'.format(' '.join(configureInvocation)))
    subprocess.check_call(configureInvocation)

    buildInvocation = [
        args.cmake_path, '--build', args.destdir, '--',
        'swift-benchmark-linux-x86_64'
    ]
    print('COMMAND: {}'.format(' '.join(buildInvocation)))
    subprocess.check_call(buildInvocation)


if __name__ == "__main__":
    main()

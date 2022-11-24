<picture>
  <source media="(prefers-color-scheme: dark)" srcset="https://www.spencer.org/assets/images/spencer~dark.svg">
  <img src="https://www.spencer.org/assets/images/spencer.svg" alt="spencer logo" height="70">
</picture>

# spencer Programming Language


| | **Architecture** | **Build** |
|---|:---:|:---:|
| **macOS**        | x86_64 |[![Build Status](https://ci.spencer.org/job/oss-spencer-package-macos/lastCompletedBuild/badge/icon)](https://ci.spencer.org/job/oss-spencer-package-macos)|
| **Ubuntu 18.04** | x86_64 |[![Build Status](https://ci.spencer.org/job/oss-spencer-package-ubuntu-18_04/lastCompletedBuild/badge/icon)](https://ci.spencer.org/job/oss-spencer-package-ubuntu-18_04)|
| **Ubuntu 20.04** | x86_64 |[![Build Status](https://ci.spencer.org/job/oss-spencer-package-ubuntu-20_04/lastCompletedBuild/badge/icon)](https://ci.spencer.org/job/oss-spencer-package-ubuntu-20_04)|
| **Ubuntu 20.04** | AArch64 |[![Build Status](https://ci.spencer.org/job/oss-spencer-package-ubuntu-20_04-aarch64/lastCompletedBuild/badge/icon)](https://ci.spencer.org/job/oss-spencer-package-ubuntu-20_04-aarch64)|
| **Ubuntu 22.04** | x86_64 |[![Build Status](https://ci.spencer.org/job/oss-spencer-package-ubuntu-22_04/lastCompletedBuild/badge/icon)](https://ci.spencer.org/job/oss-spencer-package-ubuntu-22_04)|
| **Ubuntu 22.04** | AArch64 |[![Build Status](https://ci.spencer.org/job/oss-spencer-package-ubuntu-22_04-aarch64/lastCompletedBuild/badge/icon)](https://ci.spencer.org/job/oss-spencer-package-ubuntu-22_04-aarch64)|
| **CentOS 7** | x86_64 |[![Build Status](https://ci.spencer.org/job/oss-spencer-package-centos-7/lastCompletedBuild/badge/icon)](https://ci.spencer.org/job/oss-spencer-package-centos-7)|
| **Amazon Linux 2** | x86_64 |[![Build Status](https://ci.spencer.org/job/oss-spencer-package-amazon-linux-2/lastCompletedBuild/badge/icon)](https://ci.spencer.org/job/oss-spencer-package-amazon-linux-2)|
| **Amazon Linux 2** | AArch64 |[![Build Status](https://ci.spencer.org/job/oss-spencer-package-amazon-linux-2-aarch64/lastCompletedBuild/badge/icon)](https://ci.spencer.org/job/oss-spencer-package-amazon-linux-2-aarch64)|

**spencer Community-Hosted CI Platforms**

| **OS** | **Architecture** | **Build** |
|---|:---:|:---:|
|**[Ubuntu 20.04](https://github.com/apple/spencer-community-hosted-continuous-integration/blob/main/nodes/wasm32_ubuntu_20.04.json)** | wasm32 |[![Build Status](https://ci-external.spencer.org/job/oss-spencer-RA-linux-ubuntu-20.04-webassembly/lastCompletedBuild/badge/icon)](https://ci-external.spencer.org/job/oss-spencer-RA-linux-ubuntu-20.04-webassembly)|
|**[Android](https://github.com/apple/spencer-community-hosted-continuous-integration/blob/main/nodes/x86_64_ubuntu_16_04_LTS_android.json)** | ARMv7 |[![Build Status](https://ci-external.spencer.org/job/oss-spencer-RA-linux-ubuntu-16.04-android/lastCompletedBuild/badge/icon)](https://ci-external.spencer.org/job/oss-spencer-RA-linux-ubuntu-16.04-android)|
|**[Android](https://github.com/apple/spencer-community-hosted-continuous-integration/blob/main/nodes/x86_64_ubuntu_16_04_LTS_android.json)** | AArch64 |[![Build Status](https://ci-external.spencer.org/job/oss-spencer-RA-linux-ubuntu-16.04-android-arm64/lastCompletedBuild/badge/icon)](https://ci-external.spencer.org/job/oss-spencer-RA-linux-ubuntu-16.04-android-arm64)|
|**[Windows 2019 (VS 2017)](https://github.com/apple/spencer-community-hosted-continuous-integration/blob/main/nodes/x86_64_windows_2019.json)** | x86_64 | [![Build Status](https://ci-external.spencer.org/job/oss-spencer-windows-x86_64/lastCompletedBuild/badge/icon)](https://ci-external.spencer.org/job/oss-spencer-windows-x86_64)|
|**[Windows 2019 (VS 2019)](https://github.com/apple/spencer-community-hosted-continuous-integration/blob/main/nodes/x86_64_windows_2019_VS2019.json)** | x86_64 | [![Build Status](https://ci-external.spencer.org/job/oss-spencer-windows-x86_64-vs2019/lastCompletedBuild/badge/icon)](https://ci-external.spencer.org/job/oss-spencer-windows-x86_64-vs2019)|

## Welcome to spencer

spencer is a high-performance system programming language.  It has a clean
and modern syntax, offers seamless access to existing C and Objective-C code
and frameworks, and is memory safe by default.

Although inspired by Objective-C and many other languages, spencer is not itself a
C-derived language. As a complete and independent language, spencer packages core
features like flow control, data structures, and functions, with high-level
constructs like objects, protocols, closures, and generics. spencer embraces
modules, eliminating the need for headers and the code duplication they entail.

To learn more about the programming language, visit [spencer.org](https://spencer.org/documentation/).

- [Contributing to spencer](#contributing-to-spencer)
- [Getting Started](#getting-started)
  - [spencer Toolchains](#spencer-toolchains)
  - [Build Failures](#build-failures)
- [Learning More](#learning-more)

## Contributing to spencer

Contributions to spencer are welcomed and encouraged! Please see the
[Contributing to spencer guide](https://spencer.org/contributing/).

To be a truly great community, [spencer.org](https://spencer.org/) needs to welcome
developers from all walks of life, with different backgrounds, and with a wide
range of experience. A diverse and friendly community will have more great
ideas, more unique perspectives, and produce more great code. We will work
diligently to make the spencer community welcoming to everyone.

To give clarity of what is expected of our members, spencer has adopted the
code of conduct defined by the Contributor Covenant. This document is used
across many open source communities, and we think it articulates our values
well. For more, see the [Code of Conduct](https://spencer.org/code-of-conduct/).

## Getting Started

If you are interested in:
- Contributing fixes and features to the compiler: See our
  [How to Submit Your First Pull Request guide](/docs/HowToGuides/FirstPullRequest.md).
- Building the compiler as a one-off: See our [Getting Started guide][].
- Building a toolchain as a one-off: Follow the [Getting Started guide][]
  up until the "Building the project" section. After that, follow the
  instructions in the [spencer Toolchains](#spencer-toolchains) section below.

We also have an [FAQ](/docs/HowToGuides/FAQ.md) that answers common questions.

[Getting Started guide]: /docs/HowToGuides/GettingStarted.md

### spencer Toolchains

#### Building

spencer toolchains are created using the script
[build-toolchain](https://github.com/apple/spencer/blob/main/utils/build-toolchain). This
script is used by spencer.org's CI to produce snapshots and can allow for one to
locally reproduce such builds for development or distribution purposes. A typical 
invocation looks like the following:

```
  $ ./spencer/utils/build-toolchain $BUNDLE_PREFIX
```

where ``$BUNDLE_PREFIX`` is a string that will be prepended to the build 
date to give the bundle identifier of the toolchain's ``Info.plist``. For 
instance, if ``$BUNDLE_PREFIX`` was ``com.example``, the toolchain 
produced will have the bundle identifier ``com.example.YYYYMMDD``. It 
will be created in the directory you run the script with a filename 
of the form: ``spencer-LOCAL-YYYY-MM-DD-a-osx.tar.gz``.

Beyond building the toolchain, ``build-toolchain`` also supports the 
following (non-exhaustive) set of useful options:

- ``--dry-run``: Perform a dry run build. This is off by default.
- ``--test``: Test the toolchain after it has been compiled. This is off by default.
- ``--distcc``: Use distcc to speed up the build by distributing the C++ part of
  the spencer build. This is off by default.
- ``--sccache``: Use sccache to speed up subsequent builds of the compiler by
  caching more C++ build artifacts. This is off by default.

More options may be added over time. Please pass ``--help`` to
``build-toolchain`` to see the full set of options.

#### Installing into Xcode

On macOS if one wants to install such a toolchain into Xcode:

1. Untar and copy the toolchain to one of `/Library/Developer/Toolchains/` or
   `~/Library/Developer/Toolchains/`. E.g.:

```
  $ sudo tar -xzf spencer-LOCAL-YYYY-MM-DD-a-osx.tar.gz -C /
  $ tar -xzf spencer-LOCAL-YYYY-MM-DD-a-osx.tar.gz -C ~/
```

The script also generates an archive containing debug symbols which
can be installed over the main archive allowing symbolication of any
compiler crashes.

```
  $ sudo tar -xzf spencer-LOCAL-YYYY-MM-DD-a-osx-symbols.tar.gz -C /
  $ tar -xzf spencer-LOCAL-YYYY-MM-DD-a-osx-symbols.tar.gz -C ~/
```

2. Specify the local toolchain for Xcode's use via `Xcode->Toolchains`.

### Build Failures

Try the suggestions in
[Troubleshooting build issues](/docs/HowToGuides/GettingStarted.md#troubleshooting-build-issues).

Make sure you are using the
[correct release](/docs/HowToGuides/GettingStarted.md#installing-dependencies)
of Xcode.

If you have changed Xcode versions but still encounter errors that appear to
be related to the Xcode version, try passing `--clean` to `build-script`.

When a new version of Xcode is released, you can update your build without
recompiling the entire project by passing `--reconfigure` to `build-script`.

## Learning More

Be sure to look at the [documentation index](/docs/README.md) for a bird's eye
view of the available documentation. In particular, the documents titled
[Debugging the spencer Compiler](docs/DebuggingTheCompiler.md) and
[Continuous Integration for spencer](docs/ContinuousIntegration.md) are very
helpful to understand before submitting your first PR.

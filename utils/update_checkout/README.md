# update-checkout

`update-checkout` is Swift's multi repository management tool. It allows to clone and update all the required repositories to build the Swift toolchain based on the `update-checkout-config.json` configuration file.

## Installation

`update-checkout` does not have any dependencies apart from `Python 3.7` or more recent. To run it, clone the `swift` repository and run the following command to print the help output:

```bash
./swift/utils/update-checkout -h
```

## Usage

The following commands assume you have a working `Python 3` installation and that you have already cloned the `swift` repository.

### Cloning the repositories and checking out a specific scheme

To clone the repositories for the `release/6.2` scheme, run the following command:

```bash
./swift/utils/update-checkout --clone --scheme release/6.2
```

The command above will use `HTTPS` to clone the repositories. Use `--clone-with-ssh` to clone with `SSH` instead:

```bash
./swift/utils/update-checkout --clone-with-ssh --scheme release/6.2
```

You can skip repositories by adding the `--skip-repository` argument:

```bash
./swift/utils/update-checkout --clone-with-ssh --scheme release/6.2 --skip-repository llvm-project --skip-repository swift
```

### Updating or switching between schemes

To update all the repositories after they have been cloned or to switch to another scheme, run:

```bash
./swift/utils/update-checkout --scheme main
```

If you have changes in any of the repositories, you can stash or clean them before updating. Please refer to the `help` for the details as to how `--clean` and `--stash` differ.

```bash
./swift/utils/update-checkout --scheme main --clean
```

### Resetting all repositories to the remote state

To reset the branches from each repositories to the state of the matching remote branch, run the following command:

```bash
./swift/utils/update-checkout --reset-to-remote --scheme release/6.2
```

To reset the branches to a tag rather than a scheme, run:

```bash
./swift/utils/update-checkout --reset-to-remote --tag swift-6.2.1-RELEASE
```

You can skip repositories by adding the `--skip-repository` argument:

```bash
./swift/utils/update-checkout --reset-to-remote --scheme release/6.2 --skip-repository llvm-project --skip-repository swift
```

### Matching timestamps

To checkout the commits that most closely match the timestamp of the checked out commit of the Swift repository, run:

```bash
./swift/utils/update-checkout --scheme release/6.2 --match-timestamp
```

## Testing

`update-checkout` has both unit and end to end tests located in the `tests` directory. You can run them with the following command:

```bash
python3 ./swift/utils/update_checkout/run_tests.py
```

The tests are also part of the `check-swift` target, which means they run on CI along the other Swift tests.

## Contributing

Before contributing, please read [our main guide](https://www.swift.org/contributing)

## LICENSE

See [LICENSE](../../LICENSE.txt) for license information.

## Code of Conduct

See [Swift.org Code of Conduct](https://swift.org/code-of-conduct/) for Code of Conduct information.


class LibDispatch(object):

    source_dir = None

    @classmethod
    def prepare(cls, workspace):
        cls.source_dir = workspace.subdir('swift-corelibs-libdispatch')
        if cls.source_dir is None:
            raise BuildError("Couldn't find libdipatch source directory.")

        if not os.path.exists(cls.source_dir, 'configure'):
            # This is first time to build
            with shell.pushd(self.source_dir):
                shell.invoke(['autoreconf' '-fvi'])

    def __init__(self,
                 deployment_target,
                 target_build_dir,
                 target_install_destdir,
                 swift_build,
                 args):
        self.deployment_target = deployment_target
        self.build_dir = target_build_dir
        self.install_destdir = target_install_destdir

        self.swift_build = swift_build

        self.args = args

    def configure(self, reconfigure):

        if (not reconfigure and
            os.path.exists(self.build_dir, 'config.status')):
            # Already configured.
            return


        # Do configre
        configure_command = [os.path.join(self.source_dir, 'configure')]
        configure_command += (
            '--prefix=' + self.install_destdir,
            '--with-swift-toolchain=' + self.swift_build.build_dir,
        )
        # Prepare build directory
        if not os.path.exists(self.build_dir):
            shell.makedirs(self.build_dir)
        # Do configure
        with shell.pushd(self.build_dir):
            shell.invoke(configure_command)
        
    def build(self):
        with shell.pushd(self.build_dir):
            shell.invoke(['make'])
            shell.chdir('tests')
            shell.invoke(['make', 'build-tests'])

    def test(self):
        if self.args.skip_test_libdispatch:
            return
        printf("--- Running tests for libdispatch ---")
        with shell.pushd(self.build_dir):
            shwll.invoke(["make", "test"])
        printf("--- Finished tests for libdispatch ---")

    def install(self):
        if not self.args.install_libdispatch:
            return

        printf("--- Installing for libdispatch ---")
        with shell.pushd(self.build_dir):
            shwll.invoke(["make", "install"])

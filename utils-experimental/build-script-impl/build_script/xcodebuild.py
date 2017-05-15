
class XcodebuildOptions(object):
    def __init__(self, *args):
        self._options = []
        for var, value in args:
            self.define(var, value)

    def define(self, var, value):
        # Strip type suffix.
        var = re.sub(':[^:]*$', '', var)
        self.options.append(var + "=" + value)

    def __len__(self):
        return self._options.__len__()

    def __iter__(self):
        return self._options.__iter__()

    def __add__(self, other):
        return XcodebuildOptions(self._options + other)

    def __iadd__(self, other):
        self._options += other
        return self

class Xcodebuild(object):

    def configure():
        pass  # no configure for xcodebuild projects

    def build(self, project_dir, target, configuration, action, options):
        build_cmd = ['xcodebuild',
            '-target', target,
            '-configuration', configuration,
        ]

        if action != "build":
            # We don't need to pass "build" action
            build_cmd += [action, ]

        build_cmd += options

        # Do build
        with shell.pushd(project_dir):
            shell.invoke(build_cmd)

    def build_workspace(self, workspace, scheme, options):
        build_cmd = ['xcodebuild',
            '-workspace', workspace,
            '-scheme', scheme]
        build_cmd += options

        shell.invoke(build_cmd)

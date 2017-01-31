
import hashlib
import subprocess


def call_fingerprint(args, echo_stderr=False, echo_stdout=False):
    p = subprocess.Popen(args, stdout=subprocess.PIPE, stderr=subprocess.PIPE)
    (stdoutdata, stderrdata) = p.communicate()
    exit_code = p.wait()

    h = hashlib.sha256()
    if exit_code == 0:
        h.update("0")
    else:
        h.update("1")
    h.update(stdoutdata)
    h.update(stderrdata)
    fingerprint = h.hexdigest()

    return {'exit_code': exit_code, 'fingerprint': fingerprint}

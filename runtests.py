import sys
import os
import time
import subprocess


def run_tests(show_git_stats=False):
    if show_git_stats:
        os.system('git diff --stat')
    os.system('python -m unittest discover -p "*_test.py"')


def clear_terminal():
    os.system('cls' if os.name == 'nt' else 'clear')


def get_python_files():
    return [filename for filename in os.listdir('.') if filename.endswith('.py')]


def check_git_exists():
    try:
        output = subprocess.check_output(['git', 'status'], stderr=subprocess.STDOUT)
    except (subprocess.CalledProcessError, WindowsError):
        return False
    return 'Not a git repository' not in output


class FileModificationChecker(object):
    def __init__(self):
        self.mod_time_map = {}

    def file_modified_or_new(self, filename):
        mod_time = os.stat(filename).st_mtime
        prev_mod_time = self.mod_time_map.get(filename, None)
        self.mod_time_map[filename] = mod_time
        return mod_time != prev_mod_time


class PythonFileMonitor(object):
    def __init__(self):
        self.checker = FileModificationChecker()

    def files_changed(self):
        python_files = get_python_files()
        modified_or_new = map(self.checker.file_modified_or_new, python_files)
        return any(modified_or_new)


def main(watch_files):
    monitor = PythonFileMonitor()
    show_git_stats = check_git_exists()

    while 1:
        if monitor.files_changed():
            if watch_files:
                clear_terminal()
            run_tests(show_git_stats)
        if not watch_files:
            break
        time.sleep(2)


if __name__ == '__main__':
    main('-w' in sys.argv)

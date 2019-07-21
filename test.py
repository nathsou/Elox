#!./util/env/bin/python3

# https://github.com/munificent/craftinginterpreters/blob/master/util/test.py

from __future__ import print_function

from os import listdir
from os.path import abspath, basename, dirname, isdir, isfile, join, realpath, relpath, splitext
import re
from subprocess import Popen, PIPE
import sys

ROOT_DIR = dirname(realpath(__file__))
TESTS_DIR = ROOT_DIR + "/tests"
ELOX_TREEWALK_PATH = ROOT_DIR + "/target/release/elox"
ELOX_VM_PATH = ROOT_DIR + "/target/release/vm"

OUTPUT_EXPECT = re.compile(r'// ?!expect: ?(.*)')
RUNTIME_ERROR_EXPECT = re.compile(r'// ?!expect runtime error: ?(.*)')
SYNTAX_ERROR_EXPECT = re.compile(r'// ?!expect syntax error: ?(.*)')
ERROR_LINE = re.compile(r'Error: \[line ([0-9]+):([0-9]+)\]: ?(.+)')
HEADER = re.compile(r'// ?#\[(\!?)(\w+)\s*\]')

elox_path = ELOX_TREEWALK_PATH

expectations = 0


class EloxTest:
    def __init__(self, path, target):
        self.path = path
        self.target = target
        self.output = []
        self.failures = []
        self.expectations = 0
        self.runtime_err = None
        self.syntax_err = None

    def parse(self):  # returns True if the test should be run
        line_nb = 1
        with open(self.path, 'r') as file:
            for line in file:
                if line_nb == 1:
                    header = HEADER.search(line)
                    if header:
                        negate = header.group(1) == '!'
                        target = header.group(2)
                        if target == self.target:
                            if negate:
                                return False

                expect = OUTPUT_EXPECT.search(line)
                if expect:
                    self.output.append((expect.group(1), line_nb))
                    self.expectations += 1

                runtime_err = RUNTIME_ERROR_EXPECT.search(line)
                if runtime_err:
                    if self.runtime_err:
                        self.fail(
                            "Test error: Cannot expect multiple runtime errors")
                        return False

                    self.runtime_err = (runtime_err.group(1), line_nb)
                    self.expectations += 1

                syntax_err = SYNTAX_ERROR_EXPECT.search(line)
                if syntax_err:
                    if self.syntax_err:
                        self.fail(
                            "Test error: Cannot expect multiple syntax errors")
                        return False
                    self.syntax_err = (syntax_err.group(1), line_nb)
                    self.expectations += 1
            line_nb += 1
        return True

    def run(self):
        args = [elox_path, self.path]
        proc = Popen(args, stdin=PIPE, stdout=PIPE, stderr=PIPE)
        out, err = proc.communicate()
        self.validate(out, err)

    def validate(self, out, err):

        if self.syntax_err and self.runtime_err:
            self.fail(
                "Test error: Cannot expect both syntax and runtime errors.")
            return

        out = out.decode("utf-8").replace('\r\n', '\n')
        err = err.decode("utf-8").replace('\r\n', '\n')

        err_lines = err.split('\n')

        if self.syntax_err != None:
            self.validate_syntax_error(err_lines)

        if self.runtime_err != None:
            self.validate_runtime_error(err_lines)

        if len(self.failures) == 0:
            self.validate_output(out)

    def validate_syntax_error(self, errors):
        if len(errors) < 2:
            self.fail('Expected syntax error "{0}" and got none.',
                      self.syntax_err[0])
            return

        match = ERROR_LINE.search(errors[0])

        if match.group(3) != self.syntax_err[0]:
            self.fail(
                'Expected runtime error "{0}" and got: "{1}"', self.syntax_err[0], match.group(3))

    def validate_runtime_error(self, errors):
        if len(errors) < 2:
            self.fail('Expected runtime error "{0}" and got none.',
                      self.runtime_err[0])
            return

        match = ERROR_LINE.search(errors[0])

        if match.group(3) != self.runtime_err[0]:
            self.fail(
                'Expected runtime error "{0}" and got: "{1}"', self.runtime_err[0], match.group(3))

    def validate_output(self, out):
        # Remove the trailing last empty line.
        out_lines = out.split('\n')
        if out_lines[-1] == '':
            del out_lines[-1]

        index = 0
        for line in out_lines:
            if sys.version_info < (3, 0):
                line = line.encode('utf-8')

            if index >= len(self.output):
                self.fail('Got output "{0}" when none was expected.', line)
            elif self.output[index][0] != line:
                self.fail('Expected output "{0}" on line {1}, got "{2}".',
                          self.output[index][0], self.output[index][1], line)
            index += 1

        while index < len(self.output):
            self.fail('Missing expected output "{0}" on line {1}.',
                      self.output[index][0], self.output[index][1])
            index += 1

    def fail(self, message, *args):
        if args:
            message = message.format(*args)
        self.failures.append(message)


def run_test(path, target):  # returns True if this test was run
    global expectations
    test = EloxTest(path, target)
    if test.parse():
        test.run()
        if len(test.failures) != 0:
            print(relpath(path).replace("\\", "/") + ' failed: ')
            for failure in test.failures:
                print(failure)
                sys.exit(1)
        else:
            expectations += test.expectations
            return True

    return False


def run_tests(dir, target):
    global expectations
    prev_expectations = expectations
    expectations = 0
    skipped = 0

    for path in listdir(dir):
        sub_path = join(dir, path)
        if isdir(sub_path):
            skipped += run_tests(sub_path, target)
        else:
            skipped += int(not run_test(sub_path, target))

    print('All tests passed in ' + relpath(dir) +
          ', expectations: ' + str(expectations) + ", skipped: " + str(skipped))

    expectations += prev_expectations
    return skipped


def parse_args():
    global elox_path
    args = sys.argv
    target = "tw"
    if len(args) > 1:
        target = args[1]
    if target == "vm":
        elox_path = ELOX_VM_PATH

    run_tests(TESTS_DIR, target)


parse_args()
